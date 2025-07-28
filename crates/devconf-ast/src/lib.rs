pub mod nodes;
pub mod source;
#[cfg(test)]
pub mod tests;
pub mod utils;

use crate::source::Ctx;
use std::collections::{HashMap, VecDeque};

use crate::utils::expand_expr;
use devconf_lexer::lit;
use devconf_lexer::token::{Literal, Punctuation, SpannedToken};
use devconf_lexer::{T, kw, token::Token};

use devconf_tychecker::Context;

use crate::nodes::{AstStatement, PathSegment};
use crate::source::SourceAst;
use devconf_nodes::ast::{AstBinaryOp, AstExpr, AstUnaryOp};

pub use nodes::AstScope;

impl AstScope {
    #[must_use]
    pub fn from_tokens(base: &str, tokens: VecDeque<SpannedToken>) -> Self {
        SourceAst::new(base, tokens).parse_scope(0)
    }
}

impl SourceAst<'_> {
    pub(crate) fn parse_scope(&mut self, ident: usize) -> AstScope {
        let mut nodes = vec![];
        loop {
            if !self.parse_pre_statement(ident) {
                break;
            }
            let mut stmt = self.parse_statement(ident);
            if let AstStatement::Comment = stmt {
                continue;
            }
            match stmt {
                ref s @ AstStatement::TemplateDefinition { ref name, .. } => {
                    self.macros.insert(name.clone(), s.clone().into());
                    nodes.push(s.clone());
                }
                AstStatement::TemplateCalling { name, ref mut args } => {
                    // Limit the immutable borrow scope
                    let (params, params_len, defaults, body) = match self.macros.get(&name) {
                        Some(m) => (
                            m.params.clone(),
                            m.params.len(),
                            m.defaults.clone(),
                            m.body.clone(),
                        ),
                        None => self.error_in_place("Macro not found"),
                    };
                    let args_len = args.len();
                    match params_len.cmp(&args_len) {
                        std::cmp::Ordering::Greater => {
                            if defaults.len() + args_len == params_len {
                                args.extend(defaults.iter().map(|d| d.1.clone()));
                            } else {
                                self.error_in_place("Too many arguments")
                            }
                        }
                        std::cmp::Ordering::Less => self.error_in_place("Too few arguments"),
                        std::cmp::Ordering::Equal => {}
                    }
                    let args = params.iter().zip(args).collect::<Vec<_>>();
                    let expanded_nodes = self.expand_template(&body, args);
                    for node in expanded_nodes {
                        nodes.push(node);
                    }
                }
                s => nodes.push(s),
            }
        }
        AstScope(nodes)
    }

    fn expand_template(
        &mut self,
        scope: &AstScope,
        args: Vec<(&String, &mut AstExpr)>,
    ) -> Vec<AstStatement> {
        // Create a substitution map for easy lookup
        let substitutions: HashMap<String, AstExpr> = args
            .into_iter()
            .map(|(param, arg)| (param.clone(), arg.clone()))
            .collect();

        // Expand each statement in the template body
        scope
            .0
            .iter()
            .map(|stmt| self.expand_statement(stmt, &substitutions))
            .collect()
    }

    fn expand_statement(
        &self,
        stmt: &AstStatement,
        substitutions: &HashMap<String, AstExpr>,
    ) -> AstStatement {
        match stmt {
            AstStatement::Comment => AstStatement::Comment,
            AstStatement::Expression(expr) => {
                AstStatement::Expression(Box::new(expand_expr(expr, substitutions)))
            }
            AstStatement::Conditional {
                test,
                body,
                otherwise,
            } => AstStatement::Conditional {
                test: Box::new(expand_expr(test, substitutions)),
                body: self.expand_scope(body, substitutions),
                otherwise: otherwise
                    .as_ref()
                    .map(|scope| self.expand_scope(scope, substitutions)),
            },
            AstStatement::Assignation { path, value } => AstStatement::Assignation {
                path: path
                    .iter()
                    .map(|segment| self.expand_path_segment(segment, substitutions))
                    .collect(),
                value: Box::new(expand_expr(value, substitutions)),
            },
            AstStatement::TemplateCalling { name, args } => {
                // Template calls inside templates should have their arguments expanded
                AstStatement::TemplateCalling {
                    name: name.clone(),
                    args: args
                        .iter()
                        .map(|arg| expand_expr(arg, substitutions))
                        .collect(),
                }
            }
            AstStatement::TemplateDefinition { .. } => {
                self.error_in_place("Definition of macros inside macros isn't available")
            }
        }
    }

    fn expand_scope(&self, scope: &AstScope, substitutions: &HashMap<String, AstExpr>) -> AstScope {
        AstScope(
            scope
                .0
                .iter()
                .map(|stmt| self.expand_statement(stmt, substitutions))
                .collect(),
        )
    }

    fn expand_path_segment(
        &self,
        segment: &PathSegment,
        substitutions: &HashMap<String, AstExpr>,
    ) -> PathSegment {
        match segment {
            PathSegment::Static(s) => PathSegment::Static(s.clone()),
            PathSegment::Dynamic(expr) => {
                let expr = expand_expr(expr, substitutions);
                if let Err(e) = self.checker.check_expr(&expr, Context::Expression) {
                    self.error_in_place(e);
                }
                match expr {
                    AstExpr::Ident(l)
                    | AstExpr::Literal(Literal::UnquotedString(l) | Literal::String(l)) => {
                        PathSegment::Static(l)
                    }
                    _ => self.error_in_place("invalid expression"),
                }
            }
        }
    }

    fn parse_pre_statement(&mut self, level: usize) -> bool {
        let Some(first) = self.peek() else {
            return false;
        };

        if *first != T![Newline] {
            first.recover();
            return true;
        }

        loop {
            if level != 0 {
                match self.eat_ident(level) {
                    Some(false) => {}
                    Some(true) => continue,
                    None => return false,
                }
            }
            let Some(first) = self.peek() else {
                return false;
            };
            if *first != T![Newline] {
                first.recover();
                return true;
            }
        }
    }

    fn eat_ident(&mut self, level: usize) -> Option<bool> {
        for _ in 0..level {
            let token = self.peek()?;

            match *token.token {
                T![Identation] => {} // the same as continue but clippy::pedantic relies.
                T![Newline] => return Some(true),
                _ => {
                    token.recover();
                    return None;
                }
            }
        }

        Some(false)
    }

    fn peek_stmt<T>(
        &mut self,
        level: usize,
        callback: impl Fn(&mut SourceAst<'_>, SpannedToken) -> Option<T>,
    ) -> Option<T> {
        let mut peek = self.clone();

        peek.parse_pre_statement(level)
            .then(|| peek.tokens.pop_front())
            .flatten()
            .and_then(|t| callback(&mut peek, t))
            .inspect(|_| *self = peek)
    }

    fn parse_statement(&mut self, level: usize) -> AstStatement {
        let mut checkpoint = self.clone();
        let first = self.peek_expect();

        // println!("parse_statement: token = {first:#?}");

        match **first {
            Token::Comment(_) => AstStatement::Comment,
            Token::Ident(_) | T![Dollar] => {
                let mut nodes = vec![];
                first.recover();
                let mut seen_dot = false;
                while let Some(token) = checkpoint.peek() {
                    let span = token.span;
                    let node = match (**token).clone() {
                        T![Colon] => break,
                        Token::Literal(literal) => {
                            if !matches!(literal, Literal::String(_) | Literal::UnquotedString(_)) {
                                self.error_at(
                                    span,
                                    format!("Seems to be an invalid token ({literal:?})"),
                                );
                            }
                            seen_dot = false;
                            AstExpr::Literal(literal.clone())
                        }
                        Token::Ident(id) => {
                            seen_dot = false;
                            AstExpr::Ident(id.clone())
                        }
                        T![Dollar] => {
                            token.recover();
                            let expr = checkpoint.parse_interpolation();
                            let ty = self.checker.check_expr(&expr, Context::Expression);
                            if let Err(e) = ty {
                                self.error_in_place(e);
                            }
                            seen_dot = false;
                            expr
                        }
                        T![Dot] if seen_dot => {
                            self.error_at(span, "You can't have consecutive commas");
                        }
                        T![Dot] => {
                            seen_dot = true;
                            continue;
                        }
                        expr => panic!("Unexpected expression: {expr:#?}"),
                    };

                    let node = match node {
                        AstExpr::Ident(name) => PathSegment::Static(name.clone()),
                        AstExpr::Literal(Literal::UnquotedString(name) | Literal::String(name)) => {
                            PathSegment::Static(name.to_string())
                        }
                        AstExpr::Interpolation { expr } => PathSegment::Dynamic(
                            AstExpr::Interpolation { expr: expr.clone() }.into(),
                        ),
                        _ => panic!(),
                    };

                    nodes.push(node);
                }

                let value = match checkpoint.parse_expr() {
                    AstExpr::Ident(id) => AstExpr::Literal(Literal::UnquotedString(id)),
                    e => e,
                };

                *self = checkpoint;

                AstStatement::Assignation {
                    path: nodes,
                    value: value.into(),
                }
            }
            kw!(Template) => self.parse_template_definition(level),
            kw!(Use) => self.parse_template_call(),
            kw!(If) => self.parse_if_stmt(level),
            T![Bang] | Token::Literal(_) => {
                AstStatement::Expression(checkpoint.parse_expr().into())
            }
            _ => {
                let first = first.accept();
                checkpoint.error_at(
                    first.span,
                    format!("Unexpected token ({:?}) or unimplemented yet.", first.token),
                );
            }
        }
    }

    fn parse_template_call(&mut self) -> AstStatement {
        // Get the template name
        let template_name = self
            .expect_match("Missing name for the template", |f| {
                matches!(*f, Token::Ident(_)).then_some(f)
            })
            .into_ident()
            .expect("checked on the expect_match");

        // Expect opening parenthesis
        self.expect_token(&T![OpenParen]);

        let mut args = Vec::<AstExpr>::new();
        let mut expecting_comma = false;

        // Parse arguments until we hit the closing parenthesis
        while let Some(next) = self.peek() {
            match **next {
                T![CloseParen] => {
                    _ = next.accept(); // consume the closing paren
                    break;
                }
                T![Comma] if expecting_comma => {
                    _ = next.accept(); // consume the comma
                    expecting_comma = false;

                    // Check for trailing comma
                    if let Some(peek) = self.peek() {
                        if matches!(**peek, T![CloseParen]) {
                            let span = peek.span;
                            self.error_at(span, "Trailing comma before closing parenthesis");
                        } else {
                            peek.recover(); // put it back
                        }
                    }
                }
                T![Comma] => {
                    let span = next.span;
                    self.error_at(span, "Unexpected comma - expected expression");
                }
                _ if !expecting_comma => {
                    next.recover(); // put the token back since parse_expr will need it
                    // Parse an expression - this is where we fix the type issue
                    let expr = self.parse_expr();
                    args.push(expr);
                    expecting_comma = true;
                }
                _ => {
                    let span = next.span;
                    self.error_at(span, "Expected ',' or ')' after argument");
                }
            }
        }

        AstStatement::TemplateCalling {
            name: template_name,
            args,
        }
    }

    fn parse_type_hint(&mut self) -> Option<String> {
        let mut checkpoint = self.clone();
        // println!("parse type hint : {self:#?}");
        if let Some(token) = checkpoint.peek() {
            // dbg!(&token.token);
            if matches!(**token, T![Colon]) {
                let colon_span = token.span;
                match checkpoint.peek() {
                    Some(next_token) if matches!(**next_token, Token::Ident(_)) => {
                        if let Token::Ident(ref type_name) = **next_token {
                            let type_name = type_name.clone();
                            // dbg!(&type_name);
                            *self = checkpoint;
                            return Some(type_name.clone());
                        }
                        next_token.recover();
                    }
                    Some(next_token) if matches!(**next_token, T![CloseBrace]) => {
                        self.error_at(colon_span, "Received :, so there was expected a type hint, but instead no one was provided\n if you don't want to add a type hint, just close the interpolation".to_owned());
                    }
                    None => {
                        self.error_at(colon_span, "Received :, so there was expected a type hint, but instead no one was provided\n if you don't want to add a type hint, just close the interpolation".to_owned());
                    }
                    Some(next_token) => {
                        self.error_at(next_token.span, format!("Received :, so there was expected a type hint, but instead found: '{}'\n if you don't want to add a type hint, just close the interpolation", **next_token));
                    }
                }
            } else {
                token.recover();
            }
        }

        *self = checkpoint;
        None
    }

    fn parse_template_definition(&mut self, level: usize) -> AstStatement {
        // This is the entry point of the template definition.
        // We should parse first the arguments.
        // The AST could be Ident(name) -> OpenParen -> (Ident(_),*) -> CloseParen -> Colon
        let mut checkpoint = self.clone();
        let template_name = checkpoint
            .expect_match("Missing name for the template", |f| {
                matches!(*f, Token::Ident(_)).then_some(f)
            })
            .into_ident()
            .expect("checked on the expect_match");

        checkpoint.expect_token(&T![OpenParen]);
        // println!("here!");
        let mut params = vec![];
        let mut defaults = vec![];
        let mut seen_comma = false;
        let mut balanced_parens = false;
        // First: the arguments with the optional defaults.
        while let Some(next) = checkpoint.peek() {
            match **next {
                // the identifier
                Token::Ident(ref arg) => {
                    let arg_clone = arg.clone();
                    params.push(arg_clone);
                    seen_comma = false;
                }
                // the comma for separating terms
                T![Comma] if !seen_comma => {
                    seen_comma = true;
                }
                T![Comma] => {
                    let span = next.span;
                    self.error_at(span, "You can't have consecutive commas");
                }
                // for defaults
                T![Equals] => {
                    let default = checkpoint.parse_default_expr();
                    let arg = params.last().expect("Parsed above.").clone(); // clone here to avoid borrow issues
                    defaults.push((arg, default));
                }
                T![CloseParen] if !balanced_parens => {
                    if seen_comma {
                        let span = next.span;
                        self.error_at(span, "trailling comma");
                    }
                    balanced_parens = true;
                }
                T![CloseParen] => {
                    let span = next.span;
                    self.error_at(span, "Unbalanced parenthesis");
                }
                T![Colon] => break,
                _ => {
                    unimplemented!()
                }
            }
        }

        let body = checkpoint.parse_template_body(level);

        if body.0.is_empty() {
            self.error_in_place("Empty body!");
        }

        *self = checkpoint;
        // println!("good bye!");
        AstStatement::TemplateDefinition {
            name: template_name,
            params,
            defaults,
            body,
        }
    }

    fn parse_template_body(&mut self, level: usize) -> AstScope {
        self.parse_scope(level + 1)
    }

    fn parse_default_expr(&mut self) -> AstExpr {
        let token = self.peek_expect();
        match **token {
            Token::Ident(ref name) => {
                let name = name.clone();
                AstExpr::Ident(name)
            }
            Token::Literal(ref lit) => {
                let lit = lit.clone();
                AstExpr::Literal(lit)
            }
            lit!(Null) => AstExpr::Literal(Literal::Null),
            _ => {
                let token = token.accept();
                self.error_at(token.span, format!("Unexpected token: {:?}", token.token));
            }
        }
    }

    fn parse_if_stmt(&mut self, level: usize) -> AstStatement {
        let test = self.parse_expr().into();
        self.expect_token(&T![Colon]);
        let body = self.parse_scope(level + 1);
        // self.expect_token(&T![CloseBrace]);
        let otherwise = self.peek_stmt(level, |source, keyword| match keyword.token {
            kw!(Else) => {
                source.expect_token(&T![Colon]);

                Some(source.parse_scope(level + 1))
            }
            _ => None,
        });
        // self.expect_token(&T![CloseBrace]);
        AstStatement::Conditional {
            test,
            body,
            otherwise,
        }
    }

    fn precedence_of(token: &Token) -> Option<(u8, AstBinaryOp)> {
        match token {
            T![Or] => Some((1, AstBinaryOp::Or)),
            T![And] => Some((2, AstBinaryOp::And)),
            T![Eq] => Some((3, AstBinaryOp::Eq)),
            T![Ne] => Some((4, AstBinaryOp::Ne)),
            _ => None,
        }
    }

    fn parse_with_precedence(&mut self, min: u8, mut left: AstExpr) -> AstExpr {
        while let Some(expr) = self.peek() {
            // println!("parse precedence {:#?}", expr.token);
            if let Some((prec, op)) = Self::precedence_of(&(expr).clone()) {
                if prec < min {
                    expr.recover();
                    break;
                }
                let right = self.parse_unary();
                let right = self.parse_with_precedence(prec + 1, right);
                left = AstExpr::BinaryExpr {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                }
            } else {
                expr.recover();
                break;
            }
        }
        left
    }

    // Main expression parser with precedence
    fn parse_expr(&mut self) -> AstExpr {
        let left = self.parse_unary();
        self.parse_with_precedence(0, left)
    }

    fn parse_unary(&mut self) -> AstExpr {
        if let Some(token) = self.peek() {
            match **token {
                T![Bang] => {
                    let expr = self.parse_unary();
                    return AstExpr::UnaryExpr {
                        op: AstUnaryOp::Not,
                        expr: Box::new(expr),
                    };
                }
                _ => {
                    token.recover();
                }
            }
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> AstExpr {
        let token = self.peek_expect();
        let expr = match **token {
            Token::Ident(ref name) => {
                let name = name.clone();
                AstExpr::Ident(name)
            }
            Token::Literal(ref lit) => {
                let lit = lit.clone();
                AstExpr::Literal(lit)
            }
            T![OpenSquareBracket] => {
                token.recover();
                self.parse_array()
            }
            T![OpenBrace] => {
                token.recover();
                self.parse_object()
            }
            T![Dollar] => {
                token.recover();
                self.parse_interpolation()
            }
            T![OpenParen] => {
                let expr = self.parse_expr();
                self.expect_token(&T![CloseParen]);
                expr
            }
            lit!(Null) => AstExpr::Literal(Literal::Null),
            _ => {
                let token = token.accept();
                self.error_at(token.span, format!("Unexpected token: {:?}", token.token));
            }
        };

        if let Ctx::Expression = self.actual_ctx
            && let Some(ty) = self.parse_type_hint()
        {
            AstExpr::Cast {
                expr: expr.into(),
                ty,
            }
        } else {
            expr
        }
    }

    fn parse_array(&mut self) -> AstExpr {
        self.expect_token(&T![OpenSquareBracket]);
        let mut elements = vec![];

        'parser: loop {
            if let Some(token) = self.peek() {
                // println!("Found next token: {token:#?}");
                if matches!(**token, T![CloseSquareBracket]) {
                    token.recover();
                    break 'parser;
                }
                token.recover();
            } else {
                break 'parser;
            }

            // Parse element
            // for some reason, this consumes the actual token so the `,` is not found.
            let expr = match self.parse_expr() {
                AstExpr::Ident(id) => AstExpr::Literal(Literal::UnquotedString(id)),
                ast => ast,
            };
            elements.push(expr);

            // Check what follows
            if let Some(token) = self.peek() {
                match **token {
                    T![Comma] => {
                        _ = token.accept();
                    }
                    T![CloseSquareBracket] => {
                        token.recover();
                        break 'parser;
                    }
                    _ => {
                        let unexpected = token.accept();
                        self.error_at(
                            unexpected.span,
                            format!("Expected ',' or ']', found {:?}", unexpected.token),
                        );
                    }
                }
            } else {
                break 'parser; // End of input
            }
        }

        self.expect_token(&T![CloseSquareBracket]);
        AstExpr::Array(elements)
    }

    fn parse_object(&mut self) -> AstExpr {
        self.expect_token(&T![OpenBrace]);
        let mut pairs = vec![];

        while let Some(token) = self.clone().peek() {
            if matches!(**token, T![CloseBrace]) {
                break;
            }

            let key = match **token {
                Token::Ident(ref name) => {
                    let name = name.clone();
                    self.tokens.pop_front();
                    name
                }
                Token::Punctuation(Punctuation::Newline | Punctuation::Identation) => {
                    self.tokens.pop_front();
                    continue;
                }
                _ => self.error_unexpected_token(&token.accept()),
            };

            self.expect_token(&T![Colon]);
            let value = match self.parse_expr() {
                AstExpr::Ident(id) => AstExpr::Literal(Literal::UnquotedString(id)),
                ast => ast,
            };

            pairs.push((key, value));

            if let Some(token) = self.clone().peek() {
                match **token {
                    Token::Punctuation(Punctuation::Comma | Punctuation::Newline) => {
                        self.tokens.pop_front();
                    }
                    T![CloseBrace] => break,
                    _ => self.error_unexpected_token(&token.accept()),
                }
            }
        }

        self.expect_token(&T![CloseBrace]);

        AstExpr::Object(pairs)
    }

    fn parse_interpolation(&mut self) -> AstExpr {
        self.actual_ctx = Ctx::Expression;
        // println!("in interpolation: {self:#?}");
        // let mut peek = self.clone();
        self.expect_token(&T![Dollar]);
        self.expect_token(&T![OpenBrace]);

        // Parse the base expression (identifier first)
        let mut expr = self.parse_primary_expr_for_interpolation();
        if let Some(ty) = self.parse_type_hint() {
            expr = AstExpr::Cast {
                expr: expr.into(),
                ty,
            };
        }

        // // println!("after expr pepe {expr:#?}");
        expr = self.parse_binary_continuation(expr);
        // // println!("got expr pepe: ${expr:#?}");

        self.expect_token(&T![CloseBrace]);
        self.actual_ctx = Ctx::Statement;

        AstExpr::Interpolation {
            expr: Box::new(expr),
        }
    }

    // Helper function to parse just the primary expression in interpolation context
    fn parse_primary_expr_for_interpolation(&mut self) -> AstExpr {
        let token = self.peek_expect();

        match **token {
            Token::Ident(ref name) => AstExpr::Ident(name.clone()),
            Token::Literal(ref lit) => AstExpr::Literal(lit.clone()),
            T![OpenParen] => {
                self.tokens.pop_front();
                let expr = self.parse_expr();
                self.expect_token(&T![CloseParen]);
                expr
            }
            _ => {
                let token = token.accept();
                self.error_at(
                    token.span,
                    format!("Unexpected token in interpolation: {:?}", token.token),
                );
            }
        }
    }

    // Helper function to continue parsing binary expressions
    fn parse_binary_continuation(&mut self, left: AstExpr) -> AstExpr {
        self.parse_with_precedence(0, left)
    }

    fn error_unexpected_token(&mut self, token: &SpannedToken) -> ! {
        self.error_at(token.span, format!("Unexpected token: {:?}", token.token));
    }
}
