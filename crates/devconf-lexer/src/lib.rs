pub mod source;
pub mod token;
pub mod utils;

use source::LexerResult;
use std::collections::VecDeque;
use token::SpannedToken;
use winnow::{
    Parser,
    combinator::{alt, delimited, peek},
    error::{AddContext, ParserError},
    stream::{AsChar, Stream},
    token::{any, take_while},
};

use crate::{
    source::{LexerError, SourceLexer, SourceLexerExt},
    token::{IntoSpan, Literal, Punctuation, Token},
    utils::eat_spaces,
};
pub struct Lexer;

impl Lexer {
    #[allow(clippy::should_implement_trait)]
    /// # Errors
    /// It return an error based on the `winnow` parse.
    pub fn from_str(input: &str) -> LexerResult<'_, VecDeque<SpannedToken>> {
        let mut input = SourceLexer::new(input);
        let mut tokens = VecDeque::<token::SpannedToken>::new();

        while Self::next_token(&mut tokens, &mut input)? {}

        Ok(tokens)
    }

    #[allow(clippy::too_many_lines)]
    fn next_token<'a>(
        tokens: &mut VecDeque<SpannedToken>,
        input: &mut SourceLexer<'a>,
    ) -> LexerResult<'a, bool> {
        let Ok(parsed_char) = peek(any::<_, ()>).parse_next(input) else {
            return Ok(false);
        };

        if parsed_char == '#' {
            // We consume the '#'
            _ = any::<_, ()>(input);
            let (comment, span) = take_while(0.., |c| c != '\n')
                .with_span()
                .map(IntoSpan::into_span)
                .parse_next(input)?;
            tokens.push_back(SpannedToken {
                span,
                token: Token::Comment(comment.trim().to_owned()),
            });
            return Ok(true);
        }

        if parsed_char.is_alpha() {
            Self::token_ident(tokens, input)?;
            return Ok(true);
        }

        if parsed_char.is_numeric() || parsed_char == '-' {
            Self::token_number(tokens, input)?;
            return Ok(true);
        }

        if parsed_char == '\'' || parsed_char == '"' {
            Self::token_string(tokens, input)?;
            return Ok(true);
        }

        if parsed_char == '@' {
            let (token, span) = alt((
                "@if".value(kw!(If)),
                "@else".value(kw!(Else)),
                "@use".value(kw!(Use)),
                "@template".value(kw!(Template)),
            ))
            .with_span()
            .map(IntoSpan::into_span)
            .parse_next(input)?;

            tokens.push_back(SpannedToken { span, token });
            eat_spaces(input)?;
            return Ok(true);
        }

        if parsed_char == '.'
            && let Some(next) = input.peek_slice(2).chars().last()
            && next.is_numeric()
        {
            // because of dot assignation, maybe we found stuff like foo.42, who is invalid
            // so we need to check before numeric parsing, if before we found an identifier. to just set it as a integer.
            _ = tokens
                .iter()
                .rev()
                .take(2)
                .any(|token| matches!(token.token, Token::Ident(_)))
                .then(|| {
                    // we just consume the identifier so the next token is parsed as a integer instead of a float.
                    // anyway, on the `ast` we catch this to return it as invalid, but this is just a workaround because
                    // the behaviour of parsing a float just because we saw a dot before.
                    let (_, span) = any::<_, ()>
                        .with_span()
                        .map(IntoSpan::into_span)
                        .parse_next(input)
                        .unwrap_or_else(|()| {
                            input.error(format!("unexpected char: {parsed_char:#?}"))
                        });
                    tokens.push_back(SpannedToken {
                        span,
                        token: token::Token::Punctuation(Punctuation::Dot), // we not that is a dot because of `parsed_char``
                    });
                });

            Self::token_number(tokens, input)?;
            return Ok(true);
        }

        if parsed_char == '\r' {
            _ = any::<_, ()>(input);
            return Ok(true);
        }

        let (token, span) = alt([
            ".".value(Punctuation::Dot),
            ",".value(Punctuation::Comma),
            ";".value(Punctuation::Semicolon),
            ":".value(Punctuation::Colon),
            "$".value(Punctuation::Dollar),
            "&&".value(Punctuation::And),
            "||".value(Punctuation::Or),
            "!=".value(Punctuation::Ne),
            "!".value(Punctuation::Bang),
            "?".value(Punctuation::Question),
            "==".value(Punctuation::Eq),
            "=".value(Punctuation::Equals),
            "{".value(Punctuation::OpenBrace),
            "}".value(Punctuation::CloseBrace),
            "[".value(Punctuation::OpenSquareBracket),
            "]".value(Punctuation::CloseSquareBracket),
            "(".value(Punctuation::OpenParen),
            ")".value(Punctuation::CloseParen),
            "\n".value(Punctuation::Newline),
            "   ".value(Punctuation::Identation),
            "\t".value(Punctuation::Identation),
        ])
        .with_span()
        .map(IntoSpan::into_span)
        .parse_next(input)
        .unwrap_or_else(|()| input.error(format!("unexpected char: {parsed_char:#?}")));

        if token == Punctuation::Dot
            && let Some(next) = peek(any::<_, ()>).parse_next(input).ok()
            && next.is_numeric()
        {
            Self::token_number(tokens, input)?;
            return Ok(true);
        }

        if token != Punctuation::Newline && token != Punctuation::Identation {
            eat_spaces(input)?;
        }

        tokens.push_back(SpannedToken {
            span,
            token: token::Token::Punctuation(token),
        });

        Ok(true)
    }

    fn token_ident<'i>(
        tokens: &mut VecDeque<SpannedToken>,
        input: &mut SourceLexer<'i>,
    ) -> LexerResult<'i> {
        let (ident, span) = take_while(1.., |c: char| c.is_alphanumeric() || c == '_')
            .with_span()
            .map(IntoSpan::into_span)
            .parse_next(input)?;

        let token = match ident {
            "null" => lit!(Literal::Null),
            "true" => lit!(true),
            "false" => lit!(false),

            _ => ident!(@expr ident.to_owned()),
        };

        tokens.push_back(SpannedToken { span, token });

        eat_spaces(input)
    }

    fn token_number<'i>(
        tokens: &mut VecDeque<SpannedToken>,
        input: &mut SourceLexer<'i>,
    ) -> LexerResult<'i> {
        _ = eat_spaces(input);

        let (num, span) = take_while(1.., |c: char| c.is_ascii_digit() || c == '_' || c == '.')
            .with_span()
            .map(IntoSpan::into_span)
            .parse_next(input)?;

        let clean = Self::sanitize_number(num, input)?;
        if clean.contains('.') {
            tokens.push_back(SpannedToken {
                span,
                token: Token::Literal(Literal::Float(clean.parse().unwrap())),
            });
        } else {
            tokens.push_back(SpannedToken {
                span,
                token: Token::Literal(Literal::Integer(clean.parse().unwrap())),
            });
        }

        eat_spaces(input)
    }

    fn sanitize_number<'s>(raw: &str, input: &mut SourceLexer<'s>) -> LexerResult<'s, String> {
        if raw.contains("__") {
            return Err(LexerError::from_input(input).add_context(
                input,
                &input.checkpoint(),
                "invalid double underscore",
            ));
        }

        if raw.is_empty() || raw.starts_with('_') || raw.ends_with('_') {
            return Err(LexerError::from_input(input).add_context(
                input,
                &input.checkpoint(),
                "numbers cannot start with _ or ends with _",
            ));
        }

        if raw.contains("._") || raw.contains("_.") {
            return Err(LexerError::from_input(input).add_context(
                input,
                &input.checkpoint(),
                "numbers cannot be formmatted with _. or ._",
            ));
        }

        let parts = raw.split('.').collect::<Vec<_>>();

        if parts.len() > 2 {
            return Err(LexerError::from_input(input).add_context(
                input,
                &input.checkpoint(),
                "numbers cannot have more than one decimal point",
            ));
        }

        if !Self::is_valid_integer_part(parts[0]) {
            return Err(LexerError::from_input(input).add_context(
                input,
                &input.checkpoint(),
                "the integer is invalid",
            ));
        }

        // Si hay parte decimal, validarla (también puede tener underscores)
        if parts.len() == 2 {
            // Permitir parte decimal vacía (ej: "5." es válido para forzar float)
            if !parts[1].is_empty() {
                // Solo validar si hay dígitos después del punto
                if !Self::is_valid_decimal_part(parts[1]) {
                    return Err(LexerError::from_input(input).add_context(
                        input,
                        &input.checkpoint(),
                        "invalid decimal part",
                    ));
                }
            }
        }

        Ok(raw.replace('_', ""))
    }

    fn is_valid_decimal_part(part: &str) -> bool {
        if part.is_empty() || part.starts_with('_') || part.ends_with('_') {
            return false;
        }
        let segments: Vec<&str> = part.split('_').collect();
        for segment in &segments {
            if segment.is_empty() || !segment.chars().all(|c| c.is_ascii_digit()) {
                return false;
            }
        }
        true
    }

    fn is_valid_integer_part(part: &str) -> bool {
        if part.is_empty() {
            return true;
        }

        if part.starts_with('_') || part.ends_with('_') {
            return false;
        }

        let segments: Vec<&str> = part.split('_').collect();

        // Primer segmento puede tener 1-3 dígitos
        if !segments[0].chars().all(|c| c.is_ascii_digit()) || segments[0].is_empty() {
            return false;
        }

        // Resto de segmentos deben tener exactamente 3 dígitos
        for segment in &segments[1..] {
            if segment.len() != 3 || !segment.chars().all(|c| c.is_ascii_digit()) {
                return false;
            }
        }

        true
    }

    fn token_string<'i>(
        tokens: &mut VecDeque<SpannedToken>,
        input: &mut SourceLexer<'i>,
    ) -> LexerResult<'i> {
        let (str, span) = alt((
            delimited('"', take_while(0.., |c| c != '"'), '"'),
            delimited('\'', take_while(0.., |c| c != '\''), '\''),
        ))
        .with_span()
        .map(IntoSpan::into_span)
        .parse_next(input)?;

        tokens.push_back(SpannedToken {
            span,
            token: Token::Literal(Literal::String(str.to_owned())),
        });

        eat_spaces(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_unquoted_string() {
        let input = "hello";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(Vec::from(tokens), &[ident!("hello")]);
    }

    #[test]
    fn test_tokenize_quoted_string() {
        let input = "\"hello world\"";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(Vec::from(tokens), &[lit!("hello world")]);
    }

    #[test]
    fn test_invalid_quoted_string() {
        let input = "'hello";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_err());
    }

    #[test]
    fn test_tokenize_valid_integer() {
        let input = "42";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(Vec::from(tokens), &[lit!(42)]);
    }

    #[test]
    fn test_tokenize_valid_float() {
        let input = "2.7123";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(Vec::from(tokens), &[lit!(2.7123)]);
    }

    #[test]
    fn test_tokenize_invalid_integer() {
        let input = "4_";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_err());
    }

    #[test]
    fn test_tokenize_invalid_float() {
        let input = "3._";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_err());
    }

    #[test]
    fn test_tokenize_formatted_integers() {
        let input = "1_000\n2_000_000\n1_234";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[
                lit!(1_000),
                T!(Newline),
                lit!(2_000_000),
                T!(Newline),
                lit!(1_234)
            ]
        );
    }

    #[test]
    fn test_tokenize_formatted_floats() {
        let input = "2.7_142\n42.45_434\n.5";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[
                lit!(2.7142),
                T!(Newline),
                lit!(42.45434),
                T!(Newline),
                lit!(0.5)
            ]
        );
    }

    #[test]
    fn test_tokenize_booleans() {
        let input = "true\nfalse";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(Vec::from(tokens), &[lit!(true), T!(Newline), lit!(false)]);
    }

    #[test]
    fn test_tokenize_comments() {
        let input = "# this is a comment\n42";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[cmt!("this is a comment".to_owned()), T!(Newline), lit!(42)]
        );
    }

    #[test]
    fn test_tokenize_inline_comments() {
        let input = "foo: bar # this is a comment bro";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[
                ident!("foo"),
                T!(Colon),
                ident!("bar"),
                cmt!("this is a comment bro".to_owned())
            ]
        );
    }

    #[test]
    fn test_tokenize_array() {
        let input = "[1, 2, 3]";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[
                T!(OpenSquareBracket),
                lit!(1),
                T!(Comma),
                lit!(2),
                T!(Comma),
                lit!(3),
                T!(CloseSquareBracket)
            ]
        );
    }

    #[test]
    fn test_tokenize_object() {
        let input = "app:\n{ foo: 42\nbar: \"hello world\" }";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[
                ident!("app"),
                T!(Colon),
                T!(Newline),
                T!(OpenBrace),
                ident!("foo"),
                T!(Colon),
                lit!(42),
                T!(Newline),
                ident!("bar"),
                T!(Colon),
                lit!("hello world"),
                T!(CloseBrace)
            ]
        );
    }

    #[test]
    fn test_tokenize_expression_eval() {
        let input = "${FOO}";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[T!(Dollar), T!(OpenBrace), ident!("FOO"), T!(CloseBrace)]
        );
    }

    #[test]
    fn test_invalid_nested_dot_assignation() {
        let input = "hello..world: true";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[
                ident!("hello"),
                T![Dot],
                T![Dot],
                ident!("world"),
                T![Colon],
                lit!(true)
            ]
        );
    }

    #[test]
    fn test_invalid_nested_dots_with_integers() {
        let input = "hello.42: true";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[ident!("hello"), T![Dot], lit!(42), T![Colon], lit!(true)]
        );
    }

    #[test]
    fn test_invalid_nested_dot_assignation_with_numbers() {
        let input = "hello..42: true";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[
                ident!("hello"),
                T![Dot],
                T![Dot],
                lit!(42),
                T![Colon],
                lit!(true)
            ]
        );
    }

    #[test]
    fn test_tokenize_keywords() {
        let input = "@if true { 42 } @else { 21 }";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[
                kw!(If),
                lit!(true),
                T!(OpenBrace),
                lit!(42),
                T!(CloseBrace),
                kw!(Else),
                T!(OpenBrace),
                lit!(21),
                T!(CloseBrace)
            ]
        );
    }

    #[test]
    fn test_tokenize_template() {
        let input = "@template service(name, port, foo=null):\n\tapp.${name}.port = ${port}\n\t@if foo != null:\n\t\tapp.${name}.foo = ${foo}";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[
                kw!(Template),
                ident!("service"),
                T!(OpenParen),
                ident!("name"),
                T!(Comma),
                ident!("port"),
                T!(Comma),
                ident!("foo"),
                T!(Equals),
                lit!(Null),
                T!(CloseParen),
                T!(Colon),
                T!(Newline),
                T!(Identation),
                ident!("app"),
                T!(Dot),
                T!(Dollar),
                T!(OpenBrace),
                ident!("name"),
                T!(CloseBrace),
                T!(Dot),
                ident!("port"),
                T!(Equals),
                T!(Dollar),
                T!(OpenBrace),
                ident!("port"),
                T!(CloseBrace),
                T!(Newline),
                T!(Identation),
                kw!(If),
                ident!("foo"),
                T!(Ne),
                lit!(Null),
                T!(Colon),
                T!(Newline),
                T!(Identation),
                T!(Identation),
                ident!("app"),
                T!(Dot),
                T!(Dollar),
                T!(OpenBrace),
                ident!("name"),
                T!(CloseBrace),
                T!(Dot),
                ident!("foo"),
                T!(Equals),
                T!(Dollar),
                T!(OpenBrace),
                ident!("foo"),
                T!(CloseBrace)
            ]
        );
    }

    #[test]
    fn test_tokenize_comparisons() {
        let input = "a == b\na != b\na && b\na || b";
        let lexer = Lexer::from_str(input);
        assert!(lexer.is_ok());
        let tokens = lexer.unwrap();
        assert_eq!(
            Vec::from(tokens),
            &[
                ident!("a"),
                T!(Eq),
                ident!("b"),
                T!(Newline),
                ident!("a"),
                T!(Ne),
                ident!("b"),
                T!(Newline),
                ident!("a"),
                T!(And),
                ident!("b"),
                T!(Newline),
                ident!("a"),
                T!(Or),
                ident!("b")
            ]
        );
    }
}
