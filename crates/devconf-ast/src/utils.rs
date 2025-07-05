#[macro_export]
macro_rules! scope {
    ($($expr:expr),*) => {
        $crate::nodes::AstScope(Vec::from([ $($expr),* ]))
    };
}

#[macro_export]
macro_rules! bin_op {
    ($a:expr, $op:ident, $b:expr) => {
        devconf_nodes::ast::AstExpr::BinaryOp {
            op: $crate::nodes::AstBinaryOp::$op,
            left: $a.into(),
            right: $b.into(),
        }
    };
}

#[macro_export]
macro_rules! unary_op {
    ($op:ident, $b:expr) => {
        devconf_nodes::ast::AstExpr::UnaryOp {
            op: $crate::nodes::AstUnaryOp::$op,
            right: $b.into(),
        }
    };
}

#[macro_export]
macro_rules! stmt {
    (@assign [$($name:expr),*], $value:expr) => {
        $crate::nodes::AstStatement::Assignation {
            path: vec![$($name),*],
            value: Box::new($value)
        }
    };
    (@template $name:expr, [$($params:expr),*]; [$($param:expr => $default:expr),*]; $body:expr) => {
        $crate::nodes::AstStatement::TemplateDefinition {
            name: $name,
            params: Vec::from([$($params),*]),
            defaults: vec![$(($param, $default)),*],
            body: $body
        }
    };
    (@expr $expr:expr) => {
        $crate::nodes::AstStatement::Expression(Box::new($expr))
    };
    (@cond $test:expr, $body:expr, $otherwise:expr) => {
        $crate::nodes::AstStatement::Conditional {
            test: $test,
            body: $body,
            otherwise: $otherwise,
        }
    };
}

#[macro_export]
macro_rules! expr {
    (@lit $expr:expr) => {
        Box::new(devconf_nodes::ast::AstExpr::Literal($expr))
    };
    (@unboxed @lit $expr:expr) => {
        devconf_nodes::ast::AstExpr::Literal($expr)
    };
    (@ident $expr:expr) => {
        Box::new(devconf_nodes::ast::AstExpr::Ident($expr))
    };
    (@unboxed @ident $expr:expr) => {
        devconf_nodes::ast::AstExpr::Ident($expr)
    };
    (@unquoted $expr:expr) => {
        Box::new(devconf_nodes::ast::AstExpr::Literal(
            devconf_lexer::token::Literal::UnquotedString($expr),
        ))
    };
    (@unboxed @unquoted $expr:expr) => {
        devconf_nodes::ast::AstExpr::Literal(
            devconf_lexer::token::Literal::UnquotedString($expr),
        )
    };
    (@array $($expr:expr),*) => {
        Box::new(devconf_nodes::ast::AstExpr::Array(vec![$($expr),*]))
    };
    (@object $($key:expr => $value:expr),*) => {
        Box::new(devconf_nodes::ast::AstExpr::Object(vec![$(($key, $value)),*]))
    };
    (@inter $expr:expr) => {
        devconf_nodes::ast::AstExpr::Interpolation {
            expr: Box::new($expr),
        }
    };
    (@cast $expr:expr, $cast:expr) => {
        devconf_nodes::ast::AstExpr::Cast {
            expr: $expr,
            ty: $cast
        }
    };
    (@bin $left:expr, $op:ident, $right:expr) => {
        devconf_nodes::ast::AstExpr::BinaryExpr {
            op: devconf_nodes::ast::AstBinaryOp::$op,
            left: $left,
            right: $right
        }
    }
}
pub(crate) fn expand_expr(expr: &AstExpr, substitutions: &HashMap<String, AstExpr>) -> AstExpr {
    match expr {
        AstExpr::Ident(name) => {
            let expr = substitutions
                .get(name)
                .cloned()
                .unwrap_or_else(|| expr.clone());
            match expr {
                AstExpr::Ident(i) => AstExpr::Literal(Literal::UnquotedString(i)),
                e => e,
            }
        }
        AstExpr::Literal(lit) => AstExpr::Literal(lit.clone()),
        AstExpr::Array(elements) => AstExpr::Array(
            elements
                .iter()
                .map(|element| expand_expr(element, substitutions))
                .collect(),
        ),
        AstExpr::Object(pairs) => AstExpr::Object(
            pairs
                .iter()
                .map(|(key, value)| (key.clone(), expand_expr(value, substitutions)))
                .collect(),
        ),
        AstExpr::BinaryExpr { op, left, right } => AstExpr::BinaryExpr {
            op: op.clone(),
            left: Box::new(expand_expr(left, substitutions)),
            right: Box::new(expand_expr(right, substitutions)),
        },
        AstExpr::UnaryExpr { op, expr } => AstExpr::UnaryExpr {
            op: op.clone(),
            expr: Box::new(expand_expr(expr, substitutions)),
        },
        AstExpr::Cast { expr, ty } => AstExpr::Cast {
            expr: Box::new(expand_expr(expr, substitutions)),
            ty: ty.clone(),
        },
        AstExpr::Interpolation { expr } => expand_expr(expr, substitutions),
        AstExpr::FunctionCall { .. } => expr.clone(),
    }
}

#[must_use]
pub fn precedence_of(token: &Token) -> Option<(u8, AstBinaryOp)> {
    match token {
        T![Or] => Some((1, AstBinaryOp::Or)),
        T![And] => Some((2, AstBinaryOp::And)),
        T![Eq] => Some((3, AstBinaryOp::Eq)),
        T![Ne] => Some((4, AstBinaryOp::Ne)),
        _ => None,
    }
}

use std::collections::HashMap;

pub use bin_op;
use devconf_lexer::{
    T,
    token::{Literal, Token},
};
use devconf_nodes::ast::{AstBinaryOp, AstExpr};
pub use scope;
pub use stmt;
pub use unary_op;
