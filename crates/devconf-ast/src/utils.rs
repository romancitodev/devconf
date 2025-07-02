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

pub use bin_op;
pub use scope;
pub use stmt;
pub use unary_op;
