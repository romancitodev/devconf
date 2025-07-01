#[macro_export]
macro_rules! scope {
    ($($expr:expr),*) => {
        $crate::nodes::AstScope(Vec::from([ $($expr),* ]))
    };
}

#[macro_export]
macro_rules! bin_op {
    ($a:expr, $op:ident, $b:expr) => {
        $crate::nodes::AstExpr::BinaryOp {
            op: $crate::nodes::AstBinaryOp::$op,
            left: $a.into(),
            right: $b.into(),
        }
    };
}

#[macro_export]
macro_rules! unary_op {
    ($op:ident, $b:expr) => {
        $crate::nodes::AstExpr::UnaryOp {
            op: $crate::nodes::AstUnaryOp::$op,
            right: $b.into(),
        }
    };
}

#[macro_export]
macro_rules! ast {
    (@assign $name:expr, $value:expr) => {
        $crate::nodes::AstStatement::Assignation($name, $value)
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
    (@dot $path:expr, $value:expr) => {
        $crate::nodes::AstStatement::DotAssignation {
            path: $path,
            value: Box::new($value),
        }
    };
}

#[macro_export]
macro_rules! stmt {
    (@lit $expr:expr) => {
        Box::new($crate::nodes::AstExpr::Literal($expr))
    };
    (@unboxed @lit $expr:expr) => {
        $crate::nodes::AstExpr::Literal($expr)
    };
    (@ident $expr:expr) => {
        Box::new($crate::nodes::AstExpr::Ident($expr))
    };
    (@unboxed @ident $expr:expr) => {
        $crate::nodes::AstExpr::Ident($expr)
    };
    (@unquoted $expr:expr) => {
        Box::new($crate::nodes::AstExpr::Literal(
            devconf_lexer::token::Literal::UnquotedString($expr),
        ))
    };
    (@unboxed @unquoted $expr:expr) => {
        $crate::nodes::AstExpr::Literal(
            devconf_lexer::token::Literal::UnquotedString($expr),
        )
    };
    (@array $($expr:expr),*) => {
        Box::new($crate::nodes::AstExpr::Array(vec![$($expr),*]))
    };
    (@object $($key:expr => $value:expr),*) => {
        Box::new($crate::nodes::AstExpr::Object(vec![$(($key, $value)),*]))
    };
    (@inter $expr:expr, $type_cast:expr) => {
        $crate::nodes::AstExpr::Interpolation {
            expr: Box::new($expr),
            type_cast: $type_cast,
        }
    };
}

pub use ast;
pub use bin_op;
pub use scope;
pub use unary_op;
