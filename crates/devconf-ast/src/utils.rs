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
    (@inter $expr:expr) => {
        $crate::nodes::AstExpr::Interpolation {
            expr: Box::new($expr),
        }
    };
    (@cast $expr:expr, $cast:expr) => {
        $crate::nodes::AstExpr::Cast {
            expr: $expr,
            ty: $cast
        }
    };
    (@bin $left:expr, $op:ident, $right:expr) => {
        $crate::nodes::AstExpr::BinaryExpr {
            op: $crate::nodes::AstBinaryOp::$op,
            left: $left,
            right: $right
        }
    }
}

pub use bin_op;
pub use scope;
pub use stmt;
pub use unary_op;
