use devconf_lexer::token::Literal;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum AstExpr {
    Ident(String),
    Literal(Literal),
    BinaryExpr {
        op: AstBinaryOp,
        left: Box<AstExpr>,
        right: Box<AstExpr>,
    },
    UnaryExpr {
        op: AstUnaryOp,
        expr: Box<AstExpr>,
    },
    Array(Vec<AstExpr>),
    Object(Vec<(String, AstExpr)>),
    Interpolation {
        expr: Box<AstExpr>, // we moved the type cast into a `AstExpr::Cast` to be able to track it
    },
    FunctionCall {
        name: String,
        args: Vec<AstExpr>,
    },
    Cast {
        expr: Box<AstExpr>,
        ty: String,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstBinaryOp {
    Eq,  // ==
    Ne,  // !=
    And, // &&
    Or,  // ||
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstUnaryOp {
    Not,
}

impl From<i64> for AstExpr {
    fn from(val: i64) -> Self {
        AstExpr::Literal(Literal::Integer(val))
    }
}

impl From<f64> for AstExpr {
    fn from(val: f64) -> Self {
        AstExpr::Literal(Literal::Float(val))
    }
}

impl From<&str> for AstExpr {
    fn from(val: &str) -> Self {
        AstExpr::Literal(Literal::String(val.to_owned()))
    }
}

impl From<String> for AstExpr {
    fn from(val: String) -> Self {
        AstExpr::Literal(Literal::String(val))
    }
}

impl From<bool> for AstExpr {
    fn from(val: bool) -> Self {
        AstExpr::Literal(Literal::Boolean(val))
    }
}

impl fmt::Display for AstBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstBinaryOp::Eq => write!(f, "=="),
            AstBinaryOp::Ne => write!(f, "!="),
            AstBinaryOp::And => write!(f, "&&"),
            AstBinaryOp::Or => write!(f, "||"),
        }
    }
}

impl fmt::Display for AstUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstUnaryOp::Not => write!(f, "!"),
        }
    }
}
