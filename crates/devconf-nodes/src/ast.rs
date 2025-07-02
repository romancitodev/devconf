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

impl Into<AstExpr> for i64 {
    fn into(self) -> AstExpr {
        AstExpr::Literal(Literal::Integer(self))
    }
}

impl Into<AstExpr> for f64 {
    fn into(self) -> AstExpr {
        AstExpr::Literal(Literal::Float(self))
    }
}

impl Into<AstExpr> for &str {
    fn into(self) -> AstExpr {
        AstExpr::Literal(Literal::String(self.to_owned()))
    }
}

impl Into<AstExpr> for String {
    fn into(self) -> AstExpr {
        AstExpr::Literal(Literal::String(self))
    }
}

impl Into<AstExpr> for bool {
    fn into(self) -> AstExpr {
        AstExpr::Literal(Literal::Boolean(self))
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
