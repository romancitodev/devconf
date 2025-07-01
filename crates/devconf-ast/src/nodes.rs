use std::fmt;

use devconf_lexer::token::Literal;

#[derive(Debug, Clone, PartialEq)]
pub struct AstScope(pub Vec<AstStatement>);

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
    DotAccess {
        object: Box<AstExpr>,
        property: String,
    },
    Interpolation {
        expr: Box<AstExpr>,
        type_cast: Option<String>, // for :int, :string, etc.
    },
    FunctionCall {
        name: String,
        args: Vec<AstExpr>,
    },
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

#[derive(Debug, Clone, PartialEq)]
pub enum PathSegment {
    Static(String),
    Dynamic(Box<AstExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstStatement {
    Comment,
    Assignation(String, Box<AstExpr>),
    Expression(Box<AstExpr>),
    Conditional {
        test: Box<AstExpr>,
        body: AstScope,
        otherwise: Option<AstScope>,
    },
    DotAssignation {
        path: Vec<PathSegment>,
        value: Box<AstExpr>,
    },
    TemplateDefinition {
        name: String,
        params: Vec<String>,
        defaults: Vec<(String, AstExpr)>,
        body: AstScope,
    },
    TemplateCalling {
        name: String,
        args: Vec<AstExpr>,
    },
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
