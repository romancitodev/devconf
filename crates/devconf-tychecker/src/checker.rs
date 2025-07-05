use std::{collections::HashMap, fmt};

use devconf_lexer::token::Literal;
use devconf_nodes::ast::{AstBinaryOp, AstExpr, AstUnaryOp};

// These types are the known for the type checker
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    String,
    Int,
    Float,
    Bool,
    Array(Box<Type>),
    Object(HashMap<String, Type>),
    Union(Vec<Type>),
    Null,
    Unknown,
}

#[derive(Debug, Clone, Copy)]
pub enum Context {
    Expression, // on ${}
    If,         // If I'm inside an if expression.
    Value,      // this wouldn't be possible.
}

// we implement the into because it's easier for us to check before.
impl From<Literal> for Type {
    fn from(val: Literal) -> Self {
        match val {
            Literal::Boolean(_) => Type::Bool,
            Literal::Float(_) => Type::Float,
            Literal::Integer(_) => Type::Int,
            Literal::String(_) | Literal::UnquotedString(_) => Type::String,
            Literal::Null => Type::Null,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::String => write!(f, "string"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "boolean"),
            Type::Array(e) => write!(f, "array of {e}"),
            Type::Object(hash_map) => write!(f, "hasmap of {hash_map:?}"),
            Type::Union(items) => write!(f, "union of {items:?}"),
            Type::Null => write!(f, "null"),
            Type::Unknown => write!(f, "array of unknown"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeChecker;

impl TypeChecker {
    /// # Errors
    /// - The errors can only occurr when the `Context` is `Expression` and the final inferred type isn't a string.
    pub fn check_expr(&self, expr: &AstExpr, context: Context) -> Result<Type, String> {
        let inferred_type = Self::infer_type(expr)?;

        if let Context::Expression = context
            && !Self::is_valid_expression_type(&inferred_type)
        {
            return Err(format!(
                "Expression must be a string type, got {inferred_type}"
            ));
        }

        Ok(inferred_type)
    }

    #[allow(clippy::match_wildcard_for_single_variants)]
    pub(crate) fn infer_type(expr: &AstExpr) -> Result<Type, String> {
        match expr {
            AstExpr::Ident(_) => Ok(Type::String),
            AstExpr::Literal(literal) => Ok(literal.clone().into()),
            AstExpr::BinaryExpr { op, left, right } => {
                let lhs = Self::infer_type(left)?;
                let rhs = Self::infer_type(right)?;
                Ok(Self::resolve_binary_op(op, &lhs, &rhs))
            }
            AstExpr::UnaryExpr { op, expr } => {
                let expr = Self::infer_type(expr)?;
                Ok(Self::resolve_unary_op(op, &expr))
            }
            AstExpr::Array(elements) => {
                if elements.is_empty() {
                    return Ok(Type::Array(Box::new(Type::Unknown)));
                }

                let first = Self::infer_type(&elements[0])?;
                let ty = elements.iter().skip(1).fold(first, |f, expr| {
                    let element = Self::infer_type(expr).expect("Unable to infer type.");
                    Self::unify_types(&f, &element)
                });
                Ok(Type::Array(Box::new(ty)))
            }
            AstExpr::Interpolation { expr } => {
                if let AstExpr::Cast {
                    ty,
                    expr: cast_expr,
                } = expr.as_ref()
                {
                    let expected = Self::string_to_type(ty);
                    let source_type = Self::infer_type(cast_expr)?;
                    if !Self::resolve_cast(&source_type, &expected) {
                        return Err(format!("Cannot cast from {source_type} to {expected}"));
                    }
                    Ok(expected)
                } else {
                    Self::infer_type(expr)
                }
            }
            AstExpr::Cast { expr, ty } => {
                let expr = Self::infer_type(expr)?;
                let expected = Self::string_to_type(ty);
                if !Self::resolve_cast(&expr, &expected) {
                    return Err(format!("Cannot cast from {expr} to {ty}"));
                }
                Ok(expected)
            }
            AstExpr::Object(pairs) => {
                let mut objects = HashMap::new();
                for (k, v) in pairs {
                    let ty = Self::infer_type(v)?;
                    objects.insert(k.clone(), ty);
                }
                Ok(Type::Object(objects))
            }
            _ => Ok(Type::Unknown),
        }
    }

    pub(crate) fn resolve_cast(ty: &Type, expected: &Type) -> bool {
        match (ty, expected) {
            (Type::Int, Type::Float) | (Type::Float, Type::Int) | (Type::String, _) => true,
            (a, b) => a == b,
        }
    }

    pub(crate) fn string_to_type(ty: &str) -> Type {
        match ty {
            "str" => Type::String,
            "int" => Type::Int,
            "float" => Type::Float,
            "bool" => Type::Bool,
            _ => Type::Unknown,
        }
    }

    fn resolve_binary_op(op: &AstBinaryOp, lhs: &Type, rhs: &Type) -> Type {
        match op {
            AstBinaryOp::And | AstBinaryOp::Or => Self::unify_types(lhs, rhs),
            AstBinaryOp::Eq | AstBinaryOp::Ne => Type::Bool,
        }
    }

    fn resolve_unary_op(op: &AstUnaryOp, _expr: &Type) -> Type {
        match op {
            AstUnaryOp::Not => Type::Bool,
        }
    }

    fn is_valid_expression_type(ty: &Type) -> bool {
        match ty {
            Type::String => true,
            Type::Union(types) if types.iter().all(|t| matches!(t, Type::String)) => true,
            _ => false,
        }
    }

    fn unify_types(a: &Type, b: &Type) -> Type {
        if a == b {
            a.clone()
        } else {
            Type::Union(vec![a.clone(), b.clone()])
        }
    }
}
