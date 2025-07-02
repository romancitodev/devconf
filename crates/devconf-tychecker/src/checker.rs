use std::{collections::HashMap, fmt};

use devconf_ast::nodes::{AstBinaryOp, AstExpr, AstUnaryOp};
use devconf_lexer::token::Literal;

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

pub enum Context {
    Expression, // on ${}
    Value,      // this wouldn't be possible.
}

// we implement the into because it's easier for us to check before.
impl Into<Type> for Literal {
    fn into(self) -> Type {
        match self {
            Self::Boolean(_) => Type::Bool,
            Self::Float(_) => Type::Float,
            Self::Integer(_) => Type::Int,
            Self::String(_) | Self::UnquotedString(_) => Type::String,
            Self::Null => Type::Null,
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
            Type::Array(e) => write!(f, "array of {}", e),
            Type::Object(hash_map) => write!(f, "hasmap of {:?}", hash_map),
            Type::Union(items) => write!(f, "union of {:?}", items),
            Type::Null => write!(f, "null"),
            Type::Unknown => write!(f, "array of unknown"),
        }
    }
}

pub struct TypeChecker;

impl TypeChecker {
    pub fn check_expr(&self, expr: &AstExpr, context: Context) -> Result<Type, String> {
        let inferred_type = self.infer_type(expr)?;

        if let Context::Expression = context
            && !self.is_valid_expression_type(&inferred_type)
        {
            return Err(format!(
                "Expression must be a string type, got {inferred_type}"
            ));
        }

        Ok(inferred_type)
    }

    pub(crate) fn infer_type(&self, expr: &AstExpr) -> Result<Type, String> {
        match expr {
            AstExpr::Ident(_) => Ok(Type::String),
            AstExpr::Literal(literal) => Ok(literal.clone().into()),
            AstExpr::BinaryExpr { op, left, right } => {
                let lhs = self.infer_type(left)?;
                let rhs = self.infer_type(right)?;

                self.resolve_binary_op(op, lhs, rhs)
            }
            AstExpr::UnaryExpr { op, expr } => {
                let expr = self.infer_type(expr)?;
                self.resolve_unary_op(op, &expr)
            }
            AstExpr::Array(elements) => {
                if elements.is_empty() {
                    return Ok(Type::Array(Box::new(Type::Unknown)));
                }

                let first = self.infer_type(&elements[0])?;
                let ty = elements.iter().skip(1).fold(first, |f, expr| {
                    let element = self.infer_type(expr).expect("Unable to infer type.");
                    self.unify_types(&f, &element)
                        .expect("Unable to unify types.")
                });
                Ok(Type::Array(Box::new(ty)))
            }
            AstExpr::Interpolation { expr } => match expr.as_ref() {
                AstExpr::Cast {
                    ty,
                    expr: cast_expr,
                } => {
                    let expected = self.string_to_type(ty)?;
                    let source_type = self.infer_type(cast_expr)?;
                    if !self.resolve_cast(&source_type, &expected) {
                        return Err(format!("Cannot cast from {} to {}", source_type, expected));
                    }
                    Ok(expected)
                }
                _ => {
                    let _inner_type = self.infer_type(expr)?;
                    Ok(Type::String)
                }
            },
            AstExpr::Cast { expr, ty } => {
                let expr = self.infer_type(expr)?;
                let expected = self.string_to_type(&ty)?;
                if !self.resolve_cast(&expr, &expected) {
                    return Err(format!("Cannot cast from {} to {}", expr, ty));
                }
                Ok(expected)
            }
            AstExpr::Object(pairs) => {
                let mut objects = HashMap::new();
                for (k, v) in pairs {
                    let ty = self.infer_type(v)?;
                    objects.insert(k.clone(), ty);
                }
                Ok(Type::Object(objects))
            }
            _ => Ok(Type::Unknown),
        }
    }

    pub(crate) fn resolve_cast(&self, ty: &Type, expected: &Type) -> bool {
        match (ty, expected) {
            (Type::String, _) => true,
            (Type::Int, Type::Float) | (Type::Float, Type::Int) => true,
            (a, b) => a == b,
        }
    }

    pub(crate) fn string_to_type(&self, ty: &String) -> Result<Type, String> {
        match ty.as_str() {
            "str" => Ok(Type::String),
            "int" => Ok(Type::Int),
            "float" => Ok(Type::Float),
            "bool" => Ok(Type::Bool),
            _ => Ok(Type::Unknown),
        }
    }

    fn resolve_binary_op(&self, op: &AstBinaryOp, lhs: Type, rhs: Type) -> Result<Type, String> {
        match op {
            AstBinaryOp::And | AstBinaryOp::Or => self.unify_types(&lhs, &rhs),
            AstBinaryOp::Eq | AstBinaryOp::Ne => Ok(Type::Bool),
        }
    }

    fn resolve_unary_op(&self, op: &AstUnaryOp, _expr: &Type) -> Result<Type, String> {
        match op {
            AstUnaryOp::Not => Ok(Type::Bool),
        }
    }

    fn is_valid_expression_type(&self, ty: &Type) -> bool {
        match ty {
            Type::String => true,
            Type::Union(types) if types.iter().all(|t| matches!(t, Type::String)) => true,
            _ => false,
        }
    }

    fn unify_types(&self, a: &Type, b: &Type) -> Result<Type, String> {
        if a == b {
            Ok(a.clone())
        } else {
            Ok(Type::Union(vec![a.clone(), b.clone()]))
        }
    }
}
