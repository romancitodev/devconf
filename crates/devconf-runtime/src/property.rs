use hashbrown::HashMap;
use std::env;

use devconf_ast::{AstScope, nodes::AstStatement};
use devconf_lexer::token::Literal;
use devconf_nodes::ast::{AstBinaryOp, AstExpr, AstUnaryOp};

use super::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Table(pub HashMap<String, Value>);

impl Table {
    #[must_use]
    pub fn from_ast(ast: AstScope) -> Self {
        let mut map = HashMap::new();
        for stmt in ast.0 {
            if matches!(stmt, AstStatement::TemplateDefinition { .. }) {
                continue;
            }
            let (key, value) = Self::map_statement(stmt);
            Self::insert_nested_value(&mut map, &key, value);
        }
        Self(map)
    }

    fn insert_nested_value(obj: &mut HashMap<String, Value>, path: &str, value: Value) {
        if let Value::Object(nested_obj) = value {
            for (nested_key, nested_value) in nested_obj {
                let full_key = if path.is_empty() {
                    nested_key
                } else {
                    format!("{path}.{nested_key}")
                };
                Self::insert_nested_value(obj, &full_key, nested_value);
            }
            return;
        }

        let path_parts: Vec<&str> = path.split('.').collect();

        if path_parts.len() == 1 {
            obj.insert(path_parts[0].to_string(), value);
            return;
        }

        let current_key = path_parts[0];
        let remaining_path = path_parts[1..].join(".");

        let intermediate = obj
            .entry(current_key.to_string())
            .or_insert_with(|| Value::Object(HashMap::new()));

        if !matches!(intermediate, Value::Object(_)) {
            *intermediate = Value::Object(HashMap::new());
        }

        if let Value::Object(nested_map) = intermediate {
            Self::insert_nested_value(nested_map, &remaining_path, value);
        }
    }

    fn resolve_bin_op_value(op: &AstBinaryOp, left: AstExpr, right: AstExpr) -> Value {
        // println!("Resolving binary operation: {op:?} with left: {left:?} and right: {right:?}");
        match op {
            AstBinaryOp::Eq => {
                Value::Boolean(Self::resolve_expr(left) == Self::resolve_expr(right))
            }
            AstBinaryOp::Ne => {
                Value::Boolean(Self::resolve_expr(left) != Self::resolve_expr(right))
            }
            AstBinaryOp::And => Value::Boolean(
                Self::resolve_expr(left).as_bool() && Self::resolve_expr(right).as_bool(),
            ),
            AstBinaryOp::Or => {
                let lhs = Self::resolve_expr(left.clone());
                if lhs.as_bool() {
                    lhs
                } else {
                    Self::resolve_expr(right.clone())
                }
            }
        }
    }

    fn resolve_expr(expr: AstExpr) -> Value {
        // println!("Resolving expr: {expr:?}");
        match expr {
            AstExpr::Ident(id) => Self::resolve_ident(&id),
            AstExpr::Literal(literal) => Self::resolve_literal(literal),
            AstExpr::BinaryExpr { op, left, right } => {
                Self::resolve_bin_op_value(&op, *left, *right)
            }
            AstExpr::UnaryExpr { op, expr } => Self::resolve_unary_op(&op, *expr),
            AstExpr::Array(ast_exprs) => {
                let values: Vec<Value> = ast_exprs.into_iter().map(Self::resolve_expr).collect();
                Value::Array(values)
            }
            AstExpr::Object(items) => {
                let mut obj_map = HashMap::new();
                for (key, value_expr) in items {
                    let resolved_value = Self::resolve_expr(value_expr);
                    // Handle potential dot-separated keys in object literals
                    Self::insert_nested_value(&mut obj_map, &key, resolved_value);
                }
                Value::Object(obj_map)
            }
            AstExpr::Interpolation { expr } => Self::resolve_expr(*expr),
            AstExpr::Cast { expr, ty } => Self::resolve_cast(*expr, ty.as_str()),
            AstExpr::FunctionCall { .. } => unreachable!(),
        }
    }

    fn resolve_ident(ident: &str) -> Value {
        let value = env::var(ident).unwrap_or_default();
        // println!("Resolving ident: {ident} -> {value}");
        Value::String(value)
    }

    fn resolve_literal(expr: Literal) -> Value {
        match expr {
            Literal::String(n) | Literal::UnquotedString(n) => Value::String(n),
            Literal::Integer(n) => Value::Integer(n),
            Literal::Float(f) => Value::Float(f),
            Literal::Boolean(b) => Value::Boolean(b),
            Literal::Null => Value::Null,
        }
    }

    fn resolve_unary_op(op: &AstUnaryOp, expr: AstExpr) -> Value {
        match op {
            AstUnaryOp::Not => Value::Boolean(!Self::resolve_expr(expr).as_bool()),
        }
    }

    fn resolve_cast(expr: AstExpr, ty: &str) -> Value {
        // println!("Resolving cast: {expr:?} to type {ty}");
        let expr = Self::resolve_expr(expr);
        match ty {
            "string" | "str" => expr
                .as_str()
                .map_or(Value::String(String::new()), Value::String),
            "int" | "integer" => expr.as_int().map_or(Value::Integer(0), Value::Integer),
            "float" | "f" => expr.as_float().map_or(Value::Float(0.0), Value::Float),
            "bool" | "boolean" => {
                if expr.as_bool() {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            }
            _ => todo!(),
        }
    }

    fn map_statement(stmt: AstStatement) -> (String, Value) {
        match stmt {
            AstStatement::Assignation { path, value } => {
                // Resolve each path segment to a string
                let key = path
                    .into_iter()
                    .map(|segment| match segment {
                        devconf_ast::nodes::PathSegment::Static(s) => s,
                        devconf_ast::nodes::PathSegment::Dynamic(expr) => {
                            match Self::resolve_expr(*expr) {
                                Value::String(s) if !s.is_empty() => s,
                                v => {
                                    panic!("Dynamic path segment did not resolve to string: {v:?}")
                                }
                            }
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(".");

                let value = Self::resolve_expr(*value);
                (key, value)
            }
            AstStatement::Expression(_ast_expr) => {
                // Not an assignation, cannot map to property
                panic!("Expression statement cannot be mapped to property directly");
            }
            AstStatement::Conditional {
                test,
                body,
                otherwise,
            } => {
                // Resolve the test condition
                if Self::resolve_expr(*test).as_bool() {
                    // Map the first statement in the body (could be improved to handle multiple)
                    if let Some(stmt) = body.0.into_iter().next() {
                        Self::map_statement(stmt)
                    } else {
                        panic!("Conditional body is empty");
                    }
                } else if let Some(otherwise) = otherwise {
                    if let Some(stmt) = otherwise.0.into_iter().next() {
                        Self::map_statement(stmt)
                    } else {
                        panic!("Conditional 'otherwise' body is empty");
                    }
                } else {
                    // do nothing
                    ("".to_string(), Value::Null)
                }
            }
            _ => unreachable!(),
        }
    }
}
