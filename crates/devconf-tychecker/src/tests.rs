#[cfg(test)]
mod tests {
    use crate::checker::*;
    use devconf_ast::nodes::*;
    use devconf_lexer::token::Literal;

    fn create_checker() -> TypeChecker {
        TypeChecker
    }

    #[test]
    fn test_literal_types() {
        let checker = create_checker();

        // String literals
        let string_expr = AstExpr::Literal(Literal::String("hello".to_string()));
        assert_eq!(checker.infer_type(&string_expr).unwrap(), Type::String);

        let unquoted_string = AstExpr::Literal(Literal::UnquotedString("world".to_string()));
        assert_eq!(checker.infer_type(&unquoted_string).unwrap(), Type::String);

        // Numeric literals
        let int_expr = AstExpr::Literal(Literal::Integer(42));
        assert_eq!(checker.infer_type(&int_expr).unwrap(), Type::Int);

        let float_expr = AstExpr::Literal(Literal::Float(3.14));
        assert_eq!(checker.infer_type(&float_expr).unwrap(), Type::Float);

        // Boolean literal
        let bool_expr = AstExpr::Literal(Literal::Boolean(true));
        assert_eq!(checker.infer_type(&bool_expr).unwrap(), Type::Bool);

        // Null literal
        let null_expr = AstExpr::Literal(Literal::Null);
        assert_eq!(checker.infer_type(&null_expr).unwrap(), Type::Null);
    }

    #[test]
    fn test_identifier_type() {
        let checker = create_checker();

        let ident_expr = AstExpr::Ident("my_var".to_string());
        assert_eq!(checker.infer_type(&ident_expr).unwrap(), Type::String);
    }

    #[test]
    fn test_binary_operations() {
        let checker = create_checker();

        // Logical operations should unify types
        let and_expr = AstExpr::BinaryExpr {
            op: AstBinaryOp::And,
            left: Box::new(AstExpr::Literal(Literal::String("hello".to_string()))),
            right: Box::new(AstExpr::Literal(Literal::String("world".to_string()))),
        };
        assert_eq!(checker.infer_type(&and_expr).unwrap(), Type::String);

        // Different types create union
        let or_expr = AstExpr::BinaryExpr {
            op: AstBinaryOp::Or,
            left: Box::new(AstExpr::Literal(Literal::String("hello".to_string()))),
            right: Box::new(AstExpr::Literal(Literal::Integer(42))),
        };
        let result = checker.infer_type(&or_expr).unwrap();
        if let Type::Union(types) = result {
            assert!(types.contains(&Type::String));
            assert!(types.contains(&Type::Int));
        } else {
            panic!("Expected union type");
        }

        // Comparison operations return bool
        let eq_expr = AstExpr::BinaryExpr {
            op: AstBinaryOp::Eq,
            left: Box::new(AstExpr::Literal(Literal::Integer(1))),
            right: Box::new(AstExpr::Literal(Literal::Integer(2))),
        };
        assert_eq!(checker.infer_type(&eq_expr).unwrap(), Type::Bool);

        let ne_expr = AstExpr::BinaryExpr {
            op: AstBinaryOp::Ne,
            left: Box::new(AstExpr::Literal(Literal::String("a".to_string()))),
            right: Box::new(AstExpr::Literal(Literal::String("b".to_string()))),
        };
        assert_eq!(checker.infer_type(&ne_expr).unwrap(), Type::Bool);
    }

    #[test]
    fn test_unary_operations() {
        let checker = create_checker();

        let not_expr = AstExpr::UnaryExpr {
            op: AstUnaryOp::Not,
            expr: Box::new(AstExpr::Literal(Literal::Boolean(true))),
        };
        assert_eq!(checker.infer_type(&not_expr).unwrap(), Type::Bool);
    }

    #[test]
    fn test_array_types() {
        let checker = create_checker();

        // Empty array
        let empty_array = AstExpr::Array(vec![]);
        assert_eq!(
            checker.infer_type(&empty_array).unwrap(),
            Type::Array(Box::new(Type::Unknown))
        );

        // Homogeneous array
        let string_array = AstExpr::Array(vec![
            AstExpr::Literal(Literal::String("a".to_string())),
            AstExpr::Literal(Literal::String("b".to_string())),
            AstExpr::Literal(Literal::String("c".to_string())),
        ]);
        assert_eq!(
            checker.infer_type(&string_array).unwrap(),
            Type::Array(Box::new(Type::String))
        );

        // Mixed array creates union type
        let mixed_array = AstExpr::Array(vec![
            AstExpr::Literal(Literal::String("hello".to_string())),
            AstExpr::Literal(Literal::Integer(42)),
        ]);
        let result = checker.infer_type(&mixed_array).unwrap();
        if let Type::Array(inner) = result {
            if let Type::Union(types) = *inner {
                assert!(types.contains(&Type::String));
                assert!(types.contains(&Type::Int));
            } else {
                panic!("Expected union type in array");
            }
        } else {
            panic!("Expected array type");
        }
    }

    #[test]
    fn test_object_types() {
        let checker = create_checker();

        let object_expr = AstExpr::Object(vec![
            (
                "name".to_string(),
                AstExpr::Literal(Literal::String("John".to_string())),
            ),
            ("age".to_string(), AstExpr::Literal(Literal::Integer(30))),
            (
                "active".to_string(),
                AstExpr::Literal(Literal::Boolean(true)),
            ),
        ]);

        let result = checker.infer_type(&object_expr).unwrap();
        if let Type::Object(fields) = result {
            assert_eq!(fields.get("name"), Some(&Type::String));
            assert_eq!(fields.get("age"), Some(&Type::Int));
            assert_eq!(fields.get("active"), Some(&Type::Bool));
        } else {
            panic!("Expected object type");
        }
    }

    #[test]
    fn test_cast_expressions() {
        let checker = create_checker();

        // Valid casts
        let string_to_int = AstExpr::Cast {
            expr: Box::new(AstExpr::Literal(Literal::String("42".to_string()))),
            ty: "int".to_string(),
        };
        assert_eq!(checker.infer_type(&string_to_int).unwrap(), Type::Int);

        let int_to_float = AstExpr::Cast {
            expr: Box::new(AstExpr::Literal(Literal::Integer(42))),
            ty: "float".to_string(),
        };
        assert_eq!(checker.infer_type(&int_to_float).unwrap(), Type::Float);

        // Invalid cast should fail
        let bool_to_int = AstExpr::Cast {
            expr: Box::new(AstExpr::Literal(Literal::Boolean(true))),
            ty: "int".to_string(),
        };
        assert!(checker.infer_type(&bool_to_int).is_err());
    }

    #[test]
    fn test_interpolation_expressions() {
        let checker = create_checker();

        // Simple interpolation
        let simple_interp = AstExpr::Interpolation {
            expr: Box::new(AstExpr::Ident("my_var".to_string())),
        };
        assert_eq!(checker.infer_type(&simple_interp).unwrap(), Type::String);

        // Interpolation with cast
        let cast_interp = AstExpr::Interpolation {
            expr: Box::new(AstExpr::Cast {
                expr: Box::new(AstExpr::Literal(Literal::String("42".to_string()))),
                ty: "int".to_string(),
            }),
        };
        assert_eq!(checker.infer_type(&cast_interp).unwrap(), Type::Int);

        // Interpolation with invalid cast should fail
        let invalid_cast_interp = AstExpr::Interpolation {
            expr: Box::new(AstExpr::Cast {
                expr: Box::new(AstExpr::Literal(Literal::Boolean(true))),
                ty: "int".to_string(),
            }),
        };
        assert!(checker.infer_type(&invalid_cast_interp).is_err());
    }

    #[test]
    fn test_context_validation_expression() {
        let checker = create_checker();

        // Valid expression context (string)
        let string_expr = AstExpr::Literal(Literal::String("valid".to_string()));
        assert!(
            checker
                .check_expr(&string_expr, Context::Expression)
                .is_ok()
        );

        // Valid expression context (string union)
        let string_union = AstExpr::BinaryExpr {
            op: AstBinaryOp::And,
            left: Box::new(AstExpr::Literal(Literal::String("a".to_string()))),
            right: Box::new(AstExpr::Literal(Literal::String("b".to_string()))),
        };
        assert!(
            checker
                .check_expr(&string_union, Context::Expression)
                .is_ok()
        );

        // Invalid expression context (non-string)
        let int_expr = AstExpr::Literal(Literal::Integer(42));
        assert!(checker.check_expr(&int_expr, Context::Expression).is_err());

        // Invalid expression context (mixed union)
        let mixed_union = AstExpr::BinaryExpr {
            op: AstBinaryOp::Or,
            left: Box::new(AstExpr::Literal(Literal::String("a".to_string()))),
            right: Box::new(AstExpr::Literal(Literal::Integer(42))),
        };
        assert!(
            checker
                .check_expr(&mixed_union, Context::Expression)
                .is_err()
        );
    }

    #[test]
    fn test_context_validation_value() {
        let checker = create_checker();

        // Value context should accept any type
        let string_expr = AstExpr::Literal(Literal::String("value".to_string()));
        assert!(checker.check_expr(&string_expr, Context::Value).is_ok());

        let int_expr = AstExpr::Literal(Literal::Integer(42));
        assert!(checker.check_expr(&int_expr, Context::Value).is_ok());

        let bool_expr = AstExpr::Literal(Literal::Boolean(true));
        assert!(checker.check_expr(&bool_expr, Context::Value).is_ok());

        let array_expr = AstExpr::Array(vec![1i64.into(), 2i64.into()]);
        assert!(checker.check_expr(&array_expr, Context::Value).is_ok());
    }

    #[test]
    fn test_string_to_type_conversion() {
        let checker = create_checker();

        assert_eq!(
            checker.string_to_type(&"str".to_string()).unwrap(),
            Type::String
        );
        assert_eq!(
            checker.string_to_type(&"int".to_string()).unwrap(),
            Type::Int
        );
        assert_eq!(
            checker.string_to_type(&"float".to_string()).unwrap(),
            Type::Float
        );
        assert_eq!(
            checker.string_to_type(&"bool".to_string()).unwrap(),
            Type::Bool
        );
        // Unknown type should return error
        assert_eq!(
            checker.string_to_type(&"unknown_type".to_string()).unwrap(),
            Type::Unknown
        );
    }

    #[test]
    fn test_cast_resolution() {
        let checker = create_checker();

        // String can cast to anything
        assert!(checker.resolve_cast(&Type::String, &Type::Int));
        assert!(checker.resolve_cast(&Type::String, &Type::Float));
        assert!(checker.resolve_cast(&Type::String, &Type::Bool));

        // Numbers can cast between each other
        assert!(checker.resolve_cast(&Type::Int, &Type::Float));
        assert!(checker.resolve_cast(&Type::Float, &Type::Int));

        // Same types can "cast" to themselves
        assert!(checker.resolve_cast(&Type::String, &Type::String));
        assert!(checker.resolve_cast(&Type::Int, &Type::Int));

        // Invalid casts
        assert!(!checker.resolve_cast(&Type::Bool, &Type::Int));
        assert!(!checker.resolve_cast(&Type::Array(Box::new(Type::String)), &Type::String));
    }

    #[test]
    fn test_complex_nested_expressions() {
        let checker = create_checker();

        // Nested binary operations
        let complex_expr = AstExpr::BinaryExpr {
            op: AstBinaryOp::And,
            left: Box::new(AstExpr::BinaryExpr {
                // bool
                op: AstBinaryOp::Eq,
                left: Box::new(AstExpr::Ident("x".to_string())), // string
                right: Box::new(AstExpr::Literal(Literal::String("test".to_string()))), // string
            }),
            right: Box::new(AstExpr::UnaryExpr {
                // bool
                op: AstUnaryOp::Not,
                expr: Box::new(AstExpr::Literal(Literal::Boolean(false))), // bool
            }),
        };

        let result = checker.infer_type(&complex_expr).unwrap();

        println!("got: {result}");

        assert_eq!(result, Type::Bool, "Expected union type with bool");

        // Array of objects
        let array_of_objects = AstExpr::Array(vec![
            AstExpr::Object(vec![
                ("id".to_string(), AstExpr::Literal(Literal::Integer(1))),
                (
                    "name".to_string(),
                    AstExpr::Literal(Literal::String("first".to_string())),
                ),
            ]),
            AstExpr::Object(vec![
                ("id".to_string(), AstExpr::Literal(Literal::Integer(2))),
                (
                    "name".to_string(),
                    AstExpr::Literal(Literal::String("second".to_string())),
                ),
            ]),
        ]);

        let result = checker.infer_type(&array_of_objects).unwrap();
        if let Type::Array(inner) = result {
            if let Type::Object(fields) = *inner {
                assert_eq!(fields.get("id"), Some(&Type::Int));
                assert_eq!(fields.get("name"), Some(&Type::String));
            } else {
                panic!("Expected object type in array");
            }
        } else {
            panic!("Expected array type");
        }
    }

    #[test]
    fn test_error_messages() {
        let checker = create_checker();

        // Invalid cast error message
        let invalid_cast = AstExpr::Cast {
            expr: Box::new(AstExpr::Literal(Literal::Boolean(true))),
            ty: "int".to_string(),
        };
        let error = checker.infer_type(&invalid_cast).unwrap_err();
        assert!(error.contains("Cannot cast from boolean to int"));

        // Invalid expression context error message
        let int_in_expr_context = AstExpr::Literal(Literal::Integer(42));
        let error = checker
            .check_expr(&int_in_expr_context, Context::Expression)
            .unwrap_err();
        assert!(error.contains("Expression must be a string type, got int"));

        // Unknown type error message
        let t = checker.string_to_type(&"invalid_type".to_string()).unwrap();
        assert_eq!(t, Type::Unknown);
    }
}
