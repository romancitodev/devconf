use crate::*;
use devconf_lexer::Lexer;

fn create_scope(content: &str) -> AstScope {
    AstScope::from_tokens(content, Lexer::from_str(content).unwrap())
}

#[test]
fn test_simple_string() {
    let input = "\"Hello, world!\"";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![stmt!(@expr "Hello, world!".to_owned().into())]
    );
    let input = "'Hello, world!'";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![stmt!(@expr "Hello, world!".to_owned().into())]
    );
}

#[test]
fn test_simple_assignment() {
    let input = "app: 'rust'";
    let scope = create_scope(input);

    assert_eq!(
        scope,
        scope![stmt! {
            @assign [PathSegment::Static("app".to_owned().into())], expr!(@unboxed @lit "rust".to_owned().into())
        }]
    );
}

#[test]
fn test_simple_assignment_unquoted_string() {
    let input = "app: rust";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![stmt! {
            @assign [PathSegment::Static("app".to_owned().into())], expr!(@unboxed @unquoted "rust".to_owned().into())
        }]
    );
}

#[test]
fn test_simple_assignment_string() {
    let input = "app: 'rust'";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![stmt! {
            @assign [PathSegment::Static("app".to_owned().into())], expr!(@unboxed @lit "rust".to_owned().into())
        }]
    );
}

#[test]
fn test_simple_assignment_integer() {
    let input = "version: 1";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![stmt! {
            @assign [PathSegment::Static("version".to_owned().into())], expr!(@unboxed @lit 1.into())
        }]
    );
}

#[test]
fn test_simple_assignment_formatted_integer() {
    let input = "version: 123_456";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![stmt! {
            @assign [PathSegment::Static("version".to_owned().into())], expr!(@unboxed @lit 123456.into())
        }]
    );
}

#[test]
fn test_simple_comment() {
    let input = "# A comment";
    let scope = create_scope(input);
    assert_eq!(scope, scope![]);
}

#[test]
fn test_comment_and_assignment() {
    let input = "# A comment\napp: 'rust' # this is the app";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![stmt! {
            @assign [PathSegment::Static("app".to_owned().into())], expr!(@unboxed @lit "rust".to_owned().into())
        }]
    );
}

#[test]
fn test_single_array() {
    let input = "items: ['apple', 'banana', 'cherry']";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![stmt! {
            @assign [PathSegment::Static("items".to_owned().into())],
            *expr![@array
                expr![@unboxed @lit "apple".to_owned().into()],
                expr![@unboxed @lit "banana".to_owned().into()],
                expr![@unboxed @lit "cherry".to_owned().into()]
            ]
        }]
    );
}

#[test]
fn test_mixed_array() {
    let input = "items: [apple, 'banana', 'cherry', true, 42]";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![stmt! {
            @assign [PathSegment::Static("items".to_owned().into())],
            *expr![@array
                expr![@unboxed @unquoted "apple".to_owned().into()],
                expr![@unboxed @lit "banana".to_owned().into()],
                expr![@unboxed @lit "cherry".to_owned().into()],
                expr![@unboxed @lit true.into()],
                expr![@unboxed @lit 42.into()]
            ]
        }]
    );
}

#[test]
fn test_simple_object() {
    let input = "items: {apple: 1, banana: 2, cherry: 3}";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![stmt! {
            @assign [PathSegment::Static("items".to_owned().into())],
            *expr![@object
                "apple".to_owned() => expr![@unboxed @lit 1.into()],
                "banana".to_owned() => expr![@unboxed @lit 2.into()],
                "cherry".to_owned() => expr![@unboxed @lit 3.into()]
            ]
        }]
    );
}

#[test]
fn test_complex_object() {
    let input = "app: { name: 'My App', version: 1.0, authors: [roman, 'Apika luca']}";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![stmt! {
            @assign [PathSegment::Static("app".to_owned().into())],
            *expr![@object
                "name".to_owned() => expr![@unboxed @lit "My App".to_owned().into()],
                "version".to_owned() => expr![@unboxed @lit 1.0.into()],
                "authors".to_owned() => *expr![@array
                    expr![@unboxed @unquoted "roman".to_owned().into()],
                    expr![@unboxed @lit "Apika luca".to_owned().into()]
                ]
            ]
        }]
    );
}

#[test]
fn test_assignment_and_interpolation() {
    let input = "port: ${APP_PORT}";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![
            stmt![@assign [PathSegment::Static("port".to_owned().into())],
                expr![@inter expr!(@unboxed @ident "APP_PORT".to_owned().into())].into()]
        ]
    )
}

#[test]
fn test_paren() {
    let input = "port: (83)";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![
            stmt![@assign [PathSegment::Static("port".to_owned().into())],
                expr!(@unboxed @lit 83.into())]
        ]
    )
}

#[test]
fn test_assignment_and_interpolation_with_type() {
    let input = "port: ${APP_PORT:int}";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![
            stmt![@assign [PathSegment::Static("port".to_owned().into())],
                expr![@inter expr!(
                    @cast expr!(@unboxed @ident "APP_PORT".to_owned().into()
                ).into(),
                    "int".to_owned()
                )].into()]
        ]
    )
}
#[test]
fn test_interpolation_with_expression() {
    let input = "port: ${APP_PORT || 8080}";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![
            stmt![@assign [PathSegment::Static("port".to_owned().into())],
                expr![@inter
                    expr![
                    @bin expr!(@ident "APP_PORT".to_owned().into()), Or, expr!(@lit 8080.into()).into()
                    ].into()].into()]
        ]
    )
}

#[test]
fn test_complex_interpolation_with_expression() {
    let input = "port: ${APP_PORT:int || 8080}";
    let scope = create_scope(input);
    assert_eq!(
        scope,
        scope![
            stmt![@assign [PathSegment::Static("port".to_owned().into())],
                expr![@inter
                    expr![@bin
                        expr!(@cast
                            expr!(@ident "APP_PORT".to_owned()), "int".to_owned()).into(),
                            Or,
                            expr!(@lit 8080.into()).into()
                    ].into()
                ].into()
            ]
        ]
    )
}

#[test]
#[should_panic = "Received :, so there was expected a type hint, but instead no one was provided\n if you don't want to add a type hint, just close the interpolation"]
fn test_invalid_assignment_and_interpolation() {
    let input = "port: ${APP_PORT:}";
    _ = create_scope(input);
}

#[test]
fn test_dot_assignation() {
    let input = "app.author: 'roman'";
    let scope = create_scope(input);

    assert_eq!(
        scope,
        scope![stmt! [
            @assign [
                PathSegment::Static("app".to_owned().into()),
                PathSegment::Static("author".to_owned().into())
            ],
            expr!(@unboxed @lit "roman".to_owned().into())
        ]]
    )
}

// #[ignore = "unimplemented!()"]
#[test]
fn test_dot_assignation_with_interpolation() {
    let input = "app.${author}: 'roman'";
    let scope = create_scope(input);

    assert_eq!(
        scope,
        scope![stmt! [
            @assign [
                PathSegment::Static("app".to_owned().into()),
                PathSegment::Dynamic(expr!(@inter expr!(@unboxed @ident "author".to_owned().into())).into())
            ],
            expr!(@unboxed @lit "roman".to_owned().into())
        ]]
    )
}

#[test]
fn test_valid_type_resolved_expresssion() {
    let input = "app.${APP_NAME || 'default'}: 'roman'";
    let scope = create_scope(input);

    assert_eq!(
        scope,
        scope![stmt! [
            @assign [
                PathSegment::Static("app".to_owned().into()),
                PathSegment::Dynamic(
                    expr![@inter
                    expr![@bin
                        expr!(@ident "APP_NAME".to_owned()).into(),
                            Or,
                            expr!(@lit "default".into())
                    ].into()
                ].into()
                )
            ],
            expr!(@unboxed @lit "roman".to_owned().into())
        ]]
    )
}

#[test]
#[should_panic]
fn test_invalid_dot_assignation_with_complex_interpolation() {
    let input = "app.${APP_NAME || default || 42}: 'roman'";
    let _ = create_scope(input);
}

#[test]
#[should_panic]
fn test_invalid_dot_assignation_with_interpolation() {
    let input = "app.${42}: 'roman'";
    let _ = create_scope(input);
}

#[test]
#[should_panic]
fn test_invalid_dot_type_hinting_on_interpolation() {
    let input = "app.${42:str}: 'roman'";
    let _ = create_scope(input);
}

#[test]
fn test_dot_assignation_with_literals() {
    let input = "app.'author': 'roman'";
    let scope = create_scope(input);

    assert_eq!(
        scope,
        scope![stmt! [
            @assign [
                PathSegment::Static("app".to_owned().into()),
                PathSegment::Static("author".to_owned().into())
            ],
            expr!(@unboxed @lit "roman".to_owned().into())
        ]]
    )
}

#[test]
#[should_panic = "Seems to be an invalid token (Integer(42))"]
fn test_invalid_dot_assignation_with_literals() {
    let input = "app.42: 'roman'";
    let _ = create_scope(input);
}

#[test]
fn test_dot_assignation_with_nested_fields() {
    let input = "app.user.enabled: true";
    let scope = create_scope(input);

    assert_eq!(
        scope,
        scope![stmt! [
            @assign [
                PathSegment::Static("app".to_owned().into()),
                PathSegment::Static("user".to_owned().into()),
                PathSegment::Static("enabled".to_owned().into())
            ],
            expr!(@unboxed @lit true.into())
        ]]
    )
}

#[test]
#[should_panic]
fn test_invalid_dot_assignation_with_dots() {
    let input = "app..author: 'roman'";
    let scope = create_scope(input);

    assert_eq!(
        scope,
        scope![stmt! [
            @assign [
                PathSegment::Static("app".to_owned().into()),
                PathSegment::Static("author".to_owned().into())
            ],
            expr!(@unboxed @lit "roman".to_owned().into())
        ]]
    )
}
