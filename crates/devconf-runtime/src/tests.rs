use devconf_ast::AstScope;
use devconf_lexer::Lexer;

use crate::{map::parse, value::Value};

fn create_scope(content: &str) -> AstScope {
    AstScope::from_tokens(content, Lexer::from_str(content).unwrap())
}

#[test]
fn test_simple_property() {
    let input = "name: \"my-app\"";
    let scope = create_scope(input);
    let table = parse(scope);

    assert_eq!(table.0.len(), 1);
    assert_eq!(
        table.0.get("name"),
        Some(&Value::String("my-app".to_string()))
    );
}

#[test]
fn test_nested_properties() {
    let input = "app.port: 8080\napp.name: \"my-service\"";
    let scope = create_scope(input);
    let table = parse(scope);

    assert_eq!(table.0.len(), 1);

    // Should have an "app" object
    if let Some(Value::Object(app_obj)) = table.0.get("app") {
        assert_eq!(app_obj.get("port"), Some(&Value::Integer(8080)));
        assert_eq!(
            app_obj.get("name"),
            Some(&Value::String("my-service".to_string()))
        );
    } else {
        panic!("Expected 'app' to be an object");
    }
}

#[test]
fn test_object_literal_flattening() {
    let input = "cache: { enabled: true, ttl: 300, type: \"redis\" }";
    let scope = create_scope(input);
    let table = parse(scope);

    assert_eq!(table.0.len(), 1);

    // Should have a "cache" object with flattened properties
    if let Some(Value::Object(cache_obj)) = table.0.get("cache") {
        assert_eq!(cache_obj.get("enabled"), Some(&Value::Boolean(true)));
        assert_eq!(cache_obj.get("ttl"), Some(&Value::Integer(300)));
        assert_eq!(
            cache_obj.get("type"),
            Some(&Value::String("redis".to_string()))
        );
    } else {
        panic!("Expected 'cache' to be an object");
    }
}

#[test]
fn test_deeply_nested_properties() {
    let input = "app.database.host: \"localhost\"\napp.database.port: 5432";
    let scope = create_scope(input);
    let table = parse(scope);

    assert_eq!(table.0.len(), 1);

    // Navigate through the nested structure
    if let Some(Value::Object(app_obj)) = table.0.get("app") {
        if let Some(Value::Object(db_obj)) = app_obj.get("database") {
            assert_eq!(
                db_obj.get("host"),
                Some(&Value::String("localhost".to_string()))
            );
            assert_eq!(db_obj.get("port"), Some(&Value::Integer(5432)));
        } else {
            panic!("Expected 'app.database' to be an object");
        }
    } else {
        panic!("Expected 'app' to be an object");
    }
}

#[test]
fn test_arrays() {
    let input = "features: [auth, logging, metrics]";
    let scope = create_scope(input);
    let table = parse(scope);

    assert_eq!(table.0.len(), 1);

    if let Some(Value::Array(features)) = table.0.get("features") {
        assert_eq!(features.len(), 3);
        assert_eq!(features[0], Value::String("auth".to_string()));
        assert_eq!(features[1], Value::String("logging".to_string()));
        assert_eq!(features[2], Value::String("metrics".to_string()));
    } else {
        panic!("Expected 'features' to be an array");
    }
}

#[test]
fn test_boolean_evaluation() {
    let input = "debug: true\nenabled: false";
    let scope = create_scope(input);
    let table = parse(scope);

    assert_eq!(table.0.len(), 2);
    assert_eq!(table.0.get("debug"), Some(&Value::Boolean(true)));
    assert_eq!(table.0.get("enabled"), Some(&Value::Boolean(false)));
}

#[test]
fn test_mixed_types() {
    let input = r#"
name: "test-app"
version: 1.0
port: 8080
debug: true
tags: [web, api]
"#;
    let scope = create_scope(input);
    let table = parse(scope);

    assert_eq!(table.0.len(), 5);
    assert_eq!(
        table.0.get("name"),
        Some(&Value::String("test-app".to_string()))
    );
    assert_eq!(table.0.get("version"), Some(&Value::Float(1.0)));
    assert_eq!(table.0.get("port"), Some(&Value::Integer(8080)));
    assert_eq!(table.0.get("debug"), Some(&Value::Boolean(true)));

    if let Some(Value::Array(tags)) = table.0.get("tags") {
        assert_eq!(tags.len(), 2);
        assert_eq!(tags[0], Value::String("web".to_string()));
        assert_eq!(tags[1], Value::String("api".to_string()));
    } else {
        panic!("Expected 'tags' to be an array");
    }
}

#[test]
fn test_complex_nested_structure() {
    let input = r#"
app.name: "my-service"
app.port: 8080
app.database.host: "localhost"
app.database.port: 5432
app.cache: { enabled: true, ttl: 300 }
"#;
    let scope = create_scope(input);
    let table = parse(scope);

    assert_eq!(table.0.len(), 1);

    if let Some(Value::Object(app_obj)) = table.0.get("app") {
        assert_eq!(
            app_obj.get("name"),
            Some(&Value::String("my-service".to_string()))
        );
        assert_eq!(app_obj.get("port"), Some(&Value::Integer(8080)));

        if let Some(Value::Object(db_obj)) = app_obj.get("database") {
            assert_eq!(
                db_obj.get("host"),
                Some(&Value::String("localhost".to_string()))
            );
            assert_eq!(db_obj.get("port"), Some(&Value::Integer(5432)));
        } else {
            panic!("Expected 'app.database' to be an object");
        }

        // Check cache object (from object literal)
        if let Some(Value::Object(cache_obj)) = app_obj.get("cache") {
            assert_eq!(cache_obj.get("enabled"), Some(&Value::Boolean(true)));
            assert_eq!(cache_obj.get("ttl"), Some(&Value::Integer(300)));
        } else {
            panic!("Expected 'app.cache' to be an object");
        }
    } else {
        panic!("Expected 'app' to be an object");
    }
}

#[test]
fn test_property_table() {
    let input = "port: ${APP_PORT:int || 8080}";
    let scope = create_scope(input);
    let table = parse(scope);
    // println!("{table:#?}");
    assert_eq!(table.0.len(), 1);
}

#[test]
fn test_with_multiple() {
    let input = "app.port: ${APP_PORT || 8081}\napp.name: pepe\napp.age: ${APP_AGE || 18}\napp.db.name: test";
    let scope = create_scope(input);
    let table = parse(scope);
    println!("{table:#?}");
    assert_eq!(table.0.len(), 1);
}

#[test]
fn test_primitives_devconf_structure() {
    let input = r#"
name: "my-app"
version: "1.0.0"
debug: false

app.port: 8080
app.database.host: "localhost"
app.database.pool_size: 10

features: [auth, logging, metrics]
cache: {
   enabled: true,
   ttl: 300,
   type: "redis"
}
"#;
    let scope = create_scope(input);
    let table = parse(scope);

    // Should have top-level properties: name, version, debug, app, features, cache
    assert_eq!(table.0.len(), 6);

    // Check basic properties
    assert_eq!(
        table.0.get("name"),
        Some(&Value::String("my-app".to_string()))
    );
    assert_eq!(
        table.0.get("version"),
        Some(&Value::String("1.0.0".to_string()))
    );
    assert_eq!(table.0.get("debug"), Some(&Value::Boolean(false)));

    // Check app nested structure
    if let Some(Value::Object(app_obj)) = table.0.get("app") {
        assert_eq!(app_obj.get("port"), Some(&Value::Integer(8080)));

        if let Some(Value::Object(db_obj)) = app_obj.get("database") {
            assert_eq!(
                db_obj.get("host"),
                Some(&Value::String("localhost".to_string()))
            );
            assert_eq!(db_obj.get("pool_size"), Some(&Value::Integer(10)));
        } else {
            panic!("Expected 'app.database' to be an object");
        }
    } else {
        panic!("Expected 'app' to be an object");
    }

    // Check features array
    if let Some(Value::Array(features)) = table.0.get("features") {
        assert_eq!(features.len(), 3);
        assert_eq!(features[0], Value::String("auth".to_string()));
        assert_eq!(features[1], Value::String("logging".to_string()));
        assert_eq!(features[2], Value::String("metrics".to_string()));
    } else {
        panic!("Expected 'features' to be an array");
    }

    // Check cache object (from object literal)
    if let Some(Value::Object(cache_obj)) = table.0.get("cache") {
        assert_eq!(cache_obj.get("enabled"), Some(&Value::Boolean(true)));
        assert_eq!(cache_obj.get("ttl"), Some(&Value::Integer(300)));
        assert_eq!(
            cache_obj.get("type"),
            Some(&Value::String("redis".to_string()))
        );
    } else {
        panic!("Expected 'cache' to be an object");
    }
}
