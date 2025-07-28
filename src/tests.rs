use crate::ser;
use serde::Serialize;

#[derive(Serialize)]
struct Config {
    name: String,
    version: String,
    debug: bool,
    app: AppConfig,
    features: Vec<String>,
    cache: CacheConfig,
}

#[derive(Serialize)]
struct AppConfig {
    port: u16,
    database: DatabaseConfig,
}

#[derive(Serialize)]
struct DatabaseConfig {
    host: String,
    pool_size: u32,
}

#[derive(Serialize)]
struct CacheConfig {
    enabled: bool,
    ttl: u32,
    #[serde(rename = "type")]
    cache_type: String,
}

#[test]
fn full_config() {
    let config = Config {
        name: "my-app".to_string(),
        version: "1.0.0".to_string(),
        debug: false,
        app: AppConfig {
            port: 8080,
            database: DatabaseConfig {
                host: "localhost".to_string(),
                pool_size: 10,
            },
        },
        features: vec![
            "auth".to_string(),
            "logging".to_string(),
            "metrics".to_string(),
        ],
        cache: CacheConfig {
            enabled: true,
            ttl: 300,
            cache_type: "redis".to_string(),
        },
    };

    let result = ser::to_string(&config).unwrap();
    // Basic assertions
    assert!(result.contains("name: \"my-app\""));
    assert!(result.contains("version: \"1.0.0\""));
    assert!(result.contains("debug: false"));
    assert!(result.contains("features: [auth, logging, metrics]"));
}
