use std::fs;

use devconf_rs::de::from_str;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct Config {
    name: String,
    package: String,
    likes: u32,
    avg: f64,
    activated: bool,
    topics: Vec<String>,
    app: App,
    services: Services,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct App {
    config: bool,
    name: String,
    debug: bool,
    port: u32,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct Services {
    #[serde(rename = "user-service")]
    user_service: Service,
    #[serde(rename = "order-service")]
    order_service: Service,
    #[serde(rename = "notification-service")]
    notification_service: Service,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct Service {
    port: u32,
    health_check: String,
    #[serde(default)]
    metrics: Option<String>,
    #[serde(default)]
    database: Option<String>,
    #[serde(default)]
    cache: Option<Cache>,
    #[serde(default)]
    queue: Option<Queue>,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct Cache {
    enabled: bool,
    ttl: u32,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct Queue {
    #[serde(rename = "type")]
    queue_type: String,
    url: String,
}

fn main() {
    let path = fs::read_to_string("examples/full.devconf")
        .unwrap_or_else(|err| panic!("Cannot read because of: {err}"));

    let config: Config = from_str(&path).unwrap();

    println!("{config:#?}");
}
