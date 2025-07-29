use std::fs;

use devconf_rs::{de::from_str, ser};

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
    #[serde(rename = "user")]
    user: Service,
    #[serde(rename = "order")]
    order: Service,
    #[serde(rename = "notification")]
    notification: Service,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct Service {
    port: u32,
    database: Option<String>,
    health_check: String,
    metrics: String,
    cache: Option<Cache>,
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

    let serialized = ser::to_string(&config).unwrap();

    println!("{serialized}");
}
