use std::fs;

use devconf_rs::de::from_str;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct Config {
    app: AppConfig,
    likes: u32,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct AppConfig {
    name: String,
    port: u32,
}

fn main() {
    let path = fs::read_to_string("examples/simple.devconf")
        .unwrap_or_else(|err| panic!("Cannot read because of: {err}"));

    let config: Config = from_str(&path).unwrap();

    println!("{config:#?}");
}
