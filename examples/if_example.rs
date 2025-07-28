use std::fs;

use devconf_rs::de::from_str;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct Config {
    app: AppConfig,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct AppConfig {
    mode: String,
}

fn main() {
    let path = fs::read_to_string("examples/if_example.devconf")
        .unwrap_or_else(|err| panic!("Cannot read because of: {err}"));

    let config: Config = from_str(&path).unwrap();

    println!("{config:#?}");
}
