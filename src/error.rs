use serde::{de, ser};
use std::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("serialization error: {0}")]
    Serialization(String),

    #[error("deserialization error: {0}")]
    Deserialization(String),

    #[error("invalid type: {message}")]
    InvalidType { message: String },

    #[error("invalid value for {typ}: {value}")]
    InvalidValue { typ: String, value: String },

    #[error("unsupported feature: {0}")]
    UnsupportedFeature(String),

    #[error("missing field: {0}")]
    MissingField(String),

    #[error("custom error: {0}")]
    Custom(String),
}

impl ser::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::Custom(msg.to_string())
    }
}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::Custom(msg.to_string())
    }
}

pub type Result<T> = std::result::Result<T, Error>;
