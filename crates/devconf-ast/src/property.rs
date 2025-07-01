use super::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub key: String,
    pub value: Value,
}
