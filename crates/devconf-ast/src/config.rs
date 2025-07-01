use super::{Property, Value};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct DevConf {
    pub properties: Vec<Property>,
}

impl DevConf {
    pub fn add_property(&mut self, key: String, value: Value) {
        self.properties.push(Property { key, value });
    }

    pub fn get_value(&self, key: &str) -> Option<&Value> {
        self.properties
            .iter()
            .find(|p| p.key == key)
            .map(|p| &p.value)
    }
}
