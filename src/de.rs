#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::cast_precision_loss
)]

use crate::error::{Error, Result};
use devconf_runtime::{Value, parse_from_str};
use serde::{
    Deserialize,
    de::{self, IntoDeserializer},
};
use std::collections::HashMap;

pub struct Deserializer {
    value: Value,
}

/// Deserializes a value of type `T` from a string slice.
///
/// # Errors
///
/// Returns an error if the input string cannot be parsed or deserialized into the target type.
pub fn from_str<'a, T>(s: &'a str) -> Result<T>
where
    T: Deserialize<'a>,
{
    let table = parse_from_str(s);
    let mut deserializer = Deserializer::from_table(table);
    let t = T::deserialize(&mut deserializer)?;
    Ok(t)
}

impl Deserializer {
    fn from_table(table: devconf_runtime::Table) -> Self {
        // Convert Table to Value::Object
        let map = table.0; // Access the inner HashMap
        Self {
            value: Value::Object(map),
        }
    }

    fn from_value(value: Value) -> Self {
        Self { value }
    }
}

impl<'de> de::Deserializer<'de> for &mut Deserializer {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::String(s) => visitor.visit_str(s),
            Value::Integer(i) => visitor.visit_i64(*i),
            Value::Float(f) => visitor.visit_f64(*f),
            Value::Boolean(b) => visitor.visit_bool(*b),
            Value::Null => visitor.visit_unit(),
            Value::Array(arr) => visitor.visit_seq(SeqDeserializer::new(arr.clone())),
            Value::Object(obj) => visitor.visit_map(MapDeserializer::new(obj.clone())),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Boolean(b) => visitor.visit_bool(*b),
            _ => Err(Error::InvalidValue {
                typ: "bool".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Integer(i) => visitor.visit_i8(*i as i8),
            _ => Err(Error::InvalidValue {
                typ: "i8".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Integer(i) => visitor.visit_i16(*i as i16),
            _ => Err(Error::InvalidValue {
                typ: "i16".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Integer(i) => visitor.visit_i32(*i as i32),
            _ => Err(Error::InvalidValue {
                typ: "i32".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Integer(i) => visitor.visit_i64(*i),
            _ => Err(Error::InvalidValue {
                typ: "i64".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Integer(i) => visitor.visit_u8(*i as u8),
            _ => Err(Error::InvalidValue {
                typ: "u8".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Integer(i) => visitor.visit_u16(*i as u16),
            _ => Err(Error::InvalidValue {
                typ: "u16".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Integer(i) => visitor.visit_u32(*i as u32),
            _ => Err(Error::InvalidValue {
                typ: "u32".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Integer(i) => visitor.visit_u64(*i as u64),
            _ => Err(Error::InvalidValue {
                typ: "u64".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Float(f) => visitor.visit_f32(*f as f32),
            Value::Integer(i) => visitor.visit_f32(*i as f32),
            _ => Err(Error::InvalidValue {
                typ: "f32".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Float(f) => visitor.visit_f64(*f),
            Value::Integer(i) => visitor.visit_f64(*i as f64),
            _ => Err(Error::InvalidValue {
                typ: "f64".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::String(s) if s.len() == 1 => visitor.visit_char(s.chars().next().unwrap()),
            _ => Err(Error::InvalidValue {
                typ: "char".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::String(s) => visitor.visit_str(s),
            _ => Err(Error::InvalidValue {
                typ: "str".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::String(s) => visitor.visit_string(s.clone()),
            _ => Err(Error::InvalidValue {
                typ: "string".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::String(s) => visitor.visit_bytes(s.as_bytes()),
            _ => Err(Error::UnsupportedFeature("bytes".to_string())),
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::String(s) => visitor.visit_byte_buf(s.as_bytes().to_vec()),
            _ => Err(Error::UnsupportedFeature("byte_buf".to_string())),
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Null => visitor.visit_unit(),
            _ => Err(Error::InvalidValue {
                typ: "unit".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Array(arr) => visitor.visit_seq(SeqDeserializer::new(arr.clone())),
            _ => Err(Error::InvalidValue {
                typ: "sequence".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::Object(obj) => visitor.visit_map(MapDeserializer::new(obj.clone())),
            _ => Err(Error::InvalidValue {
                typ: "map".to_string(),
                value: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match &self.value {
            Value::String(s) => visitor.visit_enum(s.as_str().into_deserializer()),
            _ => Err(Error::UnsupportedFeature("complex enums".to_string())),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }
}

// Sequence deserializer for arrays
struct SeqDeserializer {
    iter: std::vec::IntoIter<Value>,
}

impl SeqDeserializer {
    fn new(values: Vec<Value>) -> Self {
        Self {
            iter: values.into_iter(),
        }
    }
}

impl<'de> de::SeqAccess<'de> for SeqDeserializer {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.iter.next() {
            Some(value) => {
                let mut deserializer = Deserializer::from_value(value);
                seed.deserialize(&mut deserializer).map(Some)
            }
            None => Ok(None),
        }
    }
}

// Map deserializer for objects
struct MapDeserializer {
    iter: std::collections::hash_map::IntoIter<String, Value>,
    value: Option<Value>,
}

impl MapDeserializer {
    fn new(map: HashMap<String, Value>) -> Self {
        Self {
            iter: map.into_iter(),
            value: None,
        }
    }
}

impl<'de> de::MapAccess<'de> for MapDeserializer {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        match self.iter.next() {
            Some((key, value)) => {
                self.value = Some(value);
                seed.deserialize(key.into_deserializer()).map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        match self.value.take() {
            Some(value) => {
                let mut deserializer = Deserializer::from_value(value);
                seed.deserialize(&mut deserializer)
            }
            None => Err(Error::InvalidValue {
                typ: "value".to_string(),
                value: "missing value".to_string(),
            }),
        }
    }
}
