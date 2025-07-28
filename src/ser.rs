use crate::{error::Error, error::Result};
use serde::{Serialize, ser};

pub struct Serializer {
    output: String,
    current_key: Option<String>,
    in_array: bool,
}

/// Serializes a value into a string using the custom serializer.
///
/// # Errors
///
/// Returns an error if serialization fails.
pub fn to_string<T>(value: &T) -> Result<String>
where
    T: Serialize,
{
    let mut serializer = Serializer {
        output: String::new(),
        current_key: None,
        in_array: false,
    };

    value.serialize(&mut serializer)?;
    Ok(serializer.output.to_string())
}

impl Serializer {
    fn quote_string_if_needed(s: &str) -> String {
        // Quote if contains spaces, special characters, or starts with a number
        if s.is_empty()
            || s.contains(' ')
            || s.contains('\t')
            || s.contains('\n')
            || s.contains('"')
            || s.contains(':')
            || s.contains('{')
            || s.contains('}')
            || s.contains('[')
            || s.contains(']')
            || s.chars().next().unwrap().is_ascii_digit()
        {
            format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
        } else {
            s.to_string()
        }
    }

    fn quote_string_always(s: &str) -> String {
        format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
    }
}

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = ();
    type Error = Error;
    type SerializeSeq = SeqSerializer<'a>;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = MapSerializer<'a>;
    type SerializeStruct = StructSerializer<'a>;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, v: bool) -> Result<()> {
        self.output.push_str(if v { "true" } else { "false" });
        Ok(())
    }

    fn serialize_i8(self, v: i8) -> Result<()> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i16(self, v: i16) -> Result<()> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i32(self, v: i32) -> Result<()> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i64(self, v: i64) -> Result<()> {
        self.output.push_str(&v.to_string());
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<()> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u16(self, v: u16) -> Result<()> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u32(self, v: u32) -> Result<()> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u64(self, v: u64) -> Result<()> {
        self.output.push_str(&v.to_string());
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<()> {
        self.serialize_f64(f64::from(v))
    }

    fn serialize_f64(self, v: f64) -> Result<()> {
        self.output.push_str(&v.to_string());
        Ok(())
    }

    fn serialize_char(self, v: char) -> Result<()> {
        self.output
            .push_str(&Serializer::quote_string_if_needed(&v.to_string()));
        Ok(())
    }

    fn serialize_str(self, v: &str) -> Result<()> {
        if self.in_array {
            self.output.push_str(&Serializer::quote_string_if_needed(v));
        } else {
            self.output.push_str(&Serializer::quote_string_always(v));
        }
        Ok(())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<()> {
        self.serialize_str(&String::from_utf8_lossy(v))
    }

    fn serialize_none(self) -> Result<()> {
        self.output.push_str("null");
        Ok(())
    }

    fn serialize_some<T>(self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<()> {
        self.output.push_str("null");
        Ok(())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<()> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.output.push_str(variant);
        self.output.push('(');
        value.serialize(&mut *self)?;
        self.output.push(')');
        Ok(())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(SeqSerializer {
            serializer: self,
            first: true,
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))?;
        Ok(self)
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))?;
        Ok(self)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        self.output.push_str(variant);
        self.output.push('(');
        self.serialize_seq(Some(len))?;
        Ok(self)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(MapSerializer {
            serializer: self,
            first: true,
        })
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        Ok(StructSerializer {
            serializer: self,
            first: true,
            _prefix: String::new(),
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        self.output.push_str(variant);
        self.output.push_str(" { ");
        Ok(self)
    }
}

// Sequence serializer for arrays
pub struct SeqSerializer<'a> {
    serializer: &'a mut Serializer,
    first: bool,
}

impl ser::SerializeSeq for SeqSerializer<'_> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if self.first {
            self.serializer.output.push('[');
            self.first = false;
        } else {
            self.serializer.output.push_str(", ");
        }
        // Set the array flag before serializing the element
        self.serializer.in_array = true;
        value.serialize(&mut *self.serializer)?;
        self.serializer.in_array = false;
        Ok(())
    }

    fn end(self) -> Result<()> {
        if self.first {
            self.serializer.output.push_str("[]");
        } else {
            self.serializer.output.push(']');
        }
        Ok(())
    }
}

// Map serializer for objects
pub struct MapSerializer<'a> {
    serializer: &'a mut Serializer,
    first: bool,
}

impl ser::SerializeMap for MapSerializer<'_> {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if self.first {
            self.serializer.output.push('{');
            self.first = false;
        } else {
            self.serializer.output.push_str(", ");
        }

        // Capture the key
        let mut key_serializer = Serializer {
            output: String::new(),
            current_key: None,
            in_array: false,
        };
        key.serialize(&mut key_serializer)?;
        self.serializer.current_key = Some(key_serializer.output);
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if let Some(key) = &self.serializer.current_key.take() {
            self.serializer.output.push_str(key);
            self.serializer.output.push_str(": ");
        }
        value.serialize(&mut *self.serializer)?;
        Ok(())
    }

    fn end(self) -> Result<()> {
        if self.first {
            self.serializer.output.push_str("{}");
        } else {
            self.serializer.output.push('}');
        }
        Ok(())
    }
}

// Struct serializer for top-level structs
pub struct StructSerializer<'a> {
    serializer: &'a mut Serializer,
    first: bool,
    _prefix: String,
}

impl ser::SerializeStruct for StructSerializer<'_> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        if !self.first {
            self.serializer.output.push('\n');
        }
        self.first = false;

        // Check if this is a nested struct that should be serialized as nested properties
        let mut temp_serializer = Serializer {
            output: String::new(),
            current_key: None,
            in_array: false,
        };

        value.serialize(&mut temp_serializer)?;

        // If the value looks like a flat property list (no braces), handle it as dot notation
        if temp_serializer.output.contains('\n') && !temp_serializer.output.starts_with('{') {
            // This is a nested struct - convert to dot notation
            for line in temp_serializer.output.lines() {
                if !line.trim().is_empty() {
                    use std::fmt::Write as _;
                    write!(self.serializer.output, "{key}.{line}").unwrap();
                    self.serializer.output.push('\n');
                }
            }
            // Remove the last newline
            if self.serializer.output.ends_with('\n') {
                self.serializer.output.pop();
            }
        } else {
            // Regular property
            self.serializer.output.push_str(key);
            self.serializer.output.push_str(": ");
            self.serializer.output.push_str(&temp_serializer.output);
        }

        Ok(())
    }

    fn end(self) -> Result<()> {
        Ok(())
    }
}

// Implement dummy traits for compatibility
impl ser::SerializeTuple for &mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)?;
        Ok(())
    }

    fn end(self) -> Result<()> {
        self.output.push(']');
        Ok(())
    }
}

impl ser::SerializeTupleStruct for &mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)?;
        Ok(())
    }

    fn end(self) -> Result<()> {
        self.output.push(']');
        Ok(())
    }
}

impl ser::SerializeTupleVariant for &mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)?;
        Ok(())
    }

    fn end(self) -> Result<()> {
        self.output.push(')');
        Ok(())
    }
}

impl ser::SerializeStructVariant for &mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.output.push_str(key);
        self.output.push_str(": ");
        value.serialize(&mut **self)?;
        self.output.push_str(", ");
        Ok(())
    }

    fn end(self) -> Result<()> {
        self.output.push_str(" }");
        Ok(())
    }
}
