// Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;

// ==============================================================================
// 1. Primitive Types (Daml Compat)
// ==============================================================================

/// Counterpart to Daml's `Unit`. Serializes to `{}` (empty object).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Unit {}

impl Serialize for Unit {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use serde::ser::SerializeStruct;
        let s = serializer.serialize_struct("Unit", 0)?;
        s.end()
    }
}

impl<'de> Deserialize<'de> for Unit {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use serde::de::{MapAccess, Visitor};
        struct UnitVisitor;
        impl<'de> Visitor<'de> for UnitVisitor {
            type Value = Unit;
            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("an empty object")
            }
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                // We just consume the map
                while let Some(_) = map.next_key::<String>()? {
                    let _ = map.next_value::<serde_json::Value>()?;
                }
                Ok(Unit {})
            }
        }
        deserializer.deserialize_map(UnitVisitor)
    }
}

/// Daml `Party`. Represented as a string.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Party(pub String);

/// Daml `Int` and `Numeric`. Represented as Strings to avoid precision loss.
/// You might use crates like `rust_decimal` or `bigdecimal` here in a real app,
/// wrapped to ensure they serialize to Strings.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DamlInt(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DamlNumeric(pub String);

pub type DamlDecimal = DamlNumeric;
pub type DamlText = String;
pub type DamlDate = String; // format YYYY-MM-DD
pub type DamlTime = String; // ISO 8601

/// Daml `ContractId T`.
/// Uses PhantomData to prevent mixing IDs of different templates.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ContractId<T>(pub String, PhantomData<T>);

impl<T> Clone for ContractId<T> {
    fn clone(&self) -> Self {
        ContractId(self.0.clone(), PhantomData)
    }
}

// Custom Serialize/Deserialize to treat ContractId as a raw String
impl<T> Serialize for ContractId<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.0)
    }
}

impl<'de, T> Deserialize<'de> for ContractId<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(ContractId(s, PhantomData))
    }
}

/// Daml `TextMap`. Standard JSON object.
pub type TextMap<T> = HashMap<String, T>;

/// Daml `Map` (GenMap).
/// Serializes to `[[k, v], [k, v]]` (Array of entries), not a JSON object.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenMap<K, V>(pub Vec<(K, V)>);

impl<K, V> Serialize for GenMap<K, V>
where
    K: Serialize,
    V: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Serialize as a sequence of tuples
        serializer.collect_seq(&self.0)
    }
}

impl<'de, K, V> Deserialize<'de> for GenMap<K, V>
where
    K: DeserializeOwned,
    V: DeserializeOwned,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let vec = Vec::<(K, V)>::deserialize(deserializer)?;
        Ok(GenMap(vec))
    }
}

// ==============================================================================
// 2. Traits (Replacing Companion Objects)
// ==============================================================================

/// A trait representing a Daml Template.
/// T corresponds to the payload struct.
pub trait Template: Serialize + DeserializeOwned + Sized {
    /// The unique template ID string (e.g. "PackageId:Module:Name")
    fn template_id() -> &'static str;

    /// The key type. Use `()` (Unit) if no key exists.
    type Key: Serialize + DeserializeOwned;
}

/// A trait representing a Choice on a Template.
pub trait Choice<T: Template>: Serialize + DeserializeOwned {
    /// The return type of the choice.
    type Return: DeserializeOwned;

    /// The name of the choice.
    fn name() -> &'static str;
}

// ==============================================================================
// 3. API Structures
// ==============================================================================

/// Structure matching `DisclosedContract` in the TS code.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(bound = "T: Template")]
pub struct DisclosedContract<T: Template> {
    pub contract_id: ContractId<T>,
    pub template_id: String,
    pub created_event_blob: String,
}

// ==============================================================================
// 4. Usage Example
// ==============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // --- Example Definition (e.g. codegen output) ---

    // 1. The Payload Struct
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct Iou {
        pub issuer: Party,
        pub owner: Party,
        pub amount: DamlNumeric,
    }

    // 2. Implement Template Trait
    impl Template for Iou {
        type Key = (); // No key for this example
        fn template_id() -> &'static str {
            "d14e08...:Main:Iou"
        }
    }

    // 3. Define a Choice
    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[allow(dead_code)]
    pub struct Transfer {
        pub new_owner: Party,
    }

    // 4. Implement Choice Trait
    impl Choice<Iou> for Transfer {
        type Return = ContractId<Iou>; // Returns the new CID
        fn name() -> &'static str {
            "Transfer"
        }
    }

    // --- Serialization Tests ---

    #[test]
    fn test_serialization() {
        let iou = Iou {
            issuer: Party("Alice".to_string()),
            owner: Party("Bob".to_string()),
            amount: DamlNumeric("100.00".to_string()),
        };

        let json = serde_json::to_string(&iou).unwrap();
        // verify standard JSON serialization
        assert_eq!(
            json,
            r#"{"issuer":"Alice","owner":"Bob","amount":"100.00"}"#
        );

        let decoded: Iou = serde_json::from_str(&json).unwrap();
        assert_eq!(iou, decoded);
    }

    #[test]
    fn test_genmap_serialization() {
        // Daml GenMap: key=Int, val=Text
        let mut entries = Vec::new();
        entries.push((DamlInt("1".to_string()), "One".to_string()));
        entries.push((DamlInt("2".to_string()), "Two".to_string()));
        let map = GenMap(entries);

        let json = serde_json::to_string(&map).unwrap();
        // Should be [[k,v], [k,v]]
        assert_eq!(json, r#"[["1","One"],["2","Two"]]"#);
    }

    #[test]
    fn test_contract_id() {
        let cid: ContractId<Iou> = ContractId("cid-123".to_string(), PhantomData);
        let json = serde_json::to_string(&cid).unwrap();
        assert_eq!(json, r#""cid-123""#);
    }
}
