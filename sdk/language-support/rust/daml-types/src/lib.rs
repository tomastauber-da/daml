// Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
#[cfg(feature = "proto")]
pub use daml_proto_rs;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;

#[cfg(feature = "proto")]
pub trait ToDamlProto: Data {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value;

    fn to_proto_record(&self) -> Option<daml_proto_rs::com::daml::ledger::api::v2::Record> {
        if let daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(daml_proto_rs::com::daml::ledger::api::v2::value::Sum::Record(r)),
        } = self.to_proto()
        {
            Some(r)
        } else {
            None
        }
    }
}

// ==============================================================================
// 1. Primitive Types (Daml Compat)
// ==============================================================================

/// Counterpart to Daml's `Unit`. Serializes to `{}` (empty object).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Unit {}

#[cfg(feature = "proto")]
impl ToDamlProto for Unit {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value {
        daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(daml_proto_rs::com::daml::ledger::api::v2::value::Sum::Unit(
                (),
            )),
        }
    }
}

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

#[cfg(feature = "proto")]
impl ToDamlProto for Party {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value {
        daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(daml_proto_rs::com::daml::ledger::api::v2::value::Sum::Party(self.0.clone())),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DamlInt(pub i64);

#[cfg(feature = "proto")]
impl ToDamlProto for DamlInt {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value {
        daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(daml_proto_rs::com::daml::ledger::api::v2::value::Sum::Int64(self.0)),
        }
    }
}

pub type Int = DamlInt;

/// Daml `Int` and `Numeric`. Represented as Strings to avoid precision loss.
/// You might use crates like `rust_decimal` or `bigdecimal` here in a real app,
/// wrapped to ensure they serialize to Strings.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DamlNumeric(pub String);

#[cfg(feature = "proto")]
impl ToDamlProto for DamlNumeric {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value {
        daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(
                daml_proto_rs::com::daml::ledger::api::v2::value::Sum::Numeric(self.0.clone()),
            ),
        }
    }
}

pub type Numeric = DamlNumeric;

pub type DamlDecimal = DamlNumeric;
pub type DamlText = String;
pub type DamlDate = String; // format YYYY-MM-DD
pub type DamlTime = String; // ISO 8601
pub type Time = DamlTime;
pub type Date = DamlDate;

pub trait Data:
    Clone + std::fmt::Debug + PartialEq + serde::Serialize + serde::de::DeserializeOwned
{
}
impl<T: Clone + std::fmt::Debug + PartialEq + serde::Serialize + serde::de::DeserializeOwned> Data
    for T
{
}

/// Daml `ContractId T`.
/// Uses PhantomData to prevent mixing IDs of different templates.
#[derive(PartialEq, Eq, Hash)]
pub struct ContractId<T: ?Sized>(pub String, PhantomData<T>);

#[cfg(feature = "proto")]
impl<T: ToDamlProto> ToDamlProto for ContractId<T> {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value {
        daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(
                daml_proto_rs::com::daml::ledger::api::v2::value::Sum::ContractId(self.0.clone()),
            ),
        }
    }
}

impl<T> ContractId<T> {
    /// Casts this ContractId to another type (e.g. an Interface).
    /// This is a phantom cast; strictly safe at runtime (it's just a string),
    /// but validity depends on the Daml model.
    pub fn cast<U>(self) -> ContractId<U> {
        ContractId(self.0, std::marker::PhantomData)
    }
}

impl<T> Clone for ContractId<T> {
    fn clone(&self) -> Self {
        ContractId(self.0.clone(), PhantomData)
    }
}

impl<T: ?Sized> std::fmt::Debug for ContractId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // We only print the ID string, ignoring T
        f.debug_tuple("ContractId").field(&self.0).finish()
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

#[cfg(feature = "proto")]
impl<T: ToDamlProto> ToDamlProto for TextMap<T> {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value {
        daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(
                daml_proto_rs::com::daml::ledger::api::v2::value::Sum::TextMap(
                    daml_proto_rs::com::daml::ledger::api::v2::TextMap {
                        entries: self
                            .iter()
                            .map(
                                |x| daml_proto_rs::com::daml::ledger::api::v2::text_map::Entry {
                                    key: x.0.clone(),
                                    value: Some(x.1.to_proto()),
                                },
                            )
                            .collect(),
                    },
                ),
            ),
        }
    }
}

/// Daml `Map` (GenMap).
/// Serializes to `[[k, v], [k, v]]` (Array of entries), not a JSON object.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenMap<K, V>(pub Vec<(K, V)>);

#[cfg(feature = "proto")]
impl<K: ToDamlProto, V: ToDamlProto> ToDamlProto for GenMap<K, V> {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value {
        daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(
                daml_proto_rs::com::daml::ledger::api::v2::value::Sum::GenMap(
                    daml_proto_rs::com::daml::ledger::api::v2::GenMap {
                        entries: self
                            .0
                            .iter()
                            .map(
                                |x| daml_proto_rs::com::daml::ledger::api::v2::gen_map::Entry {
                                    key: Some(x.0.to_proto()),
                                    value: Some(x.1.to_proto()),
                                },
                            )
                            .collect(),
                    },
                ),
            ),
        }
    }
}

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

// Implementation for Option<T>
#[cfg(feature = "proto")]
impl<T: ToDamlProto> ToDamlProto for Option<T> {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value {
        daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(
                daml_proto_rs::com::daml::ledger::api::v2::value::Sum::Optional(Box::new(
                    daml_proto_rs::com::daml::ledger::api::v2::Optional {
                        value: self.as_ref().map(|v| Box::new(v.to_proto())),
                    },
                )),
            ),
        }
    }
}

// Implementation for Vec<T> (Daml List)
#[cfg(feature = "proto")]
impl<T: ToDamlProto> ToDamlProto for Vec<T> {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value {
        daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(daml_proto_rs::com::daml::ledger::api::v2::value::Sum::List(
                daml_proto_rs::com::daml::ledger::api::v2::List {
                    elements: self.iter().map(|v| v.to_proto()).collect(),
                },
            )),
        }
    }
}

// Implementation for String (Daml Text)
#[cfg(feature = "proto")]
impl ToDamlProto for String {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value {
        daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(daml_proto_rs::com::daml::ledger::api::v2::value::Sum::Text(
                self.clone(),
            )),
        }
    }
}

// Implementation for bool
#[cfg(feature = "proto")]
impl ToDamlProto for bool {
    fn to_proto(&self) -> daml_proto_rs::com::daml::ledger::api::v2::Value {
        daml_proto_rs::com::daml::ledger::api::v2::Value {
            sum: Some(daml_proto_rs::com::daml::ledger::api::v2::value::Sum::Bool(
                *self,
            )),
        }
    }
}

// ==============================================================================
// 2. Traits (Replacing Companion Objects)
// ==============================================================================

pub trait DamlType {
    /// When representing the Daml package-name, the encoding is of form `#<package-name>`
    /// where `#` (not a valid package-id character)
    fn type_id() -> &'static str;

    fn package_id() -> &'static str;

    fn package_name() -> &'static str;
    /// The dot-separated module name of the identifier.
    fn module_name() -> &'static str;
    /// The dot-separated name of the entity (e.g. record, template, ...) within the module.
    fn entity_name() -> &'static str;

    #[cfg(feature = "proto")]
    fn to_proto_id() -> daml_proto_rs::com::daml::ledger::api::v2::Identifier {
        daml_proto_rs::com::daml::ledger::api::v2::Identifier {
            package_id: format!("#{}", Self::package_name()),
            module_name: Self::module_name().to_string(),
            entity_name: Self::entity_name().to_string(),
        }
    }
}

/// A trait representing a Daml Template.
/// T corresponds to the payload struct.
pub trait Template: DamlType {
    type Key;
    fn template_id() -> &'static str {
        Self::type_id()
    }
}

/// Trait implemented by generated Marker Structs for Interfaces
pub trait Interface: DamlType {
    type View;
    fn interface_id() -> &'static str {
        Self::type_id()
    }
}

/// A trait representing a Choice on a Template.
pub trait Choice<T: DamlType>: Serialize + DeserializeOwned {
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
    impl DamlType for Iou {
        fn type_id() -> &'static str {
            "#test:Main:Iou"
        }

        fn package_id() -> &'static str {
            "d14e08"
        }

        fn package_name() -> &'static str {
            "test"
        }

        fn module_name() -> &'static str {
            "Main"
        }

        fn entity_name() -> &'static str {
            "Iou"
        }
    }
    impl Template for Iou {
        type Key = ();
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
        entries.push((DamlInt(1), "One".to_string()));
        entries.push((DamlInt(2), "Two".to_string()));
        let map = GenMap(entries);

        let json = serde_json::to_string(&map).unwrap();
        // Should be [[k,v], [k,v]]
        assert_eq!(json, r#"[[1,"One"],[2,"Two"]]"#);
    }

    #[test]
    fn test_contract_id() {
        let cid: ContractId<Iou> = ContractId("cid-123".to_string(), PhantomData);
        let json = serde_json::to_string(&cid).unwrap();
        assert_eq!(json, r#""cid-123""#);
    }
}
