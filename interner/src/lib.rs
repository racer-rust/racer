//! string interner
//! same as cargo::core::interning.rs, but thread local and Deserializable

extern crate serde;
use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::ptr;
use std::str;

fn leak(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

thread_local! {
    static STRING_CACHE: RefCell<HashSet<&'static str>> = Default::default();
}

#[derive(Clone, Copy)]
pub struct InternedString {
    inner: &'static str,
}

impl PartialEq for InternedString {
    fn eq(&self, other: &InternedString) -> bool {
        ptr::eq(self.as_str(), other.as_str())
    }
}

impl Eq for InternedString {}

impl InternedString {
    pub fn new(st: &str) -> InternedString {
        STRING_CACHE.with(|cache| {
            let mut cache = cache.borrow_mut();
            let s = cache.get(st).map(|&s| s).unwrap_or_else(|| {
                let s = leak(st.to_string());
                cache.insert(s);
                s
            });
            InternedString { inner: s }
        })
    }

    pub fn new_if_exists(st: &str) -> Option<InternedString> {
        STRING_CACHE.with(|cache| cache.borrow().get(st).map(|&s| InternedString { inner: s }))
    }

    pub fn as_str(&self) -> &'static str {
        self.inner
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &'static str {
        self.as_str()
    }
}

impl Hash for InternedString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&(self.inner as *const str), state)
    }
}

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

impl Ord for InternedString {
    fn cmp(&self, other: &InternedString) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl PartialOrd for InternedString {
    fn partial_cmp(&self, other: &InternedString) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Serialize for InternedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.inner)
    }
}

impl<'de> Deserialize<'de> for InternedString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct VisStr;
        impl<'de> Visitor<'de> for VisStr {
            type Value = InternedString;
            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "expecting string")
            }
            fn visit_borrowed_str<E: Error>(self, v: &'de str) -> Result<InternedString, E> {
                Ok(InternedString::new(v))
            }
        }
        deserializer.deserialize_str(VisStr {})
    }
}
