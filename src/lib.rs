//! This crate provides handy implementations for some design pattern utilities.
//!
//! # Features
//!
//! - `Builder`derive macro for appyling Builder pattern by generating extra Builder struct.
//! - Macro `observer` is for trait's and is for implementing Observer pattern by creating extra `Publisher` struct for the spesified instances whom implement this trait type.
//!
//! # Example
//!
//! ```rust
//! use patternutils::Builder;
//!
//! #[derive(Builder)]
//! #[builder_attr(name = "CommandCreator", opt_in)]
//! struct Command {
//!     #[builder_field(name = "value", include = true)]
//!     definition: String,
//!     
//!     #[builder_field(include = true)]
//!     arg_count: usize,
//! 
//!     accepted_flag_fallback: fn(val: usize) -> usize,
//!     childs: Vec<Command>,
//! }
//! 
//! let mut builder = Command::builder();
//! let command = builder
//!     .value(String::from("my-command"))
//!     .arg_count(3)
//!     .build(|val| {return val + 5;}, vec![]);
//! 
//! assert_eq!(command.definition, String::from("my-command"));
//! assert_eq!(command.arg_count, 3);
//! assert_eq!((command.accepted_flag_fallback)(67), 72);
//! assert_eq!(command.childs.len(), 0);
//! ```
pub use patternutils_derive::Builder;
pub use patternutils_derive::observer;