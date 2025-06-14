# PatternUtils

This crate provides handy procedural macros for implementing common design patterns in Rust.

## Features

- **`Builder` derive macro**  
  Implements the Builder pattern by generating an associated `Builder` struct with fluent setter methods and flexible configuration options.

- **`observer` macro** *(for traits)*  
  Implements the Observer pattern by generating a `Publisher` struct for the specified trait, allowing multiple observers to subscribe to instances of types implementing that trait.

## Example

```rust
use patternutils::Builder;

#[derive(Builder)]
#[builder_attr(name = "CommandCreator", opt_in)]
struct Command {
    #[builder_field(name = "value", include = true)]
    definition: String,

    #[builder_field(include = true)]
    arg_count: usize,

    accepted_flag_fallback: fn(val: usize) -> usize,
    childs: Vec<Command>,
}

let mut builder = Command::builder();
let command = builder
    .value(String::from("my-command"))
    .arg_count(3)
    .build(|val| val + 5, vec![]);

assert_eq!(command.definition, String::from("my-command"));
assert_eq!(command.arg_count, 3);
assert_eq!((command.accepted_flag_fallback)(67), 72);
assert_eq!(command.childs.len(), 0);
