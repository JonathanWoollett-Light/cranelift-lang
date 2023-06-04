#![feature(iter_intersperse)]
#![feature(linked_list_cursors)]
#![warn(clippy::pedantic)]

/// The frontend convert from the text to the abstract syntax tree, this is a singly-linked graph
pub mod frontend;
/// The middle-end does compile time analysis, to do this extensively it needs to extensively explore
/// the abstract syntax tree, this requires that child nodes link to their parents, so we need to
/// convert the abstract syntax tree to be doubly-linked.
pub mod middleend;
