/// https://www.w3.org/TR/2018/REC-selectors-3-20181106/#grammar
use serde::{Deserialize, Serialize};
pub trait Node<'a> {}

#[derive(Deserialize, Serialize, Clone)]
pub struct SelectorsGroup<'a> {
    #[serde(borrow)]
    pub selectors: Vec<Selector<'a>>,
}

impl<'a> Node<'a> for SelectorsGroup<'a> {}

#[derive(Deserialize, Serialize, Clone)]
pub struct Selector<'a> {
    #[serde(borrow)]
    pub base: SelectorSequence<'a>,
    #[serde(borrow)]
    pub modifiers: Vec<(Combinator, SelectorSequence<'a>)>,
}

impl<'a> Node<'a> for Selector<'a> {}

#[derive(Deserialize, Serialize, Clone)]
#[serde(tag = "type")]
pub enum Combinator {
    Plus,
    Greater,
    Tilde,
    None,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct SelectorSequence<'a> {
    #[serde(borrow)]
    pub type_selector: Option<TypeSelector<'a>>,
    #[serde(borrow)]
    pub attribute_selectors: Vec<AttributeSelector<'a>>,
}

impl<'a> Node<'a> for SelectorSequence<'a> {}

#[derive(Deserialize, Serialize, Clone)]
#[serde(tag = "type")]
pub enum AttributeSelector<'a> {
    Hash(&'a str),
    Class(&'a str),
    Attribute(Attribute<'a>),
    Psuedo(Psuedo<'a>),
    Negation(Negation<'a>),
}

impl<'a> Node<'a> for AttributeSelector<'a> {}

#[derive(Deserialize, Serialize, Clone)]
#[serde(tag = "type")]
pub enum Namespace<'a> {
    All,
    Ident(&'a str),
}

impl<'a> Node<'a> for Namespace<'a> {}

#[derive(Deserialize, Serialize, Clone)]
pub struct TypeSelector<'a> {
    pub namespace: Option<Namespace<'a>>,
    pub element_name: Option<&'a str>,
}

impl<'a> Node<'a> for TypeSelector<'a> {}

#[derive(Deserialize, Serialize, Clone)]
pub enum Matcher {
    Prefix,
    Suffix,
    Substring,
    Equal,
    Includes,
    Dash,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct Attribute<'a> {
    pub namespace: Option<Namespace<'a>>,
    pub name: &'a str,
    pub matcher: Option<Matcher>,
    pub value: Option<&'a str>,
}

impl<'a> Node<'a> for Attribute<'a> {}

#[derive(Deserialize, Serialize, Clone)]
pub struct Psuedo<'a> {
    pub is_class_type: bool, // if true, only one colon was used
    pub name: &'a str,
    pub arg: Option<Expression<'a>>, // if Some, then was a function
}

impl<'a> Node<'a> for Psuedo<'a> {}

#[derive(Deserialize, Serialize, Clone)]
pub struct Expression<'a> {
    #[serde(borrow)]
    pub items: Vec<ExpressionItem<'a>>,
}

impl<'a> Node<'a> for Expression<'a> {}

#[derive(Deserialize, Serialize, Clone)]
pub enum ExpressionItem<'a> {
    Plus,
    Minus,
    Dimension(&'a str, &'a str),
    Number(&'a str),
    Str(&'a str),
    Ident(&'a str),
}

impl<'a> Node<'a> for ExpressionItem<'a> {}

#[derive(Deserialize, Serialize, Clone)]
#[serde(tag = "type")]
pub enum Negation<'a> {
    TypeSelector(TypeSelector<'a>),
    Hash(&'a str),
    Class(&'a str),
    Attribute(Attribute<'a>),
    Psuedo(Psuedo<'a>),
}

impl<'a> Node<'a> for Negation<'a> {}
