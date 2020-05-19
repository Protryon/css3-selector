/// https://www.w3.org/TR/2018/REC-selectors-3-20181106/#grammar

pub trait Node<'a> {}

pub struct SelectorsGroup<'a> {
    pub selectors: Vec<Selector<'a>>,
}

impl<'a> Node<'a> for SelectorsGroup<'a> {}

pub struct Selector<'a> {
    pub base: SelectorSequence<'a>,
    pub modifiers: Vec<(Combinator, SelectorSequence<'a>)>,
}

impl<'a> Node<'a> for Selector<'a> {}

pub enum Combinator {
    Plus,
    Greater,
    Tilde,
    None,
}

pub struct SelectorSequence<'a> {
    pub type_selector: Option<TypeSelector<'a>>,
    pub attribute_selectors: Vec<AttributeSelector<'a>>,
}

impl<'a> Node<'a> for SelectorSequence<'a> {}

pub enum AttributeSelector<'a> {
    Hash(&'a str),
    Class(&'a str),
    Attribute(Attribute<'a>),
    Psuedo(Psuedo<'a>),
    Negation(Negation<'a>),
}

impl<'a> Node<'a> for AttributeSelector<'a> {}

pub enum Namespace<'a> {
    All,
    Ident(&'a str),
}

impl<'a> Node<'a> for Namespace<'a> {}

pub struct TypeSelector<'a> {
    pub namespace: Option<Namespace<'a>>,
    pub element_name: Option<&'a str>,
}

impl<'a> Node<'a> for TypeSelector<'a> {}

pub enum Matcher {
    Prefix,
    Suffix,
    Substring,
    Equal,
    Includes,
    Dash,
}

pub struct Attribute<'a> {
    pub namespace: Option<Namespace<'a>>,
    pub name: &'a str,
    pub matcher: Option<Matcher>,
    pub value: Option<&'a str>,
}

impl<'a> Node<'a> for Attribute<'a> {}

pub struct Psuedo<'a> {
    pub is_class_type: bool, // if true, only one colon was used
    pub name: &'a str,
    pub arg: Option<Expression<'a>>, // if Some, then was a function
}

impl<'a> Node<'a> for Psuedo<'a> {}

pub struct Expression<'a> {
    pub items: Vec<ExpressionItem<'a>>,
}

impl<'a> Node<'a> for Expression<'a> {}

pub enum ExpressionItem<'a> {
    Plus,
    Minus,
    Dimension(&'a str, &'a str),
    Number(&'a str),
    Str(&'a str),
    Ident(&'a str),
}

impl<'a> Node<'a> for ExpressionItem<'a> {}

pub enum Negation<'a> {
    TypeSelector(TypeSelector<'a>),
    Hash(&'a str),
    Class(&'a str),
    Attribute(Attribute<'a>),
    Psuedo(Psuedo<'a>),
}

impl<'a> Node<'a> for Negation<'a> {}
