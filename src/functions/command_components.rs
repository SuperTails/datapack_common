//! This file contains various components used in minecrafts commands

use std::{
    borrow::Borrow,
    collections::BTreeMap,
    convert::{AsRef, TryFrom},
    fmt,
    rc::Rc,
};

use command_parser::{parse_command, parse_str, CommandParse};
use itertools::Itertools;
use serde_json::Deserializer;

use super::raw_text::TextComponent;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(String);

impl BlockId {
    pub fn new(s: String) -> Result<Self, String> {
        // TODO: Determine what characters are valid
        Ok(BlockId(s))
    }
}

impl AsRef<str> for BlockId {
    fn as_ref(&self) -> &str {
        self.borrow()
    }
}

impl Borrow<str> for BlockId {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl CommandParse for BlockId {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let end_idx = value
            .find(|c: char| !(c.is_alphanumeric() || c == '_' || c == ':'))
            .unwrap_or(value.len());

        let block_id = value[..end_idx].to_string();
        let rest = &value[end_idx..];

        if block_id.is_empty() {
            Err(value)
        } else {
            Ok((rest, BlockId(block_id)))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NbtPathPart {
    pub name: SNbtString,
    pub indices: Vec<i32>,
}

impl fmt::Display for NbtPathPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#}", self.name)?;
        for index in self.indices.iter() {
            write!(f, "[{}]", index)?;
        }
        Ok(())
    }
}

impl CommandParse for NbtPathPart {
    fn parse_from_command(s: &str) -> Result<(&str, Self), &str> {
        let (mut rest, name) = SNbtString::parse_from_command(s)?;

        let mut indices = Vec::new();

        while let Some(next_rest) = rest.strip_prefix('[') {
            let (next_rest, index) = i32::parse_from_command(next_rest)?;
            indices.push(index);
            rest = next_rest.strip_prefix(']').ok_or(rest)?;
        }

        let part = NbtPathPart { name, indices };
        Ok((rest, part))
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(into = "String", try_from = "&str")]
pub struct NbtPath(pub Vec<NbtPathPart>);

impl CommandParse for NbtPath {
    fn parse_from_command(s: &str) -> Result<(&str, Self), &str> {
        let mut result = Vec::new();

        let mut rest = s;

        while let Ok((next_rest, part)) = NbtPathPart::parse_from_command(rest) {
            rest = next_rest;
            result.push(part);
            if let Some(next_rest) = rest.strip_prefix('.') {
                rest = next_rest;
            } else {
                break;
            }
        }

        if result.is_empty() {
            return Err(s);
        }

        Ok((rest, NbtPath(result)))
    }
}

impl fmt::Display for NbtPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, c) in self.0.iter().enumerate() {
            write!(f, "{}", c)?;
            if i != self.0.len() - 1 {
                write!(f, ".")?;
            }
        }

        Ok(())
    }
}

impl From<NbtPath> for String {
    fn from(p: NbtPath) -> Self {
        p.to_string()
    }
}

impl<'a> TryFrom<&'a str> for NbtPath {
    type Error = &'a str;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        parse_command(s)
    }
}

// ToDo
pub type StringNbt = String;
pub type StorageId = String;
pub type Entity = String;
pub type DataPath = String;
pub type DataType = String;
pub type RelPos = String;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SNbt {
    String(SNbtString),
    List(SNbtList),
    Compound(SNbtCompound),
    Byte(i8),
    Short(i16),
    Integer(i32),
    Long(i64),
}

impl fmt::Display for SNbt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SNbt::String(s) => s.fmt(f),
            SNbt::List(s) => s.fmt(f),
            SNbt::Compound(s) => s.fmt(f),
            SNbt::Byte(s) => write!(f, "{}b", s),
            SNbt::Short(s) => write!(f, "{}s", s),
            SNbt::Integer(s) => s.fmt(f),
            SNbt::Long(s) => write!(f, "{}l", s),
        }
    }
}

fn parse_integer_from_command<T>(s: &str, suffix: char) -> Result<(&str, T), &str>
where
    T: std::str::FromStr,
{
    let end_idx = if let Some(value_neg) = s.strip_prefix('-') {
        value_neg
            .find(|c: char| !c.is_digit(10))
            .unwrap_or(value_neg.len())
            + 1
    } else {
        s.find(|c: char| !c.is_digit(10)).unwrap_or(s.len())
    };

    let (value_str, rest) = s.split_at(end_idx);
    let value = value_str.parse().map_err(|_| s)?;
    let rest = rest.strip_prefix(suffix).ok_or(s)?;
    Ok((rest, value))
}

fn parse_nbt_byte_from_command(s: &str) -> Result<(&str, i8), &str> {
    if let Ok(result) = parse_integer_from_command(s, 'b') {
        Ok(result)
    } else {
        parse_integer_from_command(s, 'B')
    }
}

fn parse_nbt_short_from_command(s: &str) -> Result<(&str, i16), &str> {
    if let Ok(result) = parse_integer_from_command(s, 's') {
        Ok(result)
    } else {
        parse_integer_from_command(s, 'S')
    }
}

fn parse_nbt_long_from_command(s: &str) -> Result<(&str, i64), &str> {
    if let Ok(result) = parse_integer_from_command(s, 'l') {
        Ok(result)
    } else {
        parse_integer_from_command(s, 'L')
    }
}

impl CommandParse for SNbt {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        if value.starts_with('{') {
            let (rest, compound) = SNbtCompound::parse_from_command(value)?;
            Ok((rest, compound.into()))
        } else if let Ok((rest, v)) = parse_nbt_byte_from_command(value) {
            Ok((rest, v.into()))
        } else if let Ok((rest, v)) = parse_nbt_short_from_command(value) {
            Ok((rest, v.into()))
        } else if let Ok((rest, v)) = parse_nbt_long_from_command(value) {
            Ok((rest, v.into()))
        } else if let Ok((rest, v)) = i32::parse_from_command(value) {
            Ok((rest, v.into()))
        } else if value.starts_with('[') {
            let (rest, list) = SNbtList::parse_from_command(value)?;
            Ok((rest, list.into()))
        } else {
            let (rest, string) = SNbtString::parse_from_command(value)?;
            Ok((rest, string.into()))
        }
    }
}

impl From<SNbtString> for SNbt {
    fn from(s: SNbtString) -> Self {
        Self::String(s)
    }
}

impl From<SNbtCompound> for SNbt {
    fn from(s: SNbtCompound) -> Self {
        Self::Compound(s)
    }
}

impl From<SNbtList> for SNbt {
    fn from(s: SNbtList) -> Self {
        Self::List(s)
    }
}

impl From<i8> for SNbt {
    fn from(s: i8) -> Self {
        Self::Byte(s)
    }
}

impl From<i16> for SNbt {
    fn from(s: i16) -> Self {
        Self::Short(s)
    }
}

impl From<i32> for SNbt {
    fn from(s: i32) -> Self {
        Self::Integer(s)
    }
}

impl From<i64> for SNbt {
    fn from(s: i64) -> Self {
        Self::Long(s)
    }
}

/// Strings in SNbt can be printed two ways:
/// with quotes optional or with quotes required.
/// (Quotes are always optional during parsing).
///
/// Quotes are optional when a string is used as the name of a tag, e.g.
/// `{"foo":123}` is roundtripped as `{foo:123}`
/// `{"foo bar":123}` is roundtripped as `{"foo bar": 123}`
///
/// Strings used as values always have quotes.
///
/// The Display impl will print them as quoted strings by default,
/// but specifying the alternate mode `"{:#}"` will use quotes-optional mode
///
/// Note that quotes are always optional during parsing!
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct SNbtString(pub String);

impl SNbtString {
    pub fn needs_quotes(&self) -> bool {
        self.0.is_empty()
            || !self
                .0
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.')
    }
}

impl Borrow<str> for SNbtString {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl From<SNbtString> for String {
    fn from(s: SNbtString) -> Self {
        s.0
    }
}

/// See the doc comment for `SNbtString` for an explanation
impl fmt::Display for SNbtString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() && !self.needs_quotes() {
            write!(f, "{}", self.0)
        } else {
            // TODO: Minecraft will switch to using single quotes
            // if the string contains a quote to avoid needing an escape

            write!(f, "\"")?;
            for c in self.0.escape_default() {
                write!(f, "{}", c)?;
            }
            write!(f, "\"")
        }
    }
}

impl CommandParse for SNbtString {
    fn parse_from_command(s: &str) -> Result<(&str, Self), &str> {
        // TODO: Implement single-quoted strings
        if let Some(rest) = s.strip_prefix('"') {
            // TODO: Actually implement escapes lol
            let end_idx = rest.find('"').ok_or(s)?;

            let value = SNbtString(rest[..end_idx].to_string());
            let rest = &rest[end_idx + 1..];

            Ok((rest, value))
        } else if let Some(rest) = s.strip_prefix('\'') {
            // TODO: Implement escapes
            let end_idx = rest.find('\'').ok_or(s)?;

            let value = SNbtString(rest[..end_idx].to_string());
            let rest = &rest[end_idx + 1..];

            Ok((rest, value))
        } else {
            let end_idx = s
                .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.'))
                .unwrap_or(s.len());

            let value = &s[..end_idx];
            if value.is_empty() {
                return Err(s);
            }

            let value = SNbtString(value.to_string());
            let rest = &s[end_idx..];
            Ok((rest, value))
        }
    }
}

/// This also uses the {:#} and {} distinction like SNbtString.
/// {:#} will print nothing if the tag is empty.
/// {} will print `{}` if the tag is empty
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct SNbtCompound(pub BTreeMap<SNbtString, SNbt>);

impl fmt::Display for SNbtCompound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() && self.0.is_empty() {
            Ok(())
        } else {
            write!(f, "{{")?;
            for (idx, (name, tag)) in self.0.iter().enumerate() {
                write!(f, "{:#}: {}", name, tag)?;
                if idx < self.0.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, "}}")
        }
    }
}

impl CommandParse for SNbtCompound {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let mut data = BTreeMap::new();

        let mut rest = value.trim_start().strip_prefix('{').ok_or(value)?;

        while let Ok((next_rest, name)) = SNbtString::parse_from_command(rest) {
            rest = next_rest.trim_start().strip_prefix(':').ok_or(value)?;
            rest = rest.trim_start();
            let (next_rest, tag) = SNbt::parse_from_command(rest)?;
            rest = next_rest.trim_start();
            data.insert(name, tag);

            if let Some(next_rest) = rest.strip_prefix(',') {
                rest = next_rest.trim_start();
            } else {
                break;
            }
        }
        rest = rest.trim_start().strip_prefix('}').ok_or(value)?;

        Ok((rest, SNbtCompound(data)))
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct SNbtList(pub Vec<SNbt>);

impl fmt::Display for SNbtList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (idx, elem) in self.0.iter().enumerate() {
            write!(f, "{}", elem)?;
            if idx != self.0.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}

impl CommandParse for SNbtList {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let mut rest = value.strip_prefix('[').ok_or(value)?;
        rest = rest.trim_start();

        let mut elems = Vec::new();

        if !rest.starts_with(']') {
            loop {
                let (next_rest, elem) = SNbt::parse_from_command(rest)?;
                elems.push(elem);
                rest = next_rest.trim_start();
                if let Some(next_rest) = rest.strip_prefix(',') {
                    rest = next_rest.trim_start();
                } else {
                    break;
                }
            }
        }

        rest = rest.strip_prefix(']').ok_or(value)?;
        Ok((rest, SNbtList(elems)))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CommentMessage(String);

impl fmt::Display for CommentMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl CommandParse for CommentMessage {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        if !value.starts_with('#') {
            return Err(value);
        }
        let value = &value[1..];
        let new_line_idx = value.find('\n').unwrap_or(value.len());
        let (new_line, rest) = value.split_at(new_line_idx);
        Ok((rest, CommentMessage(new_line.to_string())))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct JsonText {
    pub components: Vec<TextComponent>,
}

impl fmt::Display for JsonText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.components.as_slice() {
            [single] => write!(f, "{}", serde_json::to_string(single).unwrap()),
            other => write!(f, "{}", serde_json::to_string(other).unwrap()),
        }
    }
}

impl CommandParse for JsonText {
    fn parse_from_command(mut value: &str) -> Result<(&str, Self), &str> {
        value = value.trim_start();

        let (rest, components) = if value.starts_with('[') {
            let mut stream = Deserializer::from_str(value).into_iter::<Vec<TextComponent>>();
            let components = stream.next().ok_or(value)?.map_err(|_| value)?;
            let rest = &value[stream.byte_offset()..];
            (rest, components)
        } else {
            let mut stream = Deserializer::from_str(value).into_iter::<TextComponent>();
            let component = stream.next().ok_or(value)?.map_err(|_| value)?;
            let rest = &value[stream.byte_offset()..];
            (rest, vec![component])
        };

        let result = JsonText { components };

        Ok((rest, result))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionIdent {
    /// The path of this function, for example `foo/bar/baz`
    pub path: String,
    /// The namespace of this function, for example `debris`
    pub namespace: String,
}

impl fmt::Display for FunctionIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.namespace, self.path)
    }
}

impl CommandParse for FunctionIdent {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (namespace, rest) = value.split_once(':').ok_or(value)?;
        let namespace = parse_command(namespace)?;
        let (rest, path) = String::parse_from_command(rest)?;
        Ok((rest, FunctionIdent { path, namespace }))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, serde::Deserialize, serde::Serialize)]
pub enum CoordKind {
    Absolute,
    Relative,
    Local,
}

impl fmt::Display for CoordKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CoordKind::Absolute => "",
                CoordKind::Relative => "~",
                CoordKind::Local => "^",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, serde::Deserialize, serde::Serialize)]
pub struct Coord {
    pub value: i32,
    pub kind: CoordKind,
}

impl fmt::Display for Coord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.kind, &self.value) {
            (CoordKind::Local | CoordKind::Relative, 0) => write!(f, "{}", self.kind),
            _ => write!(f, "{}{}", self.kind, self.value),
        }
    }
}

impl CommandParse for Coord {
    fn parse_from_command(mut value: &str) -> Result<(&str, Self), &str> {
        value = value.trim_start();
        let mut chars = value.chars();
        let first_char = chars.next().ok_or(value)?;
        let (mode, value) = match first_char {
            '~' => (CoordKind::Relative, chars.as_str()),
            '^' => (CoordKind::Local, chars.as_str()),
            _ => (CoordKind::Absolute, value),
        };

        let (rest, value) = if mode == CoordKind::Absolute {
            i32::parse_from_command(value)?
        } else if let Ok((rest, value)) = i32::parse_from_command(value) {
            (rest, value)
        } else {
            (value, 0)
        };

        Ok((rest.trim_start(), Coord { kind: mode, value }))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, serde::Deserialize, serde::Serialize)]
pub struct RelBlockPos {
    pub x: Coord,
    pub y: Coord,
    pub z: Coord,
}

impl fmt::Display for RelBlockPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.x, self.y, self.z)
    }
}

impl CommandParse for RelBlockPos {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, x) = Coord::parse_from_command(value)?;
        let (rest, y) = Coord::parse_from_command(rest)?;
        let (rest, z) = Coord::parse_from_command(rest)?;
        Ok((rest, RelBlockPos { x, y, z }))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct BlockState(BTreeMap<String, String>);

impl BlockState {
    pub fn get(&self, key: &str) -> Option<&str> {
        self.0.get(key).map(|v| v.as_str())
    }

    pub fn items(&self) -> impl Iterator<Item = (&str, &str)> {
        self.0.iter().map(|(k, v)| (k.as_str(), v.as_str()))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl fmt::Display for BlockState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }

        write!(f, "[")?;

        for (idx, (lhs, rhs)) in self.0.iter().enumerate() {
            write!(f, "{}={}", lhs, rhs)?;

            if idx != self.0.len() - 1 {
                write!(f, ",")?;
            }
        }

        write!(f, "]")
    }
}

impl CommandParse for BlockState {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let value = if let Some(rest) = value.strip_prefix('[') {
            rest
        } else {
            return Ok((value, BlockState::default()));
        };

        let end_idx = value.find(']').ok_or(value)?;
        let rest = &value[end_idx + 1..];
        let value = &value[..end_idx];

        let states = value
            .split(',')
            .map(|pair| {
                let (left, right) = pair.split_once('=').ok_or(value)?;
                Ok((left.trim().to_string(), right.trim().to_string()))
            })
            .collect::<Result<_, &str>>()?;
        Ok((rest, BlockState(states)))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockSpec {
    pub id: BlockId,
    pub state: BlockState,
    pub nbt: SNbtCompound,
}

impl fmt::Display for BlockSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{:#}", self.id, self.state, self.nbt)
    }
}

impl CommandParse for BlockSpec {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, id) = BlockId::parse_from_command(value)?;

        let (rest, state) = match BlockState::parse_from_command(rest) {
            Ok(val) => val,
            Err(_) => (rest, BlockState::default()),
        };
        let (rest, nbt) = match SNbtCompound::parse_from_command(rest) {
            Ok(val) => val,
            Err(_) => (rest, SNbtCompound::default()),
        };

        Ok((rest.trim_start(), BlockSpec { id, state, nbt }))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FillBlockKind {
    Replace,
    ReplaceFilter(BlockSpec),
    Destroy,
    Keep,
    Hollow,
    Outline,
}

impl fmt::Display for FillBlockKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FillBlockKind::Replace => write!(f, "replace"),
            FillBlockKind::ReplaceFilter(filter) => write!(f, "replace {}", filter),
            FillBlockKind::Destroy => write!(f, "destroy"),
            FillBlockKind::Keep => write!(f, "keep"),
            FillBlockKind::Hollow => write!(f, "hollow"),
            FillBlockKind::Outline => write!(f, "outline",),
        }
    }
}

impl CommandParse for FillBlockKind {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, word) = parse_str(value.trim_start());
        match word {
            "replace" => match BlockSpec::parse_from_command(rest) {
                Ok((rest, spec)) => Ok((rest, FillBlockKind::ReplaceFilter(spec))),
                Err(_) => Ok((rest, FillBlockKind::Replace)),
            },
            "destroy" => Ok((rest, FillBlockKind::Destroy)),
            "keep" => Ok((rest, FillBlockKind::Keep)),
            "hollow" => Ok((rest, FillBlockKind::Hollow)),
            "outline" => Ok((rest, FillBlockKind::Outline)),
            _ => Err(value),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
pub enum SetBlockKind {
    Destroy,
    Keep,
    Replace,
}

impl fmt::Display for SetBlockKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SetBlockKind::Replace => write!(f, "replace"),
            SetBlockKind::Destroy => write!(f, "destroy"),
            SetBlockKind::Keep => write!(f, "keep"),
        }
    }
}

impl CommandParse for SetBlockKind {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, word) = parse_str(value.trim_start());
        match word {
            "replace" => Ok((rest, SetBlockKind::Replace)),
            "destroy" => Ok((rest, SetBlockKind::Destroy)),
            "keep" => Ok((rest, SetBlockKind::Keep)),
            _ => Err(value),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScoreOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Assign,
    Min,
    Max,
    Swap,
}

impl fmt::Display for ScoreOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScoreOpKind::Add => write!(f, "+="),
            ScoreOpKind::Sub => write!(f, "-="),
            ScoreOpKind::Mul => write!(f, "*="),
            ScoreOpKind::Div => write!(f, "/="),
            ScoreOpKind::Mod => write!(f, "%="),
            ScoreOpKind::Assign => write!(f, "="),
            ScoreOpKind::Min => write!(f, "<"),
            ScoreOpKind::Max => write!(f, ">"),
            ScoreOpKind::Swap => write!(f, "><"),
        }
    }
}

impl CommandParse for ScoreOpKind {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, value) = parse_str(value);
        let kind = match value {
            "+=" => Self::Add,
            "-=" => Self::Sub,
            "*=" => Self::Mul,
            "/=" => Self::Div,
            "%=" => Self::Mod,
            "=" => Self::Assign,
            "<" => Self::Min,
            ">" => Self::Max,
            "><" => Self::Swap,
            _ => return Err(rest),
        };

        Ok((rest, kind))
    }
}

/// Any comparison that can be executed on two scoreboard values
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum ScoreboardComparison {
    Equal,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
}

impl ScoreboardComparison {
    /// Flips the comparison (converts OP such that `a OP b == b OP.flip_sides() a`)
    pub fn flip_sides(self) -> ScoreboardComparison {
        use ScoreboardComparison::*;
        match self {
            Equal => Equal,
            Greater => Less,
            GreaterOrEqual => LessOrEqual,
            Less => Greater,
            LessOrEqual => GreaterOrEqual,
        }
    }

    pub fn evaluate(self, lhs: i32, rhs: i32) -> bool {
        use ScoreboardComparison::*;
        match self {
            Equal => lhs == rhs,
            Greater => lhs > rhs,
            GreaterOrEqual => lhs >= rhs,
            Less => lhs < rhs,
            LessOrEqual => lhs <= rhs,
        }
    }
}

impl CommandParse for ScoreboardComparison {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, word) = parse_str(value);
        let comparison = match word {
            "=" => ScoreboardComparison::Equal,
            "<" => ScoreboardComparison::Less,
            ">" => ScoreboardComparison::Greater,
            "<=" => ScoreboardComparison::LessOrEqual,
            ">=" => ScoreboardComparison::GreaterOrEqual,
            _ => return Err(value),
        };
        Ok((rest, comparison))
    }
}

impl fmt::Display for ScoreboardComparison {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScoreboardComparison::Equal => write!(f, "="),
            ScoreboardComparison::Less => write!(f, "<"),
            ScoreboardComparison::LessOrEqual => write!(f, "<="),
            ScoreboardComparison::Greater => write!(f, ">"),
            ScoreboardComparison::GreaterOrEqual => write!(f, ">="),
        }
    }
}

/// Any valid minecraft range
///
/// A minecraft range is inclusive on both ends
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MinecraftRange {
    /// A full range, eg. 1..99
    Range { from: i32, to: i32 },
    /// A range with a lower bound, eg. 0..
    Minimum(i32),
    /// A range with an upper bound, eg. ..50
    Maximum(i32),
    /// A range that only contains one value
    Equal(i32),
}

impl MinecraftRange {
    pub fn contains(self, value: i32) -> bool {
        match self {
            MinecraftRange::Range { from, to } => (from..=to).contains(&value),
            MinecraftRange::Minimum(from) => from <= value,
            MinecraftRange::Maximum(to) => value <= to,
            MinecraftRange::Equal(v) => value == v,
        }
    }
}

impl fmt::Display for MinecraftRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MinecraftRange::Range { from, to } => write!(f, "{}..{}", from, to),
            MinecraftRange::Minimum(lo) => write!(f, "{}..", lo),
            MinecraftRange::Maximum(hi) => write!(f, "..{}", hi),
            MinecraftRange::Equal(x) => write!(f, "{}", x),
        }
    }
}

impl CommandParse for MinecraftRange {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, word) = parse_str(value);
        let range = if let Some((l, r)) = word.split_once("..") {
            let l = if l.is_empty() {
                None
            } else {
                let l = l.parse::<i32>().map_err(|_| value)?;
                Some(l)
            };

            let r = if r.is_empty() {
                None
            } else {
                let r = r.parse::<i32>().map_err(|_| value)?;
                Some(r)
            };

            match (l, r) {
                (Some(l), Some(r)) => MinecraftRange::Range { from: l, to: r },
                (Some(l), None) => MinecraftRange::Minimum(l),
                (None, Some(r)) => MinecraftRange::Maximum(r),
                (None, None) => return Err(value),
            }
        } else {
            let val = word.parse::<i32>().map_err(|_| value)?;
            MinecraftRange::Equal(val)
        };

        Ok((rest, range))
    }
}

// TODO: This needs checks on deserialization
/// Represents the name of a "player" on the scoreboard
#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, serde::Serialize, serde::Deserialize,
)]
pub struct ScoreHolder(String);

impl ScoreHolder {
    /// The maximum length of a `ScoreHolder`
    const MAX_NAME_LEN: usize = 40;

    /// Characters not allowed:
    /// All non-printing characters
    /// whitespace
    /// '*'
    /// '@' (as the first character)
    /// '"' (technically allowed, but complicates JSON)
    /// Length limit of 40 characters
    pub fn new(string: String) -> Result<Self, String> {
        if string.is_empty() {
            return Err(string);
        }

        let mut is_first = true;
        if string.contains(|c| !Self::legal(c, std::mem::replace(&mut is_first, false))) {
            return Err(string);
        }

        if string.len() > ScoreHolder::MAX_NAME_LEN {
            return Err(string);
        }

        Ok(ScoreHolder(string))
    }

    fn legal(c: char, is_first: bool) -> bool {
        match c {
            '@' if is_first => false,
            '*' | '"' => false,
            _ if c.is_whitespace() => false,
            _ if c.is_control() => false,
            _ => true,
        }
    }
}

impl AsRef<str> for ScoreHolder {
    fn as_ref(&self) -> &str {
        self.borrow()
    }
}

impl Borrow<str> for ScoreHolder {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ScoreHolder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl CommandParse for ScoreHolder {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, value) = String::parse_from_command(value)?;
        Ok((rest, ScoreHolder(value)))
    }
}

/// Any objective criterion
///
/// used in the command `scoreboard objectives add foo <criterion>`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ObjectiveCriterion {
    /// Mostly used by debris
    Dummy,
    // /// potentially generate an enum of all possibilities from minecraft data
    // Other(String),
}

impl fmt::Display for ObjectiveCriterion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ObjectiveCriterion::Dummy => "dummy",
        })
    }
}

impl CommandParse for ObjectiveCriterion {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, word) = parse_str(value);
        let criterion = match word {
            "dummy" => ObjectiveCriterion::Dummy,
            _ => return Err(value),
        };
        Ok((rest, criterion))
    }
}

// TODO: This needs checks on deserialization
/// The name of an objective on the scoreboard
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct Objective(String);

impl Objective {
    pub fn new(s: String) -> Result<Self, String> {
        // TODO: Determine what characters are valid
        Ok(Objective(s))
    }
}

impl AsRef<str> for Objective {
    fn as_ref(&self) -> &str {
        self.borrow()
    }
}

impl Borrow<str> for Objective {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Objective {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl CommandParse for Objective {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, value) = String::parse_from_command(value)?;
        Ok((rest, Objective(value)))
    }
}

/// A combination of a player and a scoreboard objective
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ScoreboardPlayer {
    pub player: Rc<ScoreHolder>,
    pub scoreboard: Rc<Objective>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SelectorVariable {
    NearestPlayer,
    RandomPlayer,
    AllPlayers,
    AllEntities,
    ThisEntity,
}

impl fmt::Display for SelectorVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            SelectorVariable::NearestPlayer => "@p",
            SelectorVariable::AllPlayers => "@a",
            SelectorVariable::RandomPlayer => "@r",
            SelectorVariable::ThisEntity => "@s",
            SelectorVariable::AllEntities => "@e",
        };
        write!(f, "{}", value)
    }
}

impl CommandParse for SelectorVariable {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        if let Some(rest) = value.strip_prefix("@p") {
            Ok((rest, SelectorVariable::NearestPlayer))
        } else if let Some(rest) = value.strip_prefix("@a") {
            Ok((rest, SelectorVariable::AllPlayers))
        } else if let Some(rest) = value.strip_prefix("@r") {
            Ok((rest, SelectorVariable::RandomPlayer))
        } else if let Some(rest) = value.strip_prefix("@s") {
            Ok((rest, SelectorVariable::ThisEntity))
        } else if let Some(rest) = value.strip_prefix("@e") {
            Ok((rest, SelectorVariable::AllEntities))
        } else {
            Err(value)
        }
    }
}

// TODO: These support a much more limit set of characters than a scoreboard objective
// TODO: This should be an enum, probably
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SelectorArg(pub String);

impl fmt::Display for SelectorArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl CommandParse for SelectorArg {
    fn parse_from_command(s: &str) -> Result<(&str, Self), &str> {
        let comma_end = s.find(',');
        let bracket_end = s.find(']');

        let end_idx = match (comma_end, bracket_end) {
            (Some(c), Some(b)) => c.min(b),
            (Some(c), None) => c,
            (None, Some(b)) => b,
            (None, None) => return Err(s),
        };

        let (arg, rest) = s.split_at(end_idx);

        Ok((rest, SelectorArg(arg.trim().to_string())))
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(into = "String", try_from = "&str")]
pub struct Selector {
    pub var: SelectorVariable,
    pub args: Vec<SelectorArg>,
}

impl From<Selector> for String {
    fn from(value: Selector) -> Self {
        value.to_string()
    }
}

impl TryFrom<&str> for Selector {
    type Error = String;

    fn try_from(_: &str) -> Result<Self, Self::Error> {
        todo!("Why do we need deserialize?")
    }
}

impl fmt::Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.var)?;
        let args = self.args.iter().map(|arg| arg.to_string()).join(", ");
        if !args.is_empty() {
            write!(f, "[{}]", args)?;
        }
        Ok(())
    }
}

impl CommandParse for Selector {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, selector_var) = SelectorVariable::parse_from_command(value)?;
        let mut rest = if let Some(rest) = rest.strip_prefix('[') {
            rest
        } else {
            return Ok((
                rest,
                Selector {
                    args: Default::default(),
                    var: selector_var,
                },
            ));
        };

        let mut args = Vec::new();
        loop {
            if rest.starts_with(']') {
                break;
            }

            let (next_rest, arg) = SelectorArg::parse_from_command(rest)?;
            rest = next_rest;
            args.push(arg);

            if let Some(next_rest) = rest.strip_prefix(',') {
                rest = next_rest.trim_start();
            }
        }

        rest = rest.strip_prefix(']').unwrap();

        Ok((
            rest,
            Selector {
                args,
                var: selector_var,
            },
        ))
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(into = "String", try_from = "&str")]
pub enum Target {
    Selector(Selector),
    Name(ScoreHolder),
}

impl From<Target> for String {
    fn from(value: Target) -> Self {
        value.to_string()
    }
}

impl TryFrom<&str> for Target {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        parse_command(value).map_err(String::from)
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Target::Selector(selector) => write!(f, "{}", selector),
            Target::Name(name) => write!(f, "{}", name),
        }
    }
}

impl CommandParse for Target {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (_, word) = parse_str(value);
        if word.starts_with('@') {
            let (rest, selector) = Selector::parse_from_command(value)?;
            Ok((rest.trim_start(), Target::Selector(selector)))
        } else {
            let (rest, holder) = ScoreHolder::parse_from_command(value)?;
            Ok((rest.trim_start(), Target::Name(holder)))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScoreboardTarget {
    Target(Target),
    Asterisk,
}

impl fmt::Display for ScoreboardTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScoreboardTarget::Target(target) => write!(f, "{}", target),
            ScoreboardTarget::Asterisk => write!(f, "*"),
        }
    }
}

impl CommandParse for ScoreboardTarget {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        if let Some(rest) = value.strip_prefix("* ") {
            Ok((rest, ScoreboardTarget::Asterisk))
        } else {
            let (rest, target) = Target::parse_from_command(value)?;
            Ok((rest, ScoreboardTarget::Target(target)))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataTarget {
    Block(RelBlockPos),
    Entity(Target),
    Storage(StorageId),
}

impl fmt::Display for DataTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataTarget::Block(block) => write!(f, "block {}", block),
            DataTarget::Entity(target) => write!(f, "entity {}", target),
            DataTarget::Storage(storage) => write!(f, "storage {}", storage),
        }
    }
}

impl CommandParse for DataTarget {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, word) = parse_str(value);
        match word {
            "block" => {
                let (rest, pos) = RelBlockPos::parse_from_command(rest)?;
                Ok((rest, DataTarget::Block(pos)))
            }
            "entity" => {
                let (rest, target) = Target::parse_from_command(rest)?;
                Ok((rest, DataTarget::Entity(target)))
            }
            "storage" => {
                let (rest, storage) = StorageId::parse_from_command(rest)?;
                Ok((rest, DataTarget::Storage(storage)))
            }
            _ => Err(value),
        }
    }
}

#[cfg(test)]
mod test {
    use command_parser::parse_command;

    use crate::functions::command_components::Selector;

    use super::{BlockState, Coord, NbtPath, SNbt, ScoreOpKind};

    #[test]
    fn test_coord() {
        let abs: Coord = parse_command("5").unwrap();
        let rel: Coord = parse_command("~-25").unwrap();
        let loc: Coord = parse_command("^225").unwrap();

        assert_eq!(abs.to_string(), "5");
        assert_eq!(rel.to_string(), "~-25");
        assert_eq!(loc.to_string(), "^225");
    }

    #[test]
    fn test_block_state() {
        let empty: BlockState = parse_command("").unwrap();
        let single: BlockState = parse_command("[foo=bar]").unwrap();
        let multiple: BlockState = parse_command("[foo=bar, a=b]").unwrap();

        assert_eq!(empty.to_string(), "");
        assert_eq!(single.to_string(), "[foo=bar]");
        assert_eq!(multiple.to_string(), "[a=b,foo=bar]");
    }

    #[test]
    fn test_score_op_kind() {
        let test_cases = ["+=", "-=", "*=", "/=", "%=", "=", "<", ">", "><"];

        for case in test_cases {
            let kind: ScoreOpKind = parse_command(case).unwrap();
            assert_eq!(kind.to_string(), case);
        }
    }

    #[test]
    fn test_selector() {
        let var: Selector = parse_command("@a").unwrap();
        assert_eq!(var.to_string(), "@a")

        // TODO: handle selector arguments
    }

    #[track_caller]
    fn roundtrip_snbt(input: &str) {
        let snbt = parse_command::<SNbt>(input).unwrap();
        let output = snbt.to_string();
        assert_eq!(input, output);
    }

    #[track_caller]
    fn roundtrip_path(input: &str) {
        let snbt = parse_command::<NbtPath>(input).unwrap();
        let output = snbt.to_string();
        assert_eq!(input, output);
    }

    #[test]
    fn test_snbt_integer() {
        roundtrip_snbt("123");
        roundtrip_snbt("0");
        roundtrip_snbt("-42");
        roundtrip_snbt("127b");
        roundtrip_snbt("-128b");
        roundtrip_snbt("32767s");
        roundtrip_snbt("-32768s");
        roundtrip_snbt("12339543845939439l");
    }

    #[test]
    fn test_snbt_list() {
        roundtrip_snbt("[]");
        roundtrip_snbt("[1, 2, 3]");
        roundtrip_snbt("[\"a\", \"bcd\", \"123ab\"]");
    }

    #[test]
    fn test_snbt_string() {
        roundtrip_snbt("\"\"");
        roundtrip_snbt("\"abc\"");
        roundtrip_snbt("\"123\"");
        roundtrip_snbt("\"foo 123 bar\"");
        // TODO: It escapes the apostrophe when it doesn't need to
        //roundtrip_snbt("\"apostr'phe\"");
        // TODO: See comment in SNbtString::Display
        // roundtrip_snbt("\'quo\"te\'");
        // TODO: Escapes aren't done yet
        // roundtrip_snbt("\"back\\slash\"");
    }

    #[test]
    fn test_snbt_compound() {
        roundtrip_snbt("{foo_bar123: 42}");
        roundtrip_snbt("{}");
        roundtrip_snbt("{\"foo 123\": \"bar\"}");
        roundtrip_snbt("{tricky: \"{\"}");
        roundtrip_snbt("{trickytoo: \"}\"}");
        roundtrip_snbt("{x: {inner: 42}, y: [1, 2, 3], z: \"Hello, world!\"}");
    }

    #[test]
    fn test_path() {
        roundtrip_path("foo");
        roundtrip_path("foo[123]");
        roundtrip_path("foo.bar");
        roundtrip_path("foo[123][45].bar");
        roundtrip_path("\"foo bar\"");
        roundtrip_path("\"spa ces\"[45].rest");
        roundtrip_path("lots[1].of[-5].\"ar rays\"[17]");
    }
}
