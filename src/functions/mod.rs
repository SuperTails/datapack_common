use std::{borrow::Borrow, convert::TryFrom, fmt::{self, Display, Formatter}, rc::Rc, str::FromStr};
use vfs::{Directory, File};

pub mod command;
pub mod raw_text;

pub use command::Command;

/// Represents a single function and its contents in a datapack
#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionIdent,
    pub cmds: Vec<Command>,
}

// TODO: This needs checks on deserialization
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, serde::Serialize, serde::Deserialize)]
/// Represents the name of a "player" on the scoreboard
pub struct ScoreHolder(String);


// TODO: This needs checks on deserialization
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
/// The name of an objective on the scoreboard
pub struct Objective(String);

impl Objective {
    pub fn new(s: String) -> Result<Self, String> {
        // TODO: Determine what characters are valid
        Ok(Objective(s))
    }
}



/// Any objective criterion
///
/// used in the command `scoreboard objectives add foo <criterion>`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectiveCriterion {
    /// Mostly used by debris
    Dummy,
    // /// potentially generate an enum of all possibilities from minecraft data
    // Other(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionIdent {
    /// The path of this function, for example `foo/bar/baz`
    pub path: String,
    /// The namespace of this function, for example `debris`
    pub namespace: Rc<str>,
}

/// A unique minecraft function identifier
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSpec {
    /// The path and namespace for this function
    pub id: FunctionIdent,
    /// Whether this function is a collection, marked by a `#`
    pub is_collection: bool,
}

/// A combination of a player and a scoreboard objective
#[derive(Debug, Clone)]
pub struct ScoreboardPlayer {
    pub player: Rc<ScoreHolder>,
    pub scoreboard: Rc<Objective>,
}

/// Any valid minecraft range
///
/// A minecraft range is inclusive on both ends
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

/// Any comparison that can be executed on two scoreboard values
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ScoreboardComparison {
    Equal,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
}

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

impl TryFrom<String> for ScoreHolder {
    type Error = String;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        ScoreHolder::new(s)
    }
}

impl AsRef<str> for ScoreHolder {
    fn as_ref(&self) -> &str {
        &self.0
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

impl TryFrom<String> for Objective {
    type Error = String;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        Objective::new(s)
    }
}

impl AsRef<str> for Objective {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for Objective {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl Display for Objective {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for ObjectiveCriterion {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "dummy" => Ok(ObjectiveCriterion::Dummy),
            _ => Err(s.to_string()),
        }
    }
}

impl Display for ObjectiveCriterion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ObjectiveCriterion::Dummy => "dummy",
        })
    }
}

impl Display for FunctionIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.namespace, self.path)
    }
}

impl Display for FunctionSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{}{}",
            if self.is_collection { "#" } else { "" },
            self.id,
        ))
    }
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

impl FromStr for MinecraftRange {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((l, r)) = s.split_once("..") {
            let l = if l.is_empty() {
                None
            } else {
                let l = l.parse::<i32>().map_err(|_| s.to_string())?;
                Some(l)
            };

            let r = if r.is_empty() {
                None
            } else {
                let r = r.parse::<i32>().map_err(|_| s.to_string())?;
                Some(r)
            };

            match (l, r) {
                (Some(l), Some(r)) => Ok(MinecraftRange::Range { from: l, to: r }),
                (Some(l), None) => Ok(MinecraftRange::Minimum(l)),
                (None, Some(r)) => Ok(MinecraftRange::Maximum(r)),
                (None, None) => Err(s.to_string())
            }
        } else {
            let val = s.parse::<i32>().map_err(|_| s.to_string())?;

            Ok(MinecraftRange::Equal(val))
        }
    }
}

impl Display for MinecraftRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            MinecraftRange::Range { from, to } => write!(f, "{}..{}", from, to),
            MinecraftRange::Minimum(lo) => write!(f, "{}..", lo),
            MinecraftRange::Maximum(hi) => write!(f, "..{}", hi),
            MinecraftRange::Equal(x) => write!(f, "{}", x)
        }
    }
}

impl FromStr for ScoreboardComparison {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "=" => Ok(ScoreboardComparison::Equal),
            "<" => Ok(ScoreboardComparison::Less),
            ">" => Ok(ScoreboardComparison::Greater),
            "<=" => Ok(ScoreboardComparison::LessOrEqual),
            ">=" => Ok(ScoreboardComparison::GreaterOrEqual),
            _ => Err(s.to_string())
        }
    }
}

impl Display for ScoreboardComparison {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ScoreboardComparison::Equal => write!(f, "="),
            ScoreboardComparison::Less => write!(f, "<"),
            ScoreboardComparison::LessOrEqual => write!(f, "<="),
            ScoreboardComparison::Greater => write!(f, ">"),
            ScoreboardComparison::GreaterOrEqual => write!(f, ">="),
        }
    }
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

impl FromStr for FunctionSpec {
    type Err = String;

    fn from_str(old_s: &str) -> Result<Self, Self::Err> {
        let mut s = old_s;

        let is_collection = if let Some(body) = s.strip_prefix('#') {
            s = body;
            true
        } else {
            false
        };

        let id = s.parse()?;

        Ok(FunctionSpec { id, is_collection })
    }
}

impl FromStr for FunctionIdent {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (namespace, path) = s.split_once(':').ok_or_else(|| s.to_string())?;

        // TODO: Verify namespace and path
        let namespace = namespace.into();
        let path = path.to_owned();

        Ok(FunctionIdent { path, namespace })
    }
}

/// Parses the contents of a `.mcfunction` file into a strongly-typed form
pub fn parse_function_body(contents: &str) -> Result<Vec<Command>, String> {
	contents.lines()
		.map(|l| l.trim())
		.filter(|l| !l.is_empty())
		.map(|l| {
            l.parse::<Command>().map_err(|e| {
                format!("error when parsing `{}`: {}", l, e)
            })
		}).collect::<Result<Vec<_>, String>>()
}

/// The `id` is the full ID of the function, e.g. `"foo:bar/baz"`
pub fn parse_function(id: &str, contents: &str) -> Result<Function, String> {
    let id = id.parse()?;
    let cmds = parse_function_body(contents)?;
	Ok(Function { id, cmds })
}

fn get_functions_in_dir(namespace: &str, path_prefix: &str, dir: &Directory) -> Result<Vec<Function>, String> {
    let mut funcs = Vec::new();

    for (subdir_name, subdir) in dir.directories.iter() {
        let prefix = format!("{}{}/", path_prefix, subdir_name);
        funcs.extend(get_functions_in_dir(namespace, &prefix, &subdir)?);
    }

    for (func_name, func) in dir.files.iter() {
        let func_name = func_name.strip_suffix(".mcfunction").unwrap();

        let id = format!("{}:{}{}", namespace, path_prefix, func_name);
        let func = parse_function(&id, &func.contents)?;
        funcs.push(func);
    }

    Ok(funcs)
}

/// Parses all of the functions contained in the datapack `dir`
pub fn get_functions(dir: &Directory) -> Result<Vec<Function>, String> {
    let data_dir = dir.directories.get("data")
        .ok_or_else(|| "datapack did not contain `data` directory".to_string())?;
    
    let mut funcs = Vec::new();
    
    for (namespace, contents) in data_dir.directories.iter() {
        funcs.extend(get_functions_in_dir(&namespace, "", contents)?);
    }

    Ok(funcs)
}

fn get_func_file<'a>(id: &FunctionIdent, root_dir: &'a mut Directory) -> &'a mut File {
    let data_dir = root_dir.dir("data".to_owned());
    let namespace_dir = data_dir.dir((&*id.namespace).to_owned());

    let mut func_dir = namespace_dir;
    let mut path = id.path.split('/').collect::<Vec<&str>>();

    for &part in path[..path.len() - 1].iter() {
        func_dir = func_dir.dir(part.to_owned());
    } 

    let file_name = format!("{}.mcfunction", path.last().unwrap());
    func_dir.file(file_name)
}

pub fn write_function(func: &Function, root_dir: &mut Directory) {
    let contents = func.cmds.iter().map(|c| c.to_string()).collect::<Vec<String>>();
    let contents = contents.join("\n");

    let func_file = get_func_file(&func.id, root_dir);
    func_file.contents = contents;
}