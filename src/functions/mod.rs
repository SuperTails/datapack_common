use crate::vfs::{Directory, File};
use std::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

pub mod command;
pub mod command_components;
pub mod raw_text;

pub use command::Command;

use self::command_components::FunctionIdent;

/// Represents a single function and its contents in a datapack
#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionIdent,
    pub cmds: Vec<Command>,
}

/// A unique minecraft function identifier
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSpec {
    /// The path and namespace for this function
    pub id: FunctionIdent,
    /// Whether this function is a collection, marked by a `#`
    pub is_collection: bool,
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
    contents
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .map(|l| {
            l.parse::<Command>()
                .map_err(|e| format!("error when parsing `{}`: {}", l, e))
        })
        .collect::<Result<Vec<_>, String>>()
}

/// The `id` is the full ID of the function, e.g. `"foo:bar/baz"`
pub fn parse_function(id: &str, contents: &str) -> Result<Function, String> {
    let id = id.parse()?;
    let cmds = parse_function_body(contents)?;
    Ok(Function { id, cmds })
}

fn get_functions_in_dir(
    namespace: &str,
    path_prefix: &str,
    dir: &Directory,
) -> Result<Vec<Function>, String> {
    let mut funcs = Vec::new();

    for (subdir_name, subdir) in dir.directories.iter() {
        let prefix = format!("{}{}/", path_prefix, subdir_name);
        funcs.extend(get_functions_in_dir(namespace, &prefix, subdir)?);
    }

    for (func_name, func) in dir.files.iter() {
        let func_name = func_name
            .strip_suffix(".mcfunction")
            .unwrap_or_else(|| panic!("invalid function name {:?}", func_name));

        let id = format!("{}:{}{}", namespace, path_prefix, func_name);
        let func = parse_function(&id, &func.contents)?;
        funcs.push(func);
    }

    Ok(funcs)
}

/// Parses all of the functions contained in the datapack `dir`
pub fn get_functions(dir: &Directory) -> Result<Vec<Function>, String> {
    let data_dir = dir
        .directories
        .get("data")
        .ok_or_else(|| "datapack did not contain `data` directory".to_string())?;

    let mut funcs = Vec::new();

    for (namespace, contents) in data_dir.directories.iter() {
        if let Some(contents) = contents.directories.get("functions") {
            funcs.extend(get_functions_in_dir(namespace, "", contents)?);
        }
    }

    Ok(funcs)
}

fn get_func_file<'a>(id: &FunctionIdent, root_dir: &'a mut Directory) -> &'a mut File {
    let data_dir = root_dir.dir("data".to_owned());
    let ns_dir = data_dir.dir((&*id.namespace).to_owned());
    let namespace_dir = ns_dir.dir("functions".to_owned());

    let mut func_dir = namespace_dir;
    let path = id.path.split('/').collect::<Vec<&str>>();

    for &part in path[..path.len() - 1].iter() {
        func_dir = func_dir.dir(part.to_owned());
    }

    let file_name = format!("{}.mcfunction", path.last().unwrap());
    func_dir.file(file_name)
}

pub fn write_function(func: &Function, root_dir: &mut Directory) {
    let contents = func
        .cmds
        .iter()
        .map(|c| c.to_string())
        .collect::<Vec<String>>();
    let contents = contents.join("\n");

    let func_file = get_func_file(&func.id, root_dir);
    func_file.contents = contents;
}
