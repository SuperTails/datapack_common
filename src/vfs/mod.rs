//! Virtual in-memory file system.
//! might switch to a more sophisticated model in the future
use rustc_hash::FxHashMap;
use std::io::prelude::*;
use std::{
    fs::{create_dir_all, OpenOptions},
    io,
    path::Path,
    str,
};

/// Custom macro to easily generate predefined file structures
/// Not very efficient at the moment though
#[macro_export]
macro_rules! directories {
        ($($rest:tt)+) => {{
        #[allow(unused_mut)]
        let mut file_map: rustc_hash::FxHashMap<String, $crate::vfs::File> = rustc_hash::FxHashMap::default();
        #[allow(unused_mut)]
        let mut dir_map: rustc_hash::FxHashMap<String, $crate::vfs::Directory> = rustc_hash::FxHashMap::default();

        $crate::directories_inner!( file_map, dir_map, $($rest)+ );

        $crate::vfs::Directory::with_contents(dir_map, file_map)
    }};
    () => {
        $crate::vfs::Directory::new()
    }

}

#[macro_export]
macro_rules! directories_inner {
    ($fname:ident, $dname:ident, $k:expr => File($v:expr)) => {
        $fname.insert($k.into(), $crate::vfs::File::with_data($v.into()));
    };
    ($fname:ident, $dname:ident, $k:expr => File($v:expr), $($rest:tt)+) => {{
        $crate::directories_inner!($fname, $dname, $k => File($v));
        $crate::directories_inner!($fname, $dname, $($rest)+);
    }};
    ($fname:ident, $dname:ident, $k:ident => $v:expr) => {
        $dname.insert(stringify!($k).to_string(), $v);
    };
    ($fname:ident, $dname:ident, $k:ident => $v:expr, $($rest:tt)+) => {{
        $crate::directories_inner!($fname, $dname, $k => $v);
        $crate::directories_inner!($fname, $dname, $($rest)+);
    }};
    ($fname:ident, $dname:ident, $k:expr => $v:expr) => {
        $dname.insert($k.into(), $v);
    };
    ($fname:ident, $dname:ident, $k:expr => $v:expr, $($rest:tt)+) => {{
        $crate::directories_inner!($fname, $dname, $k => $v);
        $crate::directories_inner!($fname, $dname, $($rest)+);
    }};
    () => {
        $crate::vfs::Directory::new()
    };
}

#[derive(Debug, Eq, PartialEq)]
pub enum FsElement<'a> {
    File(&'a mut File),
    Directory(&'a mut Directory),
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct File {
    pub contents: String,
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Directory {
    pub files: FxHashMap<String, File>,
    pub directories: FxHashMap<String, Directory>,
}

impl<'a> FsElement<'a> {
    pub fn persist(&self, name: &str, path: &Path) -> io::Result<()> {
        match self {
            FsElement::Directory(dir) => dir.persist(name, path),
            FsElement::File(file) => file.persist(name, path),
        }
    }

    pub fn dir(self) -> Option<&'a mut Directory> {
        match self {
            FsElement::Directory(dir) => Some(dir),
            FsElement::File(_) => None,
        }
    }

    pub fn file(self) -> Option<&'a mut File> {
        match self {
            FsElement::File(file) => Some(file),
            FsElement::Directory(_) => None,
        }
    }
}

impl File {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn open(path: &Path) -> Result<Self, Box<dyn std::error::Error>> {
        let contents = std::fs::read_to_string(path)?;
        Ok(Self::with_data(contents))
    }

    pub fn with_data(data: String) -> Self {
        File { contents: data }
    }

    pub fn push_string(&mut self, data: &str) {
        self.contents.push_str(data);
    }

    pub fn persist(&self, name: &str, path: &Path) -> io::Result<()> {
        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path.join(name))?;

        file.write_all(self.contents.as_bytes())?;

        Ok(())
    }
}

impl Directory {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn open(path: &Path) -> Result<Self, Box<dyn std::error::Error>> {
        let dir = std::fs::read_dir(path)?;

        let mut directories = FxHashMap::default();
        let mut files = FxHashMap::default();

        for obj in dir {
            let obj = obj?;

            let name = obj.file_name();
            let name = name.to_str().unwrap().to_string();

            let file_type = obj.file_type()?;
            if file_type.is_dir() {
                let subdir = Directory::open(&obj.path())?;
                directories.insert(name, subdir);
            } else if file_type.is_file() {
                let subfile = File::open(&obj.path())?;
                files.insert(name, subfile);
            } else {
                todo!()
            }
        }

        Ok(Directory::with_contents(directories, files))
    }

    pub fn with_contents(
        directories: FxHashMap<String, Directory>,
        files: FxHashMap<String, File>,
    ) -> Self {
        Directory { files, directories }
    }

    /// returns a new file with this name or returns an existing file with this name
    pub fn file(&mut self, name: String) -> &mut File {
        self.files.entry(name).or_default()
    }

    /// Creates a new directory with this name or returns an existing directory with this name
    pub fn dir(&mut self, name: String) -> &mut Directory {
        self.directories.entry(name).or_default()
    }

    pub fn resolve_path(&mut self, path: &[&str]) -> Option<FsElement> {
        match path.split_first() {
            Some((&first, rest)) => {
                if let Some(file) = self.files.get_mut(first) {
                    match rest.is_empty() {
                        true => Some(FsElement::File(file)),
                        false => None,
                    }
                } else if let Some(dir) = self.directories.get_mut(first) {
                    dir.resolve_path(rest)
                } else {
                    None
                }
            }
            None => Some(FsElement::Directory(self)),
        }
    }

    pub fn persist(&self, name: &str, path: &Path) -> io::Result<()> {
        let own_path = path.join(name);
        create_dir_all(&own_path)?;

        for (dirname, dir) in &self.directories {
            dir.persist(dirname, &own_path)?;
        }

        for (filename, file) in &self.files {
            file.persist(filename, &own_path)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use rustc_hash::FxHashMap;

    use super::{Directory, File, FsElement};
    use crate::directories;

    #[test]
    fn file() {
        let file = File::new();
        assert_eq!(file.contents, "");
    }

    #[test]
    fn push_file() {
        let mut file = File::new();
        file.push_string("Foo");
        assert_eq!(file.contents, "Foo");
    }

    #[test]
    fn directory() {
        let dir = Directory::new();
        assert_eq!(dir.directories, FxHashMap::default());
        assert_eq!(dir.files, FxHashMap::default());
    }

    #[test]
    fn directory_file() {
        let mut dir = Directory::new();
        dir.file("foo".to_string()).push_string("bar");

        let file = dir.resolve_path(&["foo"]).unwrap();
        match file {
            FsElement::Directory(_) => panic!(),
            FsElement::File(file) => assert_eq!(file.contents, "bar"),
        }
    }

    #[test]
    fn directory_nested() {
        let mut dir = Directory::new();
        {
            let inner = dir.dir("inner".to_string());
            inner.file("foo".to_string()).push_string("bar");
        }

        let file = dir.resolve_path(&["inner", "foo"]).unwrap();
        match file {
            FsElement::Directory(_) => panic!(),
            FsElement::File(file) => assert_eq!(file.contents, "bar"),
        }
    }

    #[test]
    fn directory_default() {
        let pack = directories! {
            "pack.mcmeta" => File("<load default pack mcmeta>"),
            data => directories! {
                minecraft => directories!(),
                debris => directories!()
            },
            src => directories!()
        };

        pack.directories.get("src").expect("No source dir");
        let data = pack.directories.get("data").expect("No data dir");
        data.directories
            .get("minecraft")
            .expect("No minecraft directory");
        data.directories.get("debris").expect("No debris directory");

        assert_eq!(
            pack.files
                .get("pack.mcmeta")
                .expect("No pack.mcmeta file")
                .contents,
            "<load default pack mcmeta>".to_string()
        );
    }
}
