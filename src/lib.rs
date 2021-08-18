use functions::Function;
use std::borrow::Cow;
use vfs::Directory;

pub mod functions;
pub mod vfs;

pub struct Datapack {
    pub functions: Vec<Function>,
    pub description: String,
    pub pack_format: u32,
}

impl Datapack {
    pub fn to_directory(&self) -> Result<Directory, String> {
        let mut dir = Directory::default();

        let pack_mcmeta_file = dir.file("pack.mcmeta".to_owned());
        let pack_mcmeta_contents = PackMcmeta {
            pack: PackInfo {
                description: Cow::Borrowed(&self.description),
                pack_format: self.pack_format,
            },
        };

        pack_mcmeta_file.contents = serde_json::to_string(&pack_mcmeta_contents)
            .map_err(|e| format!("failed to serialize `pack.mcmeta`: {}", e))?;

        for func in self.functions.iter() {
            functions::write_function(func, &mut dir);
        }

        Ok(dir)
    }

    pub fn from_directory(dir: &Directory) -> Result<Self, String> {
        let pack_mcmeta = dir
            .files
            .get("pack.mcmeta")
            .ok_or_else(|| "datapack did not contain a `pack.mcmeta`".to_string())?;

        let pack_mcmeta: PackMcmeta = serde_json::from_str(&pack_mcmeta.contents)
            .map_err(|e| format!("invalid `pack.mcmeta`: {}", e))?;

        let functions = functions::get_functions(dir)?;

        Ok(Datapack {
            functions,
            description: pack_mcmeta.pack.description.into_owned(),
            // TODO: Should this have some verification?
            pack_format: pack_mcmeta.pack.pack_format,
        })
    }
}

/// This struct exists because the `pack.mcmeta` file has a top-level object
#[derive(serde::Serialize, serde::Deserialize)]
struct PackMcmeta<'a> {
    pub pack: PackInfo<'a>,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct PackInfo<'a> {
    pub description: Cow<'a, str>,
    pub pack_format: u32,
}

#[cfg(test)]
mod tests {}
