use std::fmt;

use super::command_components::{
    NbtPath, Objective, RelBlockPos, SNbt, ScoreHolder, Selector, StorageId, StringNbt,
};
use command_parser::CommandParse;
use serde::{Deserialize, Serialize};
use serde_json::Deserializer;
use serde_with::skip_serializing_none;

#[derive(Default)]
pub struct TextBuilder {
    inner: Option<Vec<CompoundTextComponent>>,
}

impl TextBuilder {
    fn innermost(&mut self) -> Option<&mut CompoundTextComponent> {
        self.inner
            .as_mut()
            .map(|i| i.last_mut().unwrap().innermost())
    }

    fn push_component(&mut self) {
        if self.inner.is_none() {
            self.inner = Some(vec![Default::default()]);
        } else {
            self.innermost().unwrap().extra = Some(vec![JsonText::Compound(Default::default())]);
        }
    }

    pub fn new() -> Self {
        Default::default()
    }

    pub fn build(&mut self) -> JsonText {
        let parts: Vec<JsonText> = self
            .inner
            .as_ref()
            .expect("tried to build empty `TextComponent`")
            .iter()
            .map(|c| JsonText::Compound(Box::new(c.clone())))
            .collect();

        JsonText::List(parts)
    }

    pub fn color(&mut self, color: Color) -> &mut Self {
        let inner = self
            .innermost()
            .expect("tried to set the color of an empty `TextComponent`");

        assert_eq!(inner.color, None);

        inner.color = Some(color);

        self
    }

    pub fn append_text(&mut self, text: String) -> &mut Self {
        self.push_component();

        let inner = self.innermost().unwrap();
        inner.text = Some(text);

        self
    }

    pub fn append_score(
        &mut self,
        name: ScoreHolder,
        objective: Objective,
        value: Option<i32>,
    ) -> &mut Self {
        self.push_component();

        let inner = self.innermost().unwrap();

        inner.score = Some(ScoreComponent {
            name,
            objective,
            value,
        });

        self
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum JsonText {
    Bool(bool),
    String(String),
    List(Vec<JsonText>),
    Compound(Box<CompoundTextComponent>),
    // TODO: Raw number variants
}

impl fmt::Display for JsonText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        serde_json::to_string(self).unwrap().fmt(f)
    }
}

impl CommandParse for JsonText {
    fn parse_from_command(mut value: &str) -> Result<(&str, Self), &str> {
        value = value.trim_start();

        let mut d = Deserializer::from_str(value).into_iter::<JsonText>();
        let components = d.next().ok_or(value)?.map_err(|err| {
            println!("{:?}", err);
            value
        })?;
        let rest = &value[d.byte_offset()..];
        Ok((rest, components))
    }
}

impl JsonText {
    pub fn as_string<F, FS>(&self, scores: &mut F, storage_nbt: &mut FS) -> Result<String, String>
    where
        F: FnMut(&ScoreHolder, &Objective) -> Option<i32>,
        FS: FnMut(&StorageId, &NbtPath) -> Option<SNbt>,
    {
        match self {
            JsonText::Bool(b) => Ok(b.to_string()),
            JsonText::String(s) => Ok(s.clone()),
            JsonText::List(a) => a
                .iter()
                .map(|t| t.as_string(scores, storage_nbt))
                .collect::<Result<String, String>>(),
            JsonText::Compound(c) => c.as_string(scores, storage_nbt),
        }
    }
}

#[skip_serializing_none]
#[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct CompoundTextComponent {
    // --- Content tags ---
    pub text: Option<String>,
    pub translate: Option<String>,
    pub score: Option<ScoreComponent>,
    pub selector: Option<Selector>,
    pub keybind: Option<String>,
    pub nbt: Option<NbtPath>,

    // --- Other tag that goes with `translate` but isn't in a structure ---
    pub with: Option<Vec<String>>,

    // --- Other tags that go with `nbt` but aren't in a structure for some reason ---
    pub interpret: Option<bool>,
    pub block: Option<RelBlockPos>,
    pub entity: Option<Selector>,
    pub storage: Option<StorageId>,

    // --- Child component ---
    pub extra: Option<Vec<JsonText>>,

    // --- Formatting ---
    pub color: Option<Color>,
    pub font: Option<String>,
    pub bold: Option<bool>,
    pub italic: Option<bool>,
    pub underlined: Option<bool>,
    pub strikethrough: Option<bool>,
    pub obfuscated: Option<bool>,

    // --- Interactivity ---
    pub insertion: Option<String>,
    pub click_event: Option<ClickEvent>,
    pub hover_event: Option<Box<HoverEvent>>,
}

pub fn empty_score_getter(_holder: &ScoreHolder, _obj: &Objective) -> Option<i32> {
    None
}

pub fn empty_storage_nbt_getter(_storage_id: &StorageId, _path: &NbtPath) -> Option<SNbt> {
    None
}

impl CompoundTextComponent {
    pub fn as_string<F, FS>(&self, scores: &mut F, storage_nbt: &mut FS) -> Result<String, String>
    where
        F: FnMut(&ScoreHolder, &Objective) -> Option<i32>,
        FS: FnMut(&StorageId, &NbtPath) -> Option<SNbt>,
    {
        let mut result = if let Some(text) = &self.text {
            text.clone()
        } else if let Some(translate) = &self.translate {
            todo!("{:?}", translate)
        } else if let Some(ScoreComponent {
            name,
            objective,
            value,
        }) = &self.score
        {
            let result = if let Some(v) = value {
                *v
            } else if let Some(v) = scores(name, objective) {
                v
            } else {
                todo!("how to print undefined score?")
            };

            result.to_string()
        } else if let Some(selector) = &self.selector {
            todo!("{:?}", selector)
        } else if let Some(keybind) = &self.keybind {
            todo!("{:?}", keybind)
        } else if let Some(nbt) = &self.nbt {
            let snbt = if let Some(storage) = &self.storage {
                storage_nbt(storage, nbt)
            } else if let Some(entity) = &self.entity {
                todo!("nbt on entity {:?}", entity)
            } else if let Some(block) = &self.block {
                todo!("nbt on block {:?}", block)
            } else {
                return Err(
                    "nbt CompoundTextComponent did not have storage, entity, or block specified"
                        .to_string(),
                );
            };

            if let Some(snbt) = snbt {
                let interpret = self.interpret.unwrap_or(false);
                if interpret {
                    let text = snbt.to_string();
                    let text: JsonText = command_parser::parse_command(&text)
                        .map_err(|e| format!("invalid interpreted nbt {:?}", e))?;
                    text.as_string(scores, storage_nbt)?
                } else {
                    snbt.to_string()
                }
            } else {
                todo!("how to print undefined nbt?")
            }
        } else {
            panic!("no content tags in `CompoundTextComponent`")
        };

        if let Some(extra) = &self.extra {
            for ex in extra {
                result.push_str(&ex.as_string(scores, storage_nbt)?);
            }
        }

        Ok(result)
    }

    fn innermost(&mut self) -> &mut CompoundTextComponent {
        if self.extra.is_some() && !self.extra.as_ref().unwrap().is_empty() {
            let extra = self.extra.as_mut().unwrap();
            if let JsonText::Compound(extra) = extra.last_mut().unwrap() {
                extra.innermost()
            } else {
                panic!();
            }
        } else {
            self
        }
    }
}

impl fmt::Display for CompoundTextComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(self).unwrap())
    }
}

impl CommandParse for CompoundTextComponent {
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let mut stream = Deserializer::from_str(value).into_iter::<CompoundTextComponent>();
        let component = stream.next().ok_or(value)?.map_err(|_| value)?;
        let rest = &value[stream.byte_offset()..];
        Ok((rest, component))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Serialize, Deserialize)]
#[serde(into = "String")]
pub enum Color {
    Named(String),
    Hex(u8, u8, u8),
}

impl std::fmt::Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Color::Named(n) => write!(f, "{}", n),
            Color::Hex(r, g, b) => write!(f, "#{:02X}{:02X}{:02X}", r, g, b),
        }
    }
}

impl From<Color> for String {
    fn from(c: Color) -> String {
        c.to_string()
    }
}

#[skip_serializing_none]
#[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize)]
#[serde(tag = "action", content = "contents", rename_all = "snake_case")]
pub enum HoverEvent {
    ShowText(JsonText),
    ShowItem {
        id: String,
        count: Option<usize>,
        tag: Option<StringNbt>,
    },
    ShowEntity {
        name: Option<JsonText>,
        #[serde(rename = "type")]
        ty: String,
        id: String,
    },
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize)]
pub struct ClickEvent {
    pub action: ClickEventAction,
    pub value: String,
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize)]
#[serde(into = "&'static str")]
pub enum ClickEventAction {
    // rename to "open_url"
    OpenUrl,
    // rename to "open_file"
    OpenFile,
    // rename to "run_command"
    RunCommand,
    // rename to "suggest_command"
    SuggestCommand,
    // rename to "change_page",
    ChangePage,
    // rename to "copy_to_clipboard",
    CopyToClipboard,
}

impl From<ClickEventAction> for &'static str {
    fn from(c: ClickEventAction) -> Self {
        use ClickEventAction::*;

        match c {
            OpenUrl => "open_url",
            OpenFile => "open_file",
            RunCommand => "run_command",
            SuggestCommand => "suggest_command",
            ChangePage => "change_page",
            CopyToClipboard => "copy_to_clipboard",
        }
    }
}

#[skip_serializing_none]
#[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize)]
pub struct ScoreComponent {
    pub name: ScoreHolder,
    pub objective: Objective,
    pub value: Option<i32>,
}

#[cfg(test)]
mod test {
    use super::{empty_score_getter, empty_storage_nbt_getter, Color, JsonText, TextBuilder};

    #[test]
    fn eval_list_of_compound() {
        let text = TextBuilder::new()
            .append_text("foo".to_string())
            .append_text("bar".to_string())
            .build();

        let output = text
            .as_string(&mut empty_score_getter, &mut empty_storage_nbt_getter)
            .unwrap();
        assert_eq!(output, "foobar");
    }

    #[test]
    fn eval_string() {
        let text = r#""a""#;
        let text: JsonText = command_parser::parse_command(text).unwrap();

        let output = text
            .as_string(&mut empty_score_getter, &mut empty_storage_nbt_getter)
            .unwrap();
        assert_eq!(output, "a");
    }

    #[test]
    fn eval_list_of_string() {
        let text = r#"["a", "b", ["c", "d"]]"#;
        let text: JsonText = command_parser::parse_command(text).unwrap();

        let output = text
            .as_string(&mut empty_score_getter, &mut empty_storage_nbt_getter)
            .unwrap();
        assert_eq!(output, "abcd");
    }

    #[test]
    fn list_serialize() {
        let text = TextBuilder::new().append_text("foobar".to_string()).build();

        let output = serde_json::to_string(&text).unwrap();
        assert_eq!(output, r#"[{"text":"foobar"}]"#);
    }

    #[test]
    fn list_deserialize() {
        let text = TextBuilder::new().append_text("foobar".to_string()).build();

        let output: JsonText = serde_json::from_str(r#"[{"text":"foobar"}]"#).unwrap();

        assert_eq!(output, text);
    }

    #[test]
    fn string_roundtrip() {
        let before = r#"["a",["b","c"]]"#;
        let mid: JsonText = serde_json::from_str(before).unwrap();
        let after = serde_json::to_string(&mid).unwrap();
        assert_eq!(before, after);
    }

    #[test]
    fn color_serialize() {
        assert_eq!(
            serde_json::to_string(&Color::Named("red".to_string())).unwrap(),
            "\"red\"".to_string()
        );
        assert_eq!(
            serde_json::to_string(&Color::Hex(0x00, 0xFF, 0x54)).unwrap(),
            "\"#00FF54\"".to_string()
        );
    }

    #[test]
    fn hover_event_serialize() {
        assert_eq!(
            r#"{"action":"show_item","contents":{"id":"hi","count":1}}"#,
            serde_json::to_string(&super::HoverEvent::ShowItem {
                id: "hi".to_string(),
                count: Some(1),
                tag: None
            })
            .unwrap()
        );
    }
}
