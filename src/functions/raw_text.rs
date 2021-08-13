use super::command::{BlockPos, NbtPath, Selector, StorageId, StringNbt};
use super::{Objective, ScoreHolder};
use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;

#[derive(Default)]
pub struct TextBuilder {
    inner: Option<Vec<TextComponent>>,
}

impl TextBuilder {
    fn innermost(&mut self) -> Option<&mut TextComponent> {
        self.inner
            .as_mut()
            .map(|i| i.last_mut().unwrap().innermost())
    }

    fn push_component(&mut self) {
        *self.next_component() = Some(vec![Default::default()])
    }

    fn next_component(&mut self) -> &mut Option<Vec<TextComponent>> {
        if self.inner.is_none() {
            &mut self.inner
        } else {
            &mut self.innermost().unwrap().extra
        }
    }

    pub fn new() -> Self {
        Default::default()
    }

    pub fn build(&mut self) -> Vec<TextComponent> {
        self.inner
            .as_ref()
            .expect("tried to build empty `TextComponent`")
            .clone()
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

#[skip_serializing_none]
#[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct TextComponent {
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
    pub block: Option<BlockPos>,
    pub entity: Option<Selector>,
    pub storage: Option<StorageId>,

    // --- Child component ---
    pub extra: Option<Vec<TextComponent>>,

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

impl TextComponent {
    pub fn as_string<F>(&self, scores: &F) -> Result<String, String>
    where
        F: Fn(&ScoreHolder, &Objective) -> Option<i32>,
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
                todo!("how to print undefined score {}", name)
            };

            result.to_string()
        } else if let Some(selector) = &self.selector {
            todo!("{:?}", selector)
        } else if let Some(keybind) = &self.keybind {
            todo!("{:?}", keybind)
        } else if let Some(nbt) = &self.nbt {
            todo!("{:?}", nbt)
        } else {
            panic!("no content tags in `TextComponent`")
        };

        if let Some(extra) = &self.extra {
            for ex in extra {
                result.push_str(&ex.as_string(scores)?);
            }
        }

        Ok(result)
    }

    fn innermost(&mut self) -> &mut TextComponent {
        if self.extra.is_some() {
            self.extra.as_mut().unwrap().last_mut().unwrap().innermost()
        } else {
            self
        }
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
    ShowText(TextComponent),
    ShowItem {
        id: String,
        count: Option<usize>,
        tag: Option<StringNbt>,
    },
    ShowEntity {
        name: Option<TextComponent>,
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
    use super::Color;

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
