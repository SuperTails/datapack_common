use std::{collections::BTreeMap, convert::{TryFrom, TryInto}, fmt, str::FromStr};

use super::{FunctionIdent, MinecraftRange, Objective, ObjectiveCriterion, ScoreHolder, ScoreboardComparison, raw_text::TextComponent};

struct CommandParser<'a> {
    tail: &'a str,
}

#[derive(Debug, Clone)]
pub enum CommandParseError {
    UnexpectedEof,
}

type CmdParseResult<T> = Result<T, CommandParseError>;

impl CommandParser<'_> {
    pub fn next_word(&mut self) -> CmdParseResult<&str> {
        if self.tail.is_empty() {
            return Err(CommandParseError::UnexpectedEof);
        }

        if let Some(idx) = self.tail.find(char::is_whitespace) {
            let result = self.tail[..idx].trim();
            self.tail = self.tail[idx..].trim();
            Ok(result)
        } else {
            Ok(std::mem::take(&mut self.tail))
        }
    }

    pub fn peek_word(&mut self) -> Option<&str> {
        if self.tail.is_empty() {
            None
        } else if let Some(idx) = self.tail.find(char::is_whitespace) {
            Some(self.tail[..idx].trim())
        } else {
            Some(self.tail)
        }
    }

    pub fn parse(&mut self) -> CmdParseResult<Command> {
        Ok(match self.next_word()? {
            "#" => Command::Comment(self.tail.into()),
            "scoreboard" => self.parse_scoreboard()?,
            "execute" => self.parse_execute()?,
            "function" => FuncCall {
                id: self.tail.parse().expect("TODO:"),
            }
            .into(),
            "summon" => self.parse_summon()?,
            "tellraw" => self.parse_tellraw()?,
            "data" => self.parse_data()?,
            "tp" => self.parse_teleport()?,
            "fill" => self.parse_fill()?,
            "clone" => self.parse_clone()?,
            "setblock" => self.parse_setblock()?,
            "kill" => self.parse_kill()?,
            "gamerule" => self.parse_gamerule()?,
            nw => todo!("{:?}", nw),
        })
    }

    pub fn parse_target(&mut self) -> CmdParseResult<Target> {
        Ok(self.next_word()?.parse().expect("TODO:"))
    }

    pub fn parse_objective(&mut self) -> CmdParseResult<Objective> {
        Ok(self.next_word()?.to_owned().try_into().expect("TODO:"))
    }

    pub fn parse_obj_criterion(&mut self) -> CmdParseResult<ObjectiveCriterion> {
        Ok(self.next_word()?.to_owned().parse().expect("TODO:"))
    }

    pub fn parse_i32(&mut self) -> CmdParseResult<i32> {
        Ok(self.next_word()?.parse::<i32>().expect("TODO:"))
    }

    pub fn parse_f32(&mut self) -> CmdParseResult<f32> {
        Ok(self.next_word()?.parse::<f32>().expect("TODO:"))
    }

    pub fn parse_nbt_path(&mut self) -> CmdParseResult<NbtPath> {
        Ok(self.next_word()?.to_owned())
    }

    pub fn parse_kill(&mut self) -> CmdParseResult<Command> {
        let sel = self.parse_target()?;

        Ok(Kill(sel).into())
    }

    pub fn parse_gamerule(&mut self) -> CmdParseResult<Command> {
        let rule = self.next_word()?.to_string();
        let value = self.next_word().ok().map(|v| v.to_string());

        Ok(Gamerule { rule, value }.into())
    }

    pub fn parse_summon(&mut self) -> CmdParseResult<Command> {
        let entity = self.next_word()?.to_string();
        let pos = self.parse_rel_pos()?;
        let data = if self.tail.is_empty() { None } else { Some(self.tail.to_string()) };

        Ok(Summon { entity, pos, data }.into())
    }

    pub fn parse_clone(&mut self) -> CmdParseResult<Command> {
        let start = self.parse_rel_pos()?;
        let end = self.parse_rel_pos()?;
        let dest = self.parse_rel_pos()?;

        Ok(CloneCmd { start, end, dest }.into())
    }

    pub fn parse_fill(&mut self) -> CmdParseResult<Command> {
        let start = self.parse_rel_pos()?;
        let end = self.parse_rel_pos()?;
        let block = self.parse_block_spec()?;

        Ok(Fill { start, end, block }.into())
    }

    pub fn parse_block_spec(&mut self) -> CmdParseResult<BlockSpec> {
        let id_end = self.tail.find(|c: char| {
            !(c.is_alphanumeric() || c == ':' || c == '_')
        }).unwrap_or(self.tail.len());

        let id = self.tail[..id_end].to_string();
        self.tail = &self.tail[id_end..];

        let state = if self.tail.starts_with('[') {
            let state_end = self.tail.find(']').expect("TODO:");

            let state = &self.tail[..=state_end];

            self.tail = &self.tail[state_end + 1..];

            state.parse::<BlockState>().expect("TODO:")
        } else {
            BlockState::new()
        };

        let start = self.tail;

        let mut final_idx = None;

        let mut depth = 0;

        for (idx, c) in self.tail.chars().enumerate() {
            if depth == 0 && c == ' ' {
                final_idx = Some(idx);
                break;
            }

            if c == '{' {
                depth += 1;
            }

            if c == '}' {
                depth -= 1;
            }
        }

        let final_idx = final_idx.unwrap_or(self.tail.len());

        // TODO: Error
        assert_eq!(depth, 0, "{:?}", start);
        
        let result = &self.tail[..final_idx];
        self.tail = &self.tail[final_idx..].trim();

        let nbt = result.to_string();

        Ok(BlockSpec {
            id,
            state,
            nbt,
        })
    }

    pub fn parse_setblock(&mut self) -> CmdParseResult<Command> {
        let pos = self.parse_rel_pos()?;

        let block = self.parse_block_spec()?;

        let kind = self
            .next_word()
            .map(|w| w.parse().expect("TODO: "))
            .unwrap_or(SetBlockKind::Replace);

        Ok(SetBlock { pos, block, kind }.into())
    }

    pub fn parse_teleport(&mut self) -> CmdParseResult<Command> {
        let target = self.parse_target()?;
        let pos = self.parse_rel_pos()?;
        Ok(Teleport { target, pos }.into())
    }

    pub fn parse_modify_source(&mut self) -> CmdParseResult<DataModifySource> {
        match self.next_word()? {
            "value" => {
                if self.tail.starts_with('"') {
                    // TODO: Escapes
                    self.tail = &self.tail[1..];

                    let end = self.tail.find('"').expect("TODO:");

                    let value = self.tail[..end].to_string();
                    self.tail = &self.tail[end + 1..];

                    Ok(DataModifySource::ValueString(value))
                } else {
                    Ok(DataModifySource::Value(self.next_word()?.parse::<i32>().expect("TODO:")))
                }
            }
            nw => todo!("{:?}", nw)
        }
    }

    pub fn parse_data(&mut self) -> CmdParseResult<Command> {
        let kind_word = self.next_word().map(|s| s.to_owned())?;

        let target = self.parse_data_target()?;

        let kind = match kind_word.as_str() {
            "get" => {
                let path = self.parse_nbt_path()?;
                let scale = self.parse_f32()?;
                DataKind::Get { path, scale }
            }
            "modify" => {
                let path = self.parse_nbt_path()?;
                let modkind = self.next_word()?.parse::<DataModifyKind>().unwrap();
                let modsource = self.parse_modify_source()?;
                DataKind::Modify { path, kind: modkind, source: modsource }
            }
            nw => todo!("{:?}", nw),
        };

        Ok(Data { target, kind }.into())
    }

    pub fn parse_tellraw(&mut self) -> CmdParseResult<Command> {
        let target = self.parse_target()?;
        let message = serde_json::from_str(self.tail).expect("TODO:");

        Ok(Tellraw { target, message }.into())
    }

    pub fn parse_execute(&mut self) -> CmdParseResult<Command> {
        let mut cmd = Execute::new();

        loop {
            if self.peek_word() == Some("run") || self.peek_word().is_none() {
                break;
            }

            cmd.with_subcmd(self.parse_execute_subcmd()?);
        }

        if self.next_word().ok() == Some("run") {
            cmd.with_run(self.parse()?);
        }

        Ok(cmd.into())
    }

    pub fn parse_execute_subcmd(&mut self) -> CmdParseResult<ExecuteSubCmd> {
        match self.next_word()? {
            "if" => Ok(ExecuteSubCmd::Condition {
                is_unless: false,
                cond: self.parse_execute_cond()?,
            }),
            "unless" => Ok(ExecuteSubCmd::Condition {
                is_unless: true,
                cond: self.parse_execute_cond()?,
            }),
            "at" => Ok(ExecuteSubCmd::At {
                target: self.parse_target()?,
            }),
            "as" => Ok(ExecuteSubCmd::As {
                target: self.parse_target()?,
            }),
            "store" => self.parse_execute_store(),
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_execute_store(&mut self) -> CmdParseResult<ExecuteSubCmd> {
        let is_success = match self.next_word()? {
            "result" => false,
            "success" => true,
            nw => panic!("TODO: {:?}", nw),
        };

        let kind = match self.peek_word() {
            Some("score") => {
                self.next_word().unwrap();
                let target = self.parse_target()?;
                let objective = self.parse_objective()?;

                ExecuteStoreKind::Score { target, objective }
            }
            _ => {
                let target = self.parse_data_target()?;
                let path = self.parse_nbt_path()?;
                let ty = self.next_word()?.to_owned();
                let scale = self.parse_f32()?;

                ExecuteStoreKind::Data {
                    target,
                    path,
                    ty,
                    scale,
                }
            }
        };

        Ok(ExecuteSubCmd::Store { is_success, kind })
    }

    pub fn parse_coord(&mut self) -> CmdParseResult<Coord> {
        let coord = self.next_word()?;
        Ok(coord.parse().unwrap_or_else(|e| panic!("TODO: {:?} {:?}", coord, e)))
    }

    pub fn parse_rel_pos(&mut self) -> CmdParseResult<RelBlockPos> {
        let x = self.parse_coord()?;
        let y = self.parse_coord()?;
        let z = self.parse_coord()?;
        Ok(RelBlockPos(x, y, z))
    }

    pub fn parse_data_target(&mut self) -> CmdParseResult<DataTarget> {
        match self.next_word()? {
            "block" => Ok(DataTarget::Block(self.parse_rel_pos()?)),
            "entity" => {
                let target = self.parse_target()?;

                Ok(DataTarget::Entity(target))
            }
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_execute_cond(&mut self) -> CmdParseResult<ExecuteCondition> {
        match self.next_word()? {
            "score" => {
                let target = self.parse_target()?;
                let target_obj = self.parse_objective()?;
                let kind = match self.next_word()? {
                    "matches" => {
                        ExecuteCondKind::Matches(self.next_word()?.parse().expect("TODO:"))
                    }
                    s => {
                        let relation = s.parse().expect("TODO:");
                        let source = self.parse_target()?;
                        let source_obj = self.parse_objective()?;
                        ExecuteCondKind::Relation {
                            relation,
                            source,
                            source_obj,
                        }
                    }
                };

                Ok(ExecuteCondition::Score {
                    target,
                    target_obj,
                    kind,
                })
            }
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_scoreboard(&mut self) -> CmdParseResult<Command> {
        match self.next_word()? {
            "players" => self.parse_players(),
            "objectives" => self.parse_objectives(),
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_objectives(&mut self) -> CmdParseResult<Command> {
        match self.next_word()? {
            "add" => {
                let obj = self.parse_objective()?;
                let criteria = self.parse_obj_criterion()?;
                Ok(ObjAdd { obj, criteria }.into())
            }
            "remove" => Ok(ObjRemove(self.parse_objective()?).into()),
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_players(&mut self) -> CmdParseResult<Command> {
        match self.next_word()? {
            "operation" => self.parse_operation(),
            "add" => self.parse_scoreboard_add(false),
            "remove" => self.parse_scoreboard_add(true),
            "set" => self.parse_scoreboard_set(),
            "get" => self.parse_scoreboard_get(),
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_scoreboard_get(&mut self) -> CmdParseResult<Command> {
        let target = self.parse_target()?;
        let target_obj = self.parse_objective()?;
        Ok(ScoreGet { target, target_obj }.into())
    }

    pub fn parse_scoreboard_set(&mut self) -> CmdParseResult<Command> {
        let target = self.parse_target()?;
        let target_obj = self.parse_objective()?;

        let score = self.parse_i32()?;

        Ok(ScoreSet {
            target,
            target_obj,
            score,
        }
        .into())
    }

    pub fn parse_scoreboard_add(&mut self, is_remove: bool) -> CmdParseResult<Command> {
        let target = self.parse_target()?;
        let target_obj = self.parse_objective()?;
        let score = self.parse_i32()?;
        let score = if is_remove { -score } else { score };
        Ok(ScoreAdd {
            target,
            target_obj,
            score,
        }
        .into())
    }

    pub fn parse_operation(&mut self) -> CmdParseResult<Command> {
        let target = self.parse_target()?;
        let target_obj = self.parse_objective()?;
        let kind = self.next_word()?.parse().expect("TODO:");
        let source = self.parse_target()?;
        let source_obj = self.parse_objective()?;
        Ok(ScoreOp {
            target,
            target_obj,
            kind,
            source,
            source_obj,
        }
        .into())
    }
}

impl fmt::Display for CommandParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CommandParseError::UnexpectedEof => write!(f, "unexpected eof"),
        }
    }
}

impl FromStr for Command {
    type Err = CommandParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        CommandParser { tail: s }.parse()
    }
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Command::Gamerule(s) => s.fmt(f),
            Command::Kill(s) => s.fmt(f),
            Command::Fill(s) => s.fmt(f),
            Command::Summon(s) => s.fmt(f),
            Command::CloneCmd(s) => s.fmt(f),
            Command::SetBlock(s) => s.fmt(f),
            Command::ObjAdd(s) => s.fmt(f),
            Command::ObjRemove(s) => s.fmt(f),
            Command::ScoreOp(s) => s.fmt(f),
            Command::ScoreSet(s) => s.fmt(f),
            Command::ScoreGet(s) => s.fmt(f),
            Command::ScoreAdd(s) => s.fmt(f),
            Command::Execute(s) => s.fmt(f),
            Command::FuncCall(s) => s.fmt(f),
            Command::Data(s) => s.fmt(f),
            Command::Tellraw(s) => s.fmt(f),
            Command::Teleport(s) => s.fmt(f),
            Command::Comment(s) => {
                let mut commented = s.replace('\n', "\n# ");
                commented.insert_str(0, "# ");
                write!(f, "{}", commented)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Command {
    Gamerule(Gamerule),
    Kill(Kill),
    Fill(Fill),
    Summon(Summon),
    CloneCmd(CloneCmd),
    SetBlock(SetBlock),
    ObjRemove(ObjRemove),
    ObjAdd(ObjAdd),
    ScoreOp(ScoreOp),
    ScoreSet(ScoreSet),
    ScoreGet(ScoreGet),
    ScoreAdd(ScoreAdd),
    Execute(Execute),
    FuncCall(FuncCall),
    Data(Data),
    Tellraw(Box<Tellraw>),
    Teleport(Teleport),
    Comment(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Gamerule {
    pub rule: String,
    pub value: Option<String>,
}

impl fmt::Display for Gamerule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "gamerule {}", self.rule)?;

        if let Some(value) = &self.value {
            write!(f, " {}", value)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ObjRemove(pub Objective);

impl fmt::Display for ObjRemove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "scoreboard objectives remove {}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ObjAdd {
    pub obj: Objective,
    pub criteria: ObjectiveCriterion,
    // TODO: display name
}

impl fmt::Display for ObjAdd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "scoreboard objectives add {} {}",
            self.obj, self.criteria
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Kill(Target);

impl fmt::Display for Kill {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "kill {}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fill {
    pub start: RelBlockPos,
    pub end: RelBlockPos,
    pub block: BlockSpec,
}

impl fmt::Display for Fill {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fill {} {} {}", self.start, self.end, self.block)
    }
}

// TODO:
#[derive(Debug, PartialEq, Clone)]
pub struct Summon {
    pub entity: String,
    pub pos: RelBlockPos,
    pub data: Option<String>,
}

impl fmt::Display for Summon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "summon {} {}", self.entity, self.pos)?;
        if let Some(data) = self.data.as_ref() {
            write!(f, " {}", data)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CloneCmd {
    pub start: RelBlockPos,
    pub end: RelBlockPos,
    pub dest: RelBlockPos,
}

impl fmt::Display for CloneCmd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "clone {} {} {}", self.start, self.end, self.dest)
    }
}

impl From<Gamerule> for Command {
    fn from(g: Gamerule) -> Self {
        Command::Gamerule(g)
    }
}

impl From<Kill> for Command {
    fn from(k: Kill) -> Self {
        Command::Kill(k)
    }
}

impl From<Teleport> for Command {
    fn from(t: Teleport) -> Self {
        Command::Teleport(t)
    }
}

impl From<Fill> for Command {
    fn from(f: Fill) -> Self {
        Command::Fill(f)
    }
}

impl From<Summon> for Command {
    fn from(c: Summon) -> Self {
        Command::Summon(c)
    }
}

impl From<CloneCmd> for Command {
    fn from(c: CloneCmd) -> Self {
        Command::CloneCmd(c)
    }
}

impl From<ObjAdd> for Command {
    fn from(c: ObjAdd) -> Self {
        Command::ObjAdd(c)
    }
}

impl From<ObjRemove> for Command {
    fn from(c: ObjRemove) -> Self {
        Command::ObjRemove(c)
    }
}

impl From<ScoreOp> for Command {
    fn from(s: ScoreOp) -> Self {
        Command::ScoreOp(s)
    }
}

impl From<ScoreSet> for Command {
    fn from(s: ScoreSet) -> Self {
        Command::ScoreSet(s)
    }
}

impl From<Execute> for Command {
    fn from(e: Execute) -> Self {
        Command::Execute(e)
    }
}

impl From<FuncCall> for Command {
    fn from(f: FuncCall) -> Self {
        Command::FuncCall(f)
    }
}

impl From<Data> for Command {
    fn from(d: Data) -> Self {
        Command::Data(d)
    }
}

impl From<ScoreGet> for Command {
    fn from(s: ScoreGet) -> Self {
        Command::ScoreGet(s)
    }
}

impl From<ScoreAdd> for Command {
    fn from(s: ScoreAdd) -> Self {
        Command::ScoreAdd(s)
    }
}

impl From<SetBlock> for Command {
    fn from(s: SetBlock) -> Self {
        Command::SetBlock(s)
    }
}

impl From<Tellraw> for Command {
    fn from(t: Tellraw) -> Self {
        Command::Tellraw(Box::new(t))
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Teleport {
    pub target: Target,
    pub pos: RelBlockPos,
}

impl fmt::Display for Teleport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "tp {} {}", self.target, self.pos)
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Tellraw {
    pub target: Target,
    pub message: Vec<TextComponent>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Coord {
    pub val: i32,
    pub is_rel: bool,
}

impl FromStr for Coord {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "~" {
            Ok(Coord { val: 0, is_rel: true })
        } else if let Some(s) = s.strip_prefix("~") {
            let val = s.parse::<i32>().map_err(|s| s.to_string())?;
            Ok(Coord { val, is_rel: true })
        } else {
            let val = s.parse::<i32>().map_err(|s| s.to_string())?;
            Ok(Coord { val, is_rel: false } )
        }
    }
}

impl fmt::Display for Coord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_rel {
            write!(f, "~")?;
        }
        write!(f, "{}", self.val)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct RelBlockPos(pub Coord, pub Coord, pub Coord);

impl RelBlockPos {
    pub fn maybe_based(&self, base: Option<(i32, i32, i32)>) -> (i32, i32, i32) {
        if let Some(b) = base {
            self.based(b)
        } else {
            assert!(!self.0.is_rel);
            assert!(!self.1.is_rel);
            assert!(!self.2.is_rel);
            (self.0.val, self.1.val, self.2.val)
        }
    }

    pub fn based(&self, base: (i32, i32, i32)) -> (i32, i32, i32) {
        let x = if self.0.is_rel {
            self.0.val + base.0
        } else {
            self.0.val
        };

        let y = if self.1.is_rel {
            self.1.val + base.1
        } else {
            self.1.val
        };

        let z = if self.2.is_rel {
            self.2.val + base.2
        } else {
            self.2.val
        };

        (x, y, z)
    }
}

impl fmt::Display for RelBlockPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.0, self.1, self.2)
    }
}

impl FromStr for RelBlockPos {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = s.split_whitespace();
        let x = s.next().ok_or_else(|| "expected x".to_string())?;
        let y = s.next().ok_or_else(|| "expected y".to_string())?;
        let z = s.next().ok_or_else(|| "expected z".to_string())?;
        if s.next().is_some() {
            return Err("unexpected trailing data".to_string())
        }

        Ok(RelBlockPos(x.parse()?, y.parse()?, z.parse()?))
    }
}


pub type NbtPath = String;
pub type BlockPos = String;
pub type StorageId = String;
pub type StringNbt = String;

impl fmt::Display for Tellraw {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "tellraw {} {}",
            self.target,
            serde_json::to_string(&self.message).unwrap()
        )
    }
}

type BlockId = String;

type SNbt = String;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct BlockState(BTreeMap<String, String>);

impl BlockState {
    pub fn new() -> Self {
        BlockState(BTreeMap::new())
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.0.get(key).map(|v| v.as_str())
    }
    
    pub fn items(&self) -> impl Iterator<Item=(&str, &str)> {
        self.0.iter().map(|(k, v)| (k.as_str(), v.as_str()))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl FromStr for BlockState {
    type Err = String;

    fn from_str(mut s: &str) -> Result<Self, Self::Err> {
        let orig_s = s;

        if !s.starts_with('[') {
            return Err(format!("expected '[' at beginning of {}", orig_s))
        }
        s = &s[1..];

        if !s.ends_with(']') {
            return Err(format!("expected ']' at end of {}", orig_s));
        }
        s = &s[..s.len() - 1];

        let inner = s.split(',').map(|pair| {
            pair.split_once('=')
                .map(|(l, r)| (l.to_string(), r.to_string()))
                .ok_or_else(|| format!("missing '=' in {}", orig_s))
        }).collect::<Result<BTreeMap<String, String>, _>>()?;

        Ok(BlockState(inner))
    }
}

impl fmt::Display for BlockState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

#[derive(Debug, PartialEq, Clone)]
pub struct BlockSpec {
    pub id: BlockId,
    pub state: BlockState,
    pub nbt: SNbt,
}

impl fmt::Display for BlockSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.id, self.state, self.nbt)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SetBlock {
    pub pos: RelBlockPos,
    pub block: BlockSpec,
    pub kind: SetBlockKind,
}

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
pub enum SetBlockKind {
    Destroy,
    Keep,
    Replace,
}

impl FromStr for SetBlockKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "destroy" => Ok(Self::Destroy),
            "keep" => Ok(Self::Keep),
            "replace" => Ok(Self::Replace),
            _ => Err(format!("invalid setblock kind `{}`", s))
        }
    }
}

impl fmt::Display for SetBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "setblock {} {} {}", self.pos, self.block, self.kind)
    }
}

impl fmt::Display for SetBlockKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SetBlockKind::Destroy => write!(f, "destroy"),
            SetBlockKind::Keep => write!(f, "keep"),
            SetBlockKind::Replace => write!(f, "replace"),
        }
    }
}

/* Scoreboard (players functions)

TODO: scoreboard players reset <targets> [<objectives>]
*/

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScoreGet {
    pub target: Target,
    pub target_obj: Objective,
}

impl fmt::Display for ScoreGet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "scoreboard players get {} {}",
            self.target, self.target_obj
        )
    }
}

/// `scoreboard players set <targets> <objective> <score>`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScoreAdd {
    pub target: Target,
    pub target_obj: Objective,
    pub score: i32,
}

impl fmt::Display for ScoreAdd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "scoreboard players ")?;
        let score = if self.score < 0 {
            write!(f, "remove ")?;
            -self.score
        } else {
            write!(f, "add ")?;
            self.score
        };

        write!(f, "{} {} {}", self.target, self.target_obj, score)
    }
}

/// `scoreboard players set <targets> <objective> <score>`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScoreSet {
    pub target: Target,
    pub target_obj: Objective,
    pub score: i32,
}

impl fmt::Display for ScoreSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "scoreboard players set {} {} {}",
            self.target, self.target_obj, self.score
        )
    }
}


/// `scoreboard players operation <targets> <targetObjective> <operation> <source> <sourceObjective>`
///
/// `<operation>` may be: `+=`, `-=`, `*=`, `/=`, `%=`, `=`, `<` (min), `>` (max), `><` (swap)
///
/// Both `target` and `source` may be `*`, which uses all entites tracked by (TODO: the scoreboard? that objective?)
///
/// All operations treat a null score as 0
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScoreOp {
    pub target: Target,
    pub target_obj: Objective,
    pub kind: ScoreOpKind,
    pub source: Target,
    pub source_obj: Objective,
}

impl fmt::Display for ScoreOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "scoreboard players operation {} {} {} {} {}",
            self.target, self.target_obj, self.kind, self.source, self.source_obj
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScoreOpKind {
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    Assign,
    Min,
    Max,
    Swap,
}

impl FromStr for ScoreOpKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+=" => Ok(Self::AddAssign),
            "-=" => Ok(Self::SubAssign),
            "*=" => Ok(Self::MulAssign),
            "/=" => Ok(Self::DivAssign),
            "%=" => Ok(Self::ModAssign),
            "=" => Ok(Self::Assign),
            "<" => Ok(Self::Min),
            ">" => Ok(Self::Max),
            "><" => Ok(Self::Swap),
            _ => Err(s.into()),
        }
    }
}

impl fmt::Display for ScoreOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScoreOpKind::AddAssign => write!(f, "+="),
            ScoreOpKind::SubAssign => write!(f, "-="),
            ScoreOpKind::MulAssign => write!(f, "*="),
            ScoreOpKind::DivAssign => write!(f, "/="),
            ScoreOpKind::ModAssign => write!(f, "%="),
            ScoreOpKind::Assign => write!(f, "="),
            ScoreOpKind::Min => write!(f, "<"),
            ScoreOpKind::Max => write!(f, ">"),
            ScoreOpKind::Swap => write!(f, "><"),
        }
    }
}

/*

/data
    ... get <TARGET> [<path>] [<scale>]
    ... merge <TARGET> <nbt>
    ... modify <TARGET> <targetPath> <MODIFICATION>
        ... from <SOURCE> [<sourcePath>]
        ... value <value>
    ... remove <TARGET> <path>

<TARGET> = <SOURCE> = (block <targetPos> | entity <target> | storage <target>)
<MODIFICATION> = (append | insert <index> | merge | prepend | set)

*/

#[derive(Debug, PartialEq, Clone)]
pub struct Data {
    pub target: DataTarget,
    pub kind: DataKind,
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "data ")?;
        match &self.kind {
            DataKind::Get { .. } => write!(f, "get ")?,
            DataKind::Modify { .. } => write!(f, "modify ")?,
        }
        write!(f, "{} ", self.target)?;
        match &self.kind {
            DataKind::Get { path, scale } => write!(f, "{} {}", path, scale),
            DataKind::Modify { path, kind, source } => write!(f, "{} {} {}", path, kind, source),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataKind {
    Get {
        path: String,
        scale: f32,
    },
    Modify {
        path: String,
        kind: DataModifyKind,
        source: DataModifySource,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataModifyKind {
    // TODO: There's others
    Set,
}

impl FromStr for DataModifyKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "set" => Ok(DataModifyKind::Set),
            _ => Err(format!("invalid data modify kind {}", s))
        }
    }
}

impl fmt::Display for DataModifyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Set => write!(f, "set"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataModifySource {
    // TODO: There's another
    // TODO: This can technically be other datatypes too, I think
    Value(i32),
    ValueString(String),
}

impl fmt::Display for DataModifySource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataModifySource::Value(v) => write!(f, "value {}", v),
            DataModifySource::ValueString(v) => {
                if v.contains(char::is_control) || v.contains('\\') || v.contains('"') {
                    todo!("{:?}", v)
                }
                write!(f, "value {:?}", v)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataTarget {
    // TODO: More
    Block(RelBlockPos),
    Entity(Target),
}

impl fmt::Display for DataTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataTarget::Block(b) => write!(f, "block {}", b),
            DataTarget::Entity(e) => write!(f, "entity {}", e),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Execute {
    pub subcommands: Vec<ExecuteSubCmd>,
    pub run: Option<Box<Command>>,
}

impl fmt::Display for Execute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "execute")?;
        for sub in self.subcommands.iter() {
            write!(f, " {}", sub)?;
        }
        if let Some(run) = &self.run {
            write!(f, " run {}", run)?;
        }
        Ok(())
    }
}

impl Execute {
    pub fn new() -> Self {
        Execute::default()
    }

    pub fn with_subcmd(&mut self, cmd: ExecuteSubCmd) -> &mut Self {
        self.subcommands.push(cmd);
        self
    }

    pub fn with_if(&mut self, cond: ExecuteCondition) -> &mut Self {
        self.with_subcmd(ExecuteSubCmd::Condition {
            is_unless: false,
            cond,
        })
    }

    pub fn with_unless(&mut self, cond: ExecuteCondition) -> &mut Self {
        self.with_subcmd(ExecuteSubCmd::Condition {
            is_unless: true,
            cond,
        })
    }

    pub fn with_as(&mut self, target: Target) -> &mut Self {
        self.with_subcmd(ExecuteSubCmd::As { target })
    }

    pub fn with_at(&mut self, target: Target) -> &mut Self {
        self.with_subcmd(ExecuteSubCmd::At { target })
    }

    pub fn with_positioned(&mut self, pos: String) -> &mut Self {
        self.with_subcmd(ExecuteSubCmd::Positioned { pos })
    }

    pub fn with_run<C: Into<Command>>(&mut self, cmd: C) -> &mut Self {
        assert!(self.run.is_none());

        self.run = Some(Box::new(cmd.into()));
        self
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExecuteSubCmd {
    // TODO: There's others lol
    Condition {
        is_unless: bool,
        cond: ExecuteCondition,
    },
    Store {
        is_success: bool,
        kind: ExecuteStoreKind,
    },
    As {
        target: Target,
    },
    At {
        target: Target,
    },
    // TODO: There's another one
    Positioned {
        pos: String,
    }
}

impl fmt::Display for ExecuteSubCmd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Condition { is_unless, cond } => {
                if *is_unless {
                    write!(f, "unless")?;
                } else {
                    write!(f, "if")?;
                }

                write!(f, " {}", cond)
            }
            Self::Store { is_success, kind } => {
                write!(f, "store ")?;
                if *is_success {
                    write!(f, "success ")?;
                } else {
                    write!(f, "result ")?;
                }
                write!(f, "{}", kind)
            }
            Self::As { target } => write!(f, "as {}", target),
            Self::At { target } => write!(f, "at {}", target),
            Self::Positioned { pos } => write!(f, "positioned {}", pos),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExecuteCondition {
    // TODO: There's more
    Score {
        target: Target,
        target_obj: Objective,
        kind: ExecuteCondKind,
    },
    Block {
        pos: BlockPos,
        block: String,
    },
}

impl FromStr for ExecuteCondition {
    type Err = CommandParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        CommandParser { tail: s }.parse_execute_cond()
    }
}

impl fmt::Display for ExecuteCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExecuteCondition::Score {
                target,
                target_obj,
                kind,
            } => write!(f, "score {} {} {}", target, target_obj, kind),
            ExecuteCondition::Block { pos, block } => write!(f, "block {} {}", pos, block),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExecuteCondKind {
    Relation {
        relation: ScoreboardComparison,
        source: Target,
        source_obj: Objective,
    },
    Matches(MinecraftRange),
}

impl fmt::Display for ExecuteCondKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Relation {
                relation,
                source,
                source_obj,
            } => write!(f, "{} {} {}", relation, source, source_obj),
            Self::Matches(range) => write!(f, "matches {}", range),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncCall {
    pub id: FunctionIdent,
}

impl fmt::Display for FuncCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id = self.id.to_string();
        if id.contains(':') {
            write!(f, "function {}", id)
        } else {
            //write!(f, "function {}:{}", DEFAULT_NAMESPACE, id)
            panic!()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
#[serde(into = "String", try_from = "&str")]
pub enum Target {
    Uuid(ScoreHolder),
    Selector(Selector),
    Asterisk,
}

impl From<Target> for String {
	fn from(t: Target) -> String {
		t.to_string()
	}
}

impl TryFrom<&str> for Target {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "*" {
            Ok(Target::Asterisk)
        } else if s.starts_with('@') {
            Ok(s.parse::<Selector>()?.into())
        } else {
            Ok(ScoreHolder::new(s.into())?.into())
        }
    }
}

impl From<ScoreHolder> for Target {
    fn from(score_holder: ScoreHolder) -> Self {
        Target::Uuid(score_holder)
    }
}

impl From<Selector> for Target {
    fn from(selector: Selector) -> Self {
        Target::Selector(selector)
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Uuid(uuid) => write!(f, "{}", uuid),
            Self::Selector(selector) => write!(f, "{}", selector),
            Self::Asterisk => write!(f, "*"),
        }
    }
}

/// A target selector, like `@p` or `@e[tag=foo]`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
#[serde(into = "String", try_from = "&str")]
pub struct Selector {
    pub var: SelectorVariable,
    pub args: Vec<SelectorArg>,
}

impl From<Selector> for String {
    fn from(s: Selector) -> Self {
        s.to_string()
    }
}

impl std::convert::TryFrom<&str> for Selector {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        s.parse()
    }
}

impl FromStr for Selector {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let var = s[0..2]
            .parse()
            .map_err(|_| format!("invalid selector {}", &s[0..2]))?;
        let args = &s[2..];
        let args = if args.is_empty() {
            Vec::new()
        } else if !args.starts_with('[') || !args.ends_with(']') {
            return Err(format!("incorrect brackets in '{}'", args));
        } else {
            args[1..args.len() - 1]
                .split(',')
                .map(|arg| SelectorArg(arg.to_owned()))
                .collect()
        };

        Ok(Selector { var, args })
    }
}

impl fmt::Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.var)?;
        let args = self
            .args
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<String>>();
        if !args.is_empty() {
            write!(f, "[{}]", args.join(","))
        } else {
            Ok(())
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

/// The first half of a [`Selector`], e.g. `@a`, or `@e`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SelectorVariable {
    NearestPlayer,
    RandomPlayer,
    AllPlayers,
    AllEntities,
    ThisEntity,
}

impl FromStr for SelectorVariable {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "@p" => Ok(Self::NearestPlayer),
            "@r" => Ok(Self::RandomPlayer),
            "@a" => Ok(Self::AllPlayers),
            "@e" => Ok(Self::AllEntities),
            "@s" => Ok(Self::ThisEntity),
            _ => Err(()),
        }
    }
}

impl fmt::Display for SelectorVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NearestPlayer => write!(f, "@p"),
            Self::RandomPlayer => write!(f, "@r"),
            Self::AllPlayers => write!(f, "@a"),
            Self::AllEntities => write!(f, "@e"),
            Self::ThisEntity => write!(f, "@s"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExecuteStoreKind {
    // TODO: There's 2 other kinds
    Score {
        target: Target,
        objective: Objective,
    },
    Data {
        target: DataTarget,
        path: String,
        ty: String,
        scale: f32,
    },
}

impl fmt::Display for ExecuteStoreKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Score { target, objective } => write!(f, "score {} {}", target, objective),
            Self::Data {
                target,
                path,
                ty,
                scale,
            } => write!(f, "{} {} {} {}", target, path, ty, scale),
        }
    }
}
