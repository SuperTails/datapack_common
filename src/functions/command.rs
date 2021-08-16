use command_parser::{parse_multiple_commands, parse_optional_command, parser, CommandParse};
use itertools::{Itertools, Position};
use std::fmt;

/// Wrapper around `Option` to allow it to implement the `CommandParse` trait
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct Optional<T>(pub Option<T>);

impl<T> fmt::Display for Optional<T>
where
    T: CommandParse,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Some(val) => write!(f, "{}", val),
            None => Ok(()),
        }
    }
}

impl<T> CommandParse for Optional<T>
where
    T: CommandParse,
{
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, value) = parse_optional_command(value);
        Ok((rest, Optional(value)))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Repeated<T>(pub Vec<T>);

impl<T> fmt::Display for Repeated<T>
where
    T: CommandParse,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for value in self.0.iter().with_position() {
            if let Position::Last(value) | Position::Only(value) = value {
                write!(f, "{}", value)?;
            } else {
                let value = value.into_inner();
                write!(f, "{} ", value)?;
            }
        }
        Ok(())
    }
}

impl<T> CommandParse for Repeated<T>
where
    T: CommandParse,
{
    fn parse_from_command(value: &str) -> Result<(&str, Self), &str> {
        let (rest, value) = parse_multiple_commands(value);
        Ok((rest, Repeated(value)))
    }
}

#[parser]
pub mod commands {
    use crate::functions::{
        command_components::{
            BlockSpec, CommentMessage, DataPath, DataTarget, Entity, FillBlockKind, JsonText,
            Objective, RelBlockPos, SNbt, ScoreOpKind, ScoreboardTarget, Selector, SetBlockKind,
            Target,
        },
        FunctionIdent,
    };

    use super::{DataModifyKind, ExecuteSubCommand, Optional, Repeated};

    #[derive(Debug, PartialEq, Clone)]
    pub enum Command {}

    #[parse("clone $start $end $dest")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Clone {
        pub start: RelBlockPos,
        pub end: RelBlockPos,
        pub dest: RelBlockPos,
    }

    #[parse("$msg")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Comment {
        pub msg: CommentMessage,
    }

    #[parse("data get $target $path", scale=Optional(None))]
    #[parse("data get $target $path $scale")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct DataGet {
        pub target: DataTarget,
        pub path: DataPath,
        pub scale: Optional<f64>,
    }

    #[parse("data modify $target $path $kind")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct DataModify {
        pub target: DataTarget,
        pub path: DataPath,
        pub kind: DataModifyKind,
    }

    #[parse("gamerule $rule", value=Optional(None))]
    #[parse("gamerule $rule $value")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Gamerule {
        pub rule: String,
        pub value: Optional<String>,
    }

    // TODO: Remove this struct
    // This struct is required because the macro cannot handle default arguments that
    // are not patterns
    #[parse("execute run $command")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct ExecuteRun {
        pub command: Optional<Box<Command>>,
    }

    #[parse("execute $subcommands", run=Optional(None))]
    #[parse("execute $subcommands run $run")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Execute {
        pub subcommands: Repeated<ExecuteSubCommand>,
        pub run: Optional<Box<Command>>,
    }

    #[parse("fill $start $end $block", place_kind=FillBlockKind::Replace)]
    #[parse("fill $start $end $block $place_kind")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Fill {
        pub start: RelBlockPos,
        pub end: RelBlockPos,
        pub block: BlockSpec,
        pub place_kind: FillBlockKind,
    }

    #[parse("function $id")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct FuncCall {
        pub id: FunctionIdent,
    }

    #[parse("kill $target")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Kill {
        target: Target,
    }

    #[parse("scoreboard objectives add $0")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct ObjAdd(pub Objective);

    #[parse("scoreboard objectives remove $0")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct ObjRemove(pub Objective);

    #[parse("scoreboard players add $target $target_obj $score", remove = false)]
    #[parse("scoreboard players remove $target $target_obj $score", remove = true)]
    #[derive(Debug, PartialEq, Clone)]
    pub struct ScoreAdd {
        pub target: ScoreboardTarget,
        pub target_obj: Objective,
        pub remove: bool,
        pub score: i32,
    }

    #[parse("scoreboard players get $target $target_obj")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct ScoreGet {
        pub target: ScoreboardTarget,
        pub target_obj: Objective,
    }

    #[parse("scoreboard players operation $target $target_obj $op $source $source_obj")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct ScoreOp {
        pub target: ScoreboardTarget,
        pub target_obj: Objective,
        pub op: ScoreOpKind,
        pub source: ScoreboardTarget,
        pub source_obj: Objective,
    }

    #[parse("scoreboard players set $target $target_obj $score")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct ScoreSet {
        pub target: ScoreboardTarget,
        pub target_obj: Objective,
        pub score: i32,
    }

    #[parse("setblock $pos $block", kind=SetBlockKind::Replace)]
    #[parse("setblock $pos $block $kind")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct SetBlock {
        pub pos: RelBlockPos,
        pub block: BlockSpec,
        pub kind: SetBlockKind,
    }

    #[parse("summon $entity $pos", data=Optional(None))]
    #[parse("summon $entity $pos $data")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Summon {
        pub entity: Entity,
        pub pos: RelBlockPos,
        pub data: Optional<SNbt>,
    }

    #[parse("tp $target $pos")]
    #[parse("teleport $target $pos")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Teleport {
        pub target: Target,
        pub pos: RelBlockPos,
    }

    #[parse("tellraw $target $message")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Tellraw {
        pub target: Selector,
        pub message: JsonText,
    }
}

pub use commands::Command;

#[parser]
pub mod data_modify_kinds {
    use crate::functions::command_components::{DataLiteral, DataPath, DataTarget};

    use super::Optional;

    #[derive(Debug, PartialEq, Clone)]
    pub enum DataModifyKind {}

    #[parse("append from $target", source=Optional(None))]
    #[parse("append from $target $source")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct AppendFrom {
        target: DataTarget,
        source: Optional<DataPath>,
    }

    #[parse("append value $value")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Append {
        value: DataLiteral,
    }

    #[parse("insert $index from $target", source=Optional(None))]
    #[parse("insert $index from $target $source")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct InsertFrom {
        index: i32,
        target: DataTarget,
        source: Optional<DataPath>,
    }

    #[parse("insert $index value $value")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Insert {
        index: i32,
        value: DataLiteral,
    }

    #[parse("merge from $target", source=Optional(None))]
    #[parse("merge from $target $source")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct MergeFrom {
        target: DataTarget,
        source: Optional<DataPath>,
    }

    #[parse("merge value $value")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Merge {
        value: DataLiteral,
    }

    #[parse("prepend from $target", source=Optional(None))]
    #[parse("prepend from $target $source")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct PrependFrom {
        target: DataTarget,
        source: Optional<DataPath>,
    }

    #[parse("prepend value $value")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Prepend {
        value: DataLiteral,
    }

    #[parse("set from $target", source=Optional(None))]
    #[parse("set from $target $source")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct SetFrom {
        target: DataTarget,
        source: Optional<DataPath>,
    }

    #[parse("set value $value")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Set {
        value: DataLiteral,
    }

    #[parse("remove $target $source")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Remove {
        target: DataTarget,
        source: Optional<DataPath>,
    }
}

pub use data_modify_kinds::DataModifyKind;

// TODO: maybe create a generic condition enum which holds the `is_unless` attribute
#[parser]
pub mod exexute_sub_commands {
    use crate::functions::command_components::{
        BlockSpec, DataPath, DataTarget, MinecraftRange, Objective, RelBlockPos, RelPos,
        ScoreboardComparison, Target,
    };

    #[derive(Debug, Clone, PartialEq)]
    pub enum ExecuteSubCommand {}

    #[parse("if score $target $target_obj matches $range", is_unless = false)]
    #[parse("unless score $target $target_obj matches $range", is_unless = true)]
    #[derive(Debug, Clone, PartialEq)]
    pub struct IfScoreMatches {
        is_unless: bool,
        target: Target,
        target_obj: Objective,
        range: MinecraftRange,
    }

    #[parse(
        "if score $target $target_obj $relation $source $source_obj",
        is_unless = false
    )]
    #[parse(
        "unless score $target $target_obj $relation $source $source_obj",
        is_unless = true
    )]
    #[derive(Debug, Clone, PartialEq)]
    pub struct IfScoreRelation {
        is_unless: bool,
        target: Target,
        target_obj: Objective,
        relation: ScoreboardComparison,
        source: Target,
        source_obj: Objective,
    }

    #[parse("if block $pos $block", is_unless = false)]
    #[parse("unless block $pos $block", is_unless = true)]
    #[derive(Debug, Clone, PartialEq)]
    pub struct IfBlock {
        is_unless: bool,
        pos: RelBlockPos,
        block: BlockSpec,
    }

    #[parse("store result score $target $target_obj", is_success = false)]
    #[parse("store success score $target $target_obj", is_success = true)]
    #[derive(Debug, Clone, PartialEq)]
    pub struct StoreScore {
        is_success: bool,
        target: Target,
        target_obj: Objective,
    }

    #[parse("store result $target $path $scale", is_success = false)]
    #[parse("store success $target $path $scale", is_success = true)]
    #[derive(Debug, Clone, PartialEq)]
    pub struct StoreStorage {
        is_success: bool,
        target: DataTarget,
        path: DataPath,
        scale: f64,
    }

    #[parse("as $target")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct As {
        target: Target,
    }

    #[parse("at $target")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct At {
        target: Target,
    }

    #[parse("positioned $pos")]
    #[derive(Debug, PartialEq, Clone)]
    pub struct Positioned {
        pos: RelPos,
    }
}

pub use exexute_sub_commands::ExecuteSubCommand;

#[cfg(test)]
mod test {
    use super::Command;

    fn roundtrip_command(val: &str) {
        let command: Command = val.parse().unwrap();
        let command_as_str = command.to_string();
        assert_eq!(val, command_as_str)
    }

    #[test]
    fn test_clone() {
        roundtrip_command("clone 0 0 0 1 1 1 2 2 2");
    }

    #[test]
    fn test_comment() {
        roundtrip_command("# This is a comment and not actually a command");
    }

    #[test]
    fn test_data() {
        roundtrip_command("data get block 0 0 0 foo.bar.baz");
        roundtrip_command("data get entity @s foo.bar.baz 0.00115");
        roundtrip_command("data modify block 0 0 0 foo.bar.baz set value 90");
    }

    #[test]
    fn test_gamerule() {
        roundtrip_command("gamerule foo");
        roundtrip_command("gamerule foo bar");
    }

    #[test]
    fn test_execute() {
        roundtrip_command("execute run kill @e");
        roundtrip_command("execute if score source source_objective matches 0..10 run kill @e");
        roundtrip_command("execute as @a");
    }

    #[test]
    fn test_function_call() {
        roundtrip_command("function hello:world");
    }

    #[test]
    fn test_fill() {
        roundtrip_command("fill 0 0 0 1 1 1 block");
        roundtrip_command("fill ~ ~ ~ ~ ~ ~ block[facing=east]");
    }

    #[test]
    fn test_kill() {
        roundtrip_command("kill @e");
        roundtrip_command("kill player");
    }

    #[test]
    fn test_obj_remove() {
        roundtrip_command("scoreboard objectives remove my_scoreboard");
    }

    #[test]
    fn test_obj_add() {
        roundtrip_command("scoreboard objectives add my_scoreboard");
    }

    #[test]
    fn test_score_add() {
        roundtrip_command("scoreboard players add target target_objective 100");
        roundtrip_command("scoreboard players remove target target_objective 100");
    }

    #[test]
    fn test_score_get() {
        roundtrip_command("scoreboard players get source source_objective");
    }

    #[test]
    fn test_score_op() {
        roundtrip_command(
            "scoreboard players operation target target_objective += source source_objective",
        );
        roundtrip_command(
            "scoreboard players operation target target_objective *= source source_objective",
        );
        roundtrip_command(
            "scoreboard players operation target target_objective -= source source_objective",
        );
        roundtrip_command(
            "scoreboard players operation target target_objective /= source source_objective",
        );
        roundtrip_command(
            "scoreboard players operation target target_objective %= source source_objective",
        );
        roundtrip_command(
            "scoreboard players operation target target_objective = source source_objective",
        );
        roundtrip_command(
            "scoreboard players operation target target_objective < source source_objective",
        );
        roundtrip_command(
            "scoreboard players operation target target_objective > source source_objective",
        );
        roundtrip_command(
            "scoreboard players operation target target_objective >< source source_objective",
        );
    }

    #[test]
    fn test_score_set() {
        roundtrip_command("scoreboard players set source source_objective 0");
    }

    #[test]
    fn test_setblock() {
        roundtrip_command("setblock 0 0 0 block");
        roundtrip_command("setblock 0 0 0 block destroy");
        roundtrip_command("setblock 0 0 0 block keep");
        // Replace will never be emitted, since its redundant
        assert_eq!(
            "setblock 0 0 0 block replace"
                .parse::<Command>()
                .unwrap()
                .to_string(),
            "setblock 0 0 0 block"
        );
    }

    #[test]
    fn test_summon() {
        roundtrip_command("summon entity 0 0 0 {\"data\":\"value\"}");
    }

    #[test]
    fn test_teleport() {
        roundtrip_command("tp @s 0 0 0");
    }

    #[test]
    fn test_tellraw() {
        roundtrip_command(r#"tellraw @a {"text":"Hello"}"#);
    }
}
