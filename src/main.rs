use color_eyre::eyre::Result;
use log::debug;
use rand::prelude::*;
use serde::Deserialize;
use serde_derive::{Deserialize, Serialize};
use std::{fs, io};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize)]
enum Level {
     #[default]
     Lev1,  Lev2,  Lev3,  Lev4,  Lev5,  Lev6,  Lev7,  Lev8,  Lev9, Lev10,
    Lev11, Lev12, Lev13, Lev14, Lev15, Lev16, Lev17, Lev18, Lev19, Lev20,
}
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize)]
enum Level2 {
            #[default]
            Lev2,  Lev3,  Lev4,  Lev5,  Lev6,  Lev7,  Lev8,  Lev9, Lev10,
    Lev11, Lev12, Lev13, Lev14, Lev15, Lev16, Lev17, Lev18, Lev19, Lev20,
}
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize)]
enum Level3 {
                   #[default]
                   Lev3,  Lev4,  Lev5,  Lev6,  Lev7,  Lev8,  Lev9, Lev10,
    Lev11, Lev12, Lev13, Lev14, Lev15, Lev16, Lev17, Lev18, Lev19, Lev20,
}

impl std::ops::Add<Self> for Level {
    type Output = Level;
    fn add(self, other: Level) -> Self::Output {
        Self::from(self.to_usize() + other.to_usize())
    }
}

impl Level {
    fn to_usize(self) -> usize {
        self.into()
    }
}
impl Into<usize> for Level {
    fn into(self) -> usize {
        match self {
            Self::Lev1 => 1,
            Self::Lev2 => 2,
            Self::Lev3 => 3,
            Self::Lev4 => 4,
            Self::Lev5 => 5,
            Self::Lev6 => 6,
            Self::Lev7 => 7,
            Self::Lev8 => 8,
            Self::Lev9 => 9,
            Self::Lev10 => 10,
            Self::Lev11 => 11,
            Self::Lev12 => 12,
            Self::Lev13 => 13,
            Self::Lev14 => 14,
            Self::Lev15 => 15,
            Self::Lev16 => 16,
            Self::Lev17 => 17,
            Self::Lev18 => 18,
            Self::Lev19 => 19,
            Self::Lev20 => 20,
        }
    }
}
impl From<usize> for Level {
    /// Converts from [`usize`] to [`Level`].
    ///
    /// ## Panics
    ///
    /// Panics if the value is outside the range `1..=20`.
    fn from(value: usize) -> Self {
        match value {
            1 => Level::Lev1,
            2 => Level::Lev2,
            3 => Level::Lev3,
            4 => Level::Lev4,
            5 => Level::Lev5,
            6 => Level::Lev6,
            7 => Level::Lev7,
            8 => Level::Lev8,
            9 => Level::Lev9,
            10 => Level::Lev10,
            11 => Level::Lev11,
            12 => Level::Lev12,
            13 => Level::Lev13,
            14 => Level::Lev14,
            15 => Level::Lev15,
            16 => Level::Lev16,
            17 => Level::Lev17,
            18 => Level::Lev18,
            19 => Level::Lev19,
            20 => Level::Lev20,
            _ => panic!("Invalid value: Levels may only be from 1..20 inclusive."),
        }
    }
}

impl Into<Level> for Level2 {
    fn into(self) -> Level {
        match self {
            Self::Lev2 => Level::Lev2,
            Self::Lev3 => Level::Lev3,
            Self::Lev4 => Level::Lev4,
            Self::Lev5 => Level::Lev5,
            Self::Lev6 => Level::Lev6,
            Self::Lev7 => Level::Lev7,
            Self::Lev8 => Level::Lev8,
            Self::Lev9 => Level::Lev9,
            Self::Lev10 => Level::Lev10,
            Self::Lev11 => Level::Lev11,
            Self::Lev12 => Level::Lev12,
            Self::Lev13 => Level::Lev13,
            Self::Lev14 => Level::Lev14,
            Self::Lev15 => Level::Lev15,
            Self::Lev16 => Level::Lev16,
            Self::Lev17 => Level::Lev17,
            Self::Lev18 => Level::Lev18,
            Self::Lev19 => Level::Lev19,
            Self::Lev20 => Level::Lev20,
        }
    }
}
impl Into<Level> for Level3 {
    fn into(self) -> Level {
        match self {
            Self::Lev3 => Level::Lev3,
            Self::Lev4 => Level::Lev4,
            Self::Lev5 => Level::Lev5,
            Self::Lev6 => Level::Lev6,
            Self::Lev7 => Level::Lev7,
            Self::Lev8 => Level::Lev8,
            Self::Lev9 => Level::Lev9,
            Self::Lev10 => Level::Lev10,
            Self::Lev11 => Level::Lev11,
            Self::Lev12 => Level::Lev12,
            Self::Lev13 => Level::Lev13,
            Self::Lev14 => Level::Lev14,
            Self::Lev15 => Level::Lev15,
            Self::Lev16 => Level::Lev16,
            Self::Lev17 => Level::Lev17,
            Self::Lev18 => Level::Lev18,
            Self::Lev19 => Level::Lev19,
            Self::Lev20 => Level::Lev20,
        }
    }
}

trait SubclassClass {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Barbarian {} impl SubclassClass for Barbarian {}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Bard      {} impl SubclassClass for Bard      {}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Cleric    {} impl SubclassClass for Cleric    {}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Druid     {} impl SubclassClass for Druid     {}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Fighter   {} impl SubclassClass for Fighter   {}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Monk      {} impl SubclassClass for Monk      {}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Paladin   {} impl SubclassClass for Paladin   {}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Ranger    {} impl SubclassClass for Ranger    {}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Rogue     {} impl SubclassClass for Rogue     {}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Sorcerer  {} impl SubclassClass for Sorcerer  {}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Warlock   {} impl SubclassClass for Warlock   {}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)] enum Wizard    {} impl SubclassClass for Wizard    {}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
struct Level1Subclass<T: SubclassClass> {
    subclass: T,
    level: Level,
}

impl<T: SubclassClass> Level1Subclass<T> {
    const fn level(&self) -> Level {
        self.level
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
enum Level2Subclass<T: SubclassClass> {
    #[default] Lev1,
    Lev2plus {
        subclass: T,
        level: Level2,
    },
}

impl<T: SubclassClass> Level2Subclass<T> {
    fn level(&self) -> Level {
        match self {
            Self::Lev1 => Level::Lev1,
            Self::Lev2plus { level, .. } => level.to_owned().into(),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
enum Level3Subclass<T: SubclassClass> {
    #[default] Lev1,
    Lev2,
    Lev3plus {
        subclass: T,
        level: Level3,
    },
}

impl<T: SubclassClass> Level3Subclass<T> {
    fn level(&self) -> Level {
        match self {
            Self::Lev1 => Level::Lev1,
            Self::Lev2 => Level::Lev2,
            Self::Lev3plus { level, .. } => level.to_owned().into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
enum Class {
    Barbarian(Level3Subclass<Barbarian>),
    Bard(Level3Subclass<Bard>),
    Cleric(Level3Subclass<Cleric>),
    Druid(Level2Subclass<Druid>),
    Fighter(Level1Subclass<Fighter>),
    Monk(Level3Subclass<Monk>),
    Paladin(Level3Subclass<Paladin>),
    Ranger(Level3Subclass<Ranger>),
    Rogue(Level3Subclass<Rogue>),
    Sorcerer(Level1Subclass<Sorcerer>),
    Warlock(Level1Subclass<Warlock>),
    Wizard(Level2Subclass<Wizard>),
}

impl Class {
    fn level(&self) -> Level {
        match self {
            Self::Barbarian(l3sub_barbarian) => l3sub_barbarian.level(),
            Self::Bard(l3sub_bard) => l3sub_bard.level(),
            Self::Cleric(l3sub_cleric) => l3sub_cleric.level(),
            Self::Druid(l2sub_druid) => l2sub_druid.level(),
            Self::Fighter(l1sub_fighter) => l1sub_fighter.level(),
            Self::Monk(l3sub_monk) => l3sub_monk.level(),
            Self::Paladin(l3sub_paladin) => l3sub_paladin.level(),
            Self::Ranger(l3sub_ranger) => l3sub_ranger.level(),
            Self::Rogue(l3sub_rogue) => l3sub_rogue.level(),
            Self::Sorcerer(l1sub_sorcerer) => l1sub_sorcerer.level(),
            Self::Warlock(l1sub_sorcerer) => l1sub_sorcerer.level(),
            Self::Wizard(l2sub_wizard) => l2sub_wizard.level(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
struct Character {
    classes: Vec<Class>,
}

impl Character {
    fn level(self) -> Level {
        let mut inner_level: Option<Level> = None;
        for class in self.classes {
            inner_level = match inner_level {
                Some(inner2_level) => Some(class.level() + inner2_level),
                None => Some(class.level()),
            };
        }
        inner_level.unwrap_or(Level::Lev1)
    }
}

/// Data that are saveable to a file.
trait SavedData
where
    for<'a> Self: Sized + Deserialize<'a>
{
    fn parse_from_str(s: &str) -> Self {
        serde_json5::from_str::<Self>(s).expect(&format!("Error parsing {}", Self::stringified()))
    }

    fn stringified() -> &'static str {
        debug!("TODO: Check stringified value");
        stringify!(Self)
    }
}
impl SavedData for Character {}
impl SavedData for Situation {}

/// Represents the statblock of a [Creature] or [Npc].
#[derive(Debug, Clone, Copy, PartialEq, Deserialize, Serialize)]
struct Statblock {
    /// Challenge Rating
    ///
    /// 0, ⅛, ¼, ½, 1–30
    cr: Option<f32>,
}

impl Statblock {
    const fn new() -> Self {
        Self { cr: None }
    }

    const fn from_cr(cr: f32) -> Self {
        Self { cr: Some(cr) }
    }

    fn set_cr(&mut self, cr: f32) {
        self.cr = Some(cr);
    }

    const fn cr(&self) -> Option<f32> {
        self.cr
    }
}

/// A replaceable template in an [Event].
trait Replaceable
where
    for<'a> Self: Sized + Clone + PartialEq + Deserialize<'a>
{
    /// Returns the replaceable token for the `Replaceable` item.
    fn token() -> &'static str;

    /// Returns `true` if the given `token` is the `Replaceable` item's token.
    fn is_token(token: &str) -> bool {
        token == Self::token()
    }
}

/// Implements [`Replaceable`] for the given `$type`, using the given `$token`
/// for the `Replaceable::token()` method.
macro_rules! impl_replaceable {
    ($type: ty, $token: expr) => {
        impl Replaceable for $type {
            fn token() -> &'static str { $token }
        }
    };
}

/// Represents a creature or other non-characterised NPC, such as a dragon, a
/// random occultist, or a wild dog.
#[derive(Debug, Default, Clone, PartialEq, Deserialize, Serialize)]
struct Creature {
    name: String,
    stats: Option<Statblock>,
}

impl Creature {
    const fn new() -> Self {
        Self {
            name: String::new(),
            stats: None,
        }
    }

    fn from_str(s: &str) -> Self {
        Self {
            name: s.to_string(),
            ..Self::new()
        }
    }

    fn from_name_and_cr(name: &str, cr: f32) -> Self {
        Self {
            name: name.to_string(),
            stats: Some(Statblock::from_cr(cr)),
        }
    }
}

impl_replaceable!(Creature, "{creature}");

/// Represents a non-player-controlled character with some backstory or
/// characterisation.
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
struct Npc {
    name: NpcName,
    backstory: Option<String>,
    stats: Option<Statblock>
}

impl Npc {
    const fn new() -> Self {
        Self {
            name: NpcName::new(),
            backstory: None,
            stats: None,
        }
    }
}

impl_replaceable!(Npc, "{npc}");

/// Represents a randomly selectable name for an [NPC](Npc).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
struct NpcName {
    name: String,
}

impl NpcName {
    const fn new() -> Self {
        Self {
            name: String::new(),
        }
    }
}

impl_replaceable!(NpcName, "{npc_name}");

/// Represents a place in the world.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
struct Place {
    name: PlaceName,
}

impl Place {
    const fn new() -> Self {
        Self {
            name: PlaceName::new(),
        }
    }
}

impl_replaceable!(Place, "{place}");

/// Represents a randomly selectable name for a [place](Place).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
struct PlaceName {
    name: String,
}

impl PlaceName {
    const fn new() -> Self {
        Self {
            name: String::new(),
        }
    }
}

impl_replaceable!(PlaceName, "{place_name}");

/// Represents a roll of a die with `self.max` sides, and an additional modifier
///  of `self.modifier`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize, Serialize)]
struct Roll {
    max: i8,
    modifier: i8,
}

impl Roll {
    /// Roll the roll.
    ///
    /// Returns an `i16` result from the random number up to `max` + the
    /// `modifier`.
    fn roll(self) -> i16 {
        let mut rng = thread_rng();
        (rng.gen_range(1..=self.max) + self.modifier) as i16
    }

    const fn d2(modifier: i8) -> Self {
        Roll { max: 2, modifier }
    }

    const fn d4(modifier: i8) -> Self {
        Roll { max: 4, modifier }
    }

    const fn d6(modifier: i8) -> Self {
        Roll { max: 6, modifier }
    }

    const fn d8(modifier: i8) -> Self {
        Roll { max: 8, modifier }
    }

    const fn d10(modifier: i8) -> Self {
        Roll { max: 10, modifier }
    }

    const fn d12(modifier: i8) -> Self {
        Roll { max: 12, modifier }
    }

    const fn d20(modifier: i8) -> Self {
        Roll { max: 20, modifier }
    }

    const fn d100(modifier: i8) -> Self {
        Roll { max: 100, modifier }
    }
}

impl_replaceable!(Roll, "{roll}");

#[derive(Debug, Clone, PartialEq, Deserialize)]
enum ReplaceableType {
    Creature(Creature),
    Npc(Npc),
    NpcName(NpcName),
    Place(Place),
    PlaceName(PlaceName),
    Roll(Roll),
}

impl ReplaceableType {
    fn from_token(token: &str) -> Option<Self> {
        match token {
            "{creature}" => Some(ReplaceableType::Creature(Creature::new())),
            "{npc}" => Some(ReplaceableType::Npc(Npc::new())),
            "{npc_name}" => Some(ReplaceableType::NpcName(NpcName::new())),
            "{place}" => Some(ReplaceableType::Place(Place::new())),
            "{place_name}" => Some(ReplaceableType::PlaceName(PlaceName::new())),
            "{roll}" => Some(ReplaceableType::Roll(Roll::d6(0))),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
enum EventToken {
    Text(String),
    TemplateToken(ReplaceableType),
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
struct Event {
    tokens: Vec<EventToken>,
}

impl Event {
    const fn new() -> Self {
        Self { tokens: Vec::new() }
    }

    fn push(&mut self, token: EventToken) { self.tokens.push(token) }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
struct Situation {
    location: Place,
    situation_type: SituationType,
    party: Vec<Npc>,
    animal_companions: Vec<Creature>,
}

impl Situation {
    const fn new() -> Self {
        Self {
            location: Place::new(),
            situation_type: SituationType::new(),
            party: Vec::new(),
            animal_companions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
struct SituationType {
    name: String,
}

impl SituationType {
    const fn new() -> Self {
        Self {
            name: String::new(),
        }
    }
}

impl From<String> for SituationType {
    fn from(s: String) -> Self {
        Self {
            name: s,
        }
    }
}

impl From<&String> for SituationType {
    fn from(s: &String) -> Self {
        Self {
            name: s.clone(),
        }
    }
}

/// Takes in a `path` to a json5 file (including the `.json5` extension, if it
/// has that) and returns a [Vec]tor of [String]s from the file's contents,
/// given the file contains an array of strings, such as
/// `["item", "item", ...]`, formatted in the JSON5 standard.
///
/// ## Panics
///
/// Panics if the file cannot be read, or if the contents are not a valid JSON5
/// array of strings.
fn read_json5_string_array_file<P: AsRef<std::path::Path> + std::fmt::Display + Clone>(path: P) -> Vec<String> {
    serde_json5::from_str::<Vec<String>>(&fs::read_to_string(path.clone())
        // if not present - inform user of absence and provide instructions to fix
        .expect(&format!("Failed to read `{path}`.\n The reason for this is likely that the file is missing.\n To fix this error, please add the file `{path}` to the root folder of the application.")))
        .expect(&format!("`{path}` is empty or mis-formatted. The `{path}` file should have an array of strings (`[\"string1\", \"string2\"]`)."))
}

/// Get user input, including printing a prompt.
fn get_input(prompt: &str) -> Option<String> {
    let mut input = String::new();
    print!("{prompt}");
    match io::stdin().read_line(&mut input) {
        Ok(_) => Some(input.trim().to_string()),
        Err(_) => None,
    }
}

/// Loop until a valid user input is provided.
fn looped_get_input(prompt: &str) -> String {
    match get_input(prompt) {
        Some(input) => input,
        None => {
            eprintln!("Invalid input. Please try again.");
            looped_get_input(prompt)
        },
    }
}

/// Loads a saved data file of the [`SavedData`] type `T`.
///
/// Uses user input to get the path to the saved data file.
///
/// Currently only supports `.json5` format files.
fn load_saved_data_file<T: SavedData>() -> T {
    let input_situation = looped_get_input(&format!("{} File (relative path): ", T::stringified()));
    println!("Loading...");
    match fs::read_to_string(input_situation) {
        Ok(string) => {
            println!("Loaded {} File!", T::stringified());
            T::parse_from_str(&string)
        },
        Err(e) => {
            eprintln!("Error reading Situation File: {e}");
            load_saved_data_file()
        },
    }
}

fn main() -> Result<()> {
    let raw_events = read_json5_string_array_file("events.json5");
    let raw_situation_types = read_json5_string_array_file("situation_types.json5");

    let mut events: Vec<Event> = Vec::new();
    for raw_event in raw_events {
        let mut event: Event = Event::new();
        for word in raw_event.split_whitespace() {
            let token: EventToken = if let Some(replaceable) = ReplaceableType::from_token(word) {
                EventToken::TemplateToken(replaceable)
            } else {
                EventToken::Text(word.to_string())
            };
            event.push(token);
        }
        events.push(event);
    }

    let mut situation_types: Vec<SituationType> = raw_situation_types.iter()
        .map(|raw_situation_type| SituationType::from(raw_situation_type))
        .collect();

    let situation: Situation = load_saved_data_file();
    let character: Character = load_saved_data_file();

    loop {
        // TODO: prompt user for continuation of `Event`s
        break;
    }

    Ok(())
}

mod tests {
    #![cfg(test)]

    use rstest::rstest;
    use super::*;

    #[rstest(
        token,
        expected,
        case(ReplaceableType::from_token("{creature}"), Some(ReplaceableType::Creature(Creature::new()))),
        case(ReplaceableType::from_token("{npc}"), Some(ReplaceableType::Npc(Npc::new()))),
        case(ReplaceableType::from_token("{npc_name}"), Some(ReplaceableType::NpcName(NpcName::new()))),
        case(ReplaceableType::from_token("{place}"), Some(ReplaceableType::Place(Place::new()))),
        case(ReplaceableType::from_token("{place_name}"), Some(ReplaceableType::PlaceName(PlaceName::new()))),
        case(ReplaceableType::from_token("{roll}"), Some(ReplaceableType::Roll(Roll::d6(0)))),
        case(ReplaceableType::from_token("invalid token"), None)
    )]
    fn test_replaceable_type_from_token(token: Option<ReplaceableType>, expected: Option<ReplaceableType>) {
        assert_eq!(token, expected);
    }

    #[rstest]
    fn test_event_push() {
        let mut event = Event::new();
        let token = EventToken::Text("Test".to_string());
        event.push(token);
        assert_eq!(event.tokens, vec![EventToken::Text("Test".to_string())]);
    }

    #[rstest]
    fn test_situation_type_from_str() {
        let raw_situation_type = "Test Situation Type".to_string();
        let situation_type = SituationType::from(&raw_situation_type);
        assert_eq!(situation_type.name, "Test Situation Type");
    }
}
