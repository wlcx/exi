#[derive(Debug, PartialEq)]
pub enum Alignment {
    BitPacked,
    ByteAlignment,
    PreCompression,
}

#[derive(Debug, PartialEq)]
pub struct FidelityOptions {
    pub comments: bool,
    pub pis: bool,
    pub dtd: bool,
    pub prefixes: bool,
    pub lexical_values: bool,
}

impl Default for FidelityOptions {
    fn default() -> Self {
        FidelityOptions {
            comments: false,
            pis: false,
            dtd: false,
            prefixes: false,
            lexical_values: false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Options {
    pub alignment: Alignment,
    pub compression: bool,
    pub strict: bool,
    pub fragment: bool,
    pub preserve: FidelityOptions,
    pub self_contained: bool,
    pub schema_id: (),
    pub datatype_representation_map: (),
    pub block_size: usize,
    pub value_max_length: Option<usize>,
    pub value_partition_capacity: Option<usize>,
    pub user_defined: (),
}

impl Default for Options {
    fn default() -> Self {
        Options {
            alignment: Alignment::BitPacked,
            compression: false,
            strict: false,
            fragment: false,
            preserve: FidelityOptions::default(),
            self_contained: false,
            schema_id: (),
            datatype_representation_map: (),
            block_size: 1_000_000,
            value_max_length: None,
            value_partition_capacity: None,
            user_defined: (),
        }
    }
}
