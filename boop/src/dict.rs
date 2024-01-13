//! Dictionary module
//!
//! This module provides structures and functions to work with the
//! boop dictionary code.  The primary purpose is to test the assembly
//! code.
#![warn(missing_docs)]

use core::fmt::{Debug, Formatter, Write};

use crate::error::Error;
use crate::pstring::{PString, PStringFunctions};

/// The different flags a dictionary word can have
#[derive(Debug, PartialEq)]
pub enum Flag {
    /// If the word is hidden, find won't return it.
    Hidden,
    /// Immediate mode
    Immediate,
}

/// A simple structure to hold a dictionary word.
/// This doesn't parse the structure itself, and isa placeholder to get testing
/// up and running.
/// This isn't the C representation, this is a higher level representation
/// that is separate from the assembly code.  For example, the word field
/// is a reference, not an array.
#[derive(Clone, Copy)]
pub struct Word {
    /// The link to the previous word in the dictionary
    pub link: u32,
    /// The word length
    pub length: u8,
    /// The word flags
    pub flags: &'static [Flag],
    /// The word itself
    pub word: &'static [u32],
    /// Pointer to the native object
    // This may be a bad practice.
    // We need to make sure they always stay in sync
    // It might be better to use "virtual attributes" that are generated
    // from the underlying representation
    pub ptr: *const u32,
}

impl Debug for Word {
    fn fmt(&self, f: &mut Formatter) -> core::fmt::Result {
        write!(f, "link: 0x{:04X}, ", self.link)?;
        write!(f, "length: {:?}, ", self.length)?;
        write!(f, "flags: {:?}, ", self.flags)?;
        write!(f, "word: {:?}", self.word)
    }
}

/// The settings for the dictionary
pub struct DictSettings {
    /// The dictionary address
    pub dictionary_addr: *mut &'static [u32],
    /// The dictionary bottom
    pub dictionary_size: u32,
}

/// Functions for the dictionary
/// Conveniently packaged as a structure to pass to tests
pub struct DictFunctions {
    /// Initialize the dictionary
    pub dict_init_safe: fn(*mut &'static [u32], u32),
    /// Find a word in the dictionary
    pub dict_find_safe: fn(&str) -> Result<Word, Error>,
    /// Initialize the dictionary
    pub dict_add_word_safe: fn(&str, &[Flag]) -> Result<(), Error>,
    /// Get the length of a word given a pointer to the flag field
    pub dict_word_length_safe: fn(*const u8) -> u8,
    /// Encode an ASCII character as UTF-32
    pub dict_encode_ascii_as_utf32_safe: fn(u32) -> Result<u32, Error>,
    /// Encode an ASCII string as a UTF-32 string
    pub dict_encode_ascii_string_as_utf32_safe: fn(&[u8], *mut u32) -> Result<(), Error>,
}

/// Functions for the dictionary-specific pstring functions
pub struct DictPStringFunctions {
    /// PString functions

    /// Copy a PString into a dictionary entry
    pub dict_pstring_copy_safe: fn(&mut Word, &mut PString) -> Result<(), Error>,

    /// Add a PString into a dictionary
    pub dict_pstring_add_pstring_safe: fn(&mut PString, &[Flag]) -> Result<(), Error>,
}

/// Run all the dictionary tests
pub fn run_tests(
    writer: &mut dyn Write,
    buffer: &mut [u32; 128],
    settings: DictSettings,
    functions: DictFunctions,
    pstring_functions: PStringFunctions,
    dict_pstring_functions: DictPStringFunctions,
) {
    tests::test_dict_word_length_works(writer, &functions);
    tests::test_dict_encode_ascii_as_utf32_works(writer, &functions);
    tests::test_dict_encode_ascii_string_as_utf32_works(writer, buffer, &functions);
    tests::test_dict_encode_ascii_string_as_utf32_too_short_fails(writer, buffer, &functions);
    tests::test_dict_encode_ascii_string_as_utf32_nonascii_fails(writer, buffer, &functions);
    tests::test_dict_find_empty_dictionary_fails(writer, &settings, &functions);
    tests::test_dict_add_word_too_long_fails(writer, &settings, &functions);
    tests::test_dict_add_word_too_long_by_two_fails(writer, &settings, &functions);
    tests::test_dict_add_word_too_short_fails(writer, &settings, &functions);
    tests::test_dict_add_word_oom_works(writer, &settings, &functions);
    tests::test_dict_add_word_oom_fails(writer, &settings, &functions);
    tests::test_dict_add_word_oom_one_less_fails(writer, &functions);
    tests::test_dict_add_word_works(writer, &settings, &functions);
    tests::test_dict_add_two_words_works(writer, &settings, &functions);
    tests::test_dict_add_hidden_word_works(writer, &settings, &functions);
    tests::test_dict_add_immediate_word_works(writer, &settings, &functions);
    tests::test_dict_add_chinese_word_works(writer, &settings, &functions);
    tests::test_number_of_characters_in_string_works(writer);

    // Test dict_pstring_copy functions work
    tests::test_dict_pstring_copy_works(
        writer,
        &settings,
        &pstring_functions,
        &functions,
        &dict_pstring_functions,
    );
    tests::test_dict_pstring_copy_too_short_fails(
        writer,
        &settings,
        &pstring_functions,
        &functions,
        &dict_pstring_functions,
    );
    tests::test_dict_pstring_copy_too_long_fails(
        writer,
        &settings,
        &pstring_functions,
        &functions,
        &dict_pstring_functions,
    );
    tests::test_dict_add_pstring_works(
        writer,
        &settings,
        &pstring_functions,
        &functions,
        &dict_pstring_functions,
    );
}

/// Get the number of characters in a word, which is different from
/// the Rust string length
pub fn number_of_characters_in_string(word: &str) -> u32 {
    word.chars().count() as u32
}

/// Test the dictionary code
#[allow(unused_imports)]
pub mod tests {
    use crate::{
        dict::{
            number_of_characters_in_string, DictFunctions, DictPStringFunctions, DictSettings,
            Flag, Word,
        },
        error::Error,
        pstring::{PString, PStringFunctions, PStringStruct},
        tests::write_test_result,
        ArrayHandle,
    };
    use core::fmt::Write;

    /// Test that finding a word in the dictionary works
    pub fn test_dict_add_word_works(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        let res = (functions.dict_add_word_safe)("STAR", &[]);

        match res {
            Ok(()) => {
                write!(writer, "SUCCESS: Succeeded adding word to dictionary\r\n").unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed adding word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the word was added
        let res = (functions.dict_find_safe)("STAR");

        match res {
            Ok(c) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded finding added word in dictionary\r\n"
                )
                .unwrap();
                // We get back a pointer to the word
                // Compare it again here to make sure it's the same word
                let word = c.word;

                write!(
                    writer,
                    "{} == {:?}, {} == {:?}, {} == {:?}, {} == {:?}\r\n",
                    0x53, word[0], 0x54, word[1], 0x41, word[2], 0x52, word[3]
                )
                .unwrap();

                assert_eq!(c.length, 0x04);
                assert_eq!(word[0], 0x53);
                assert_eq!(word[1], 0x54);
                assert_eq!(word[2], 0x41);
                assert_eq!(word[3], 0x52);
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed finding added word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Double check to make sure it doesn't find junk
        let res = (functions.dict_find_safe)("TEST");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "FAILURE: Should not find unknown word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Should not find unknown word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }
    }

    /// Test that finding two words to the dictionary works
    pub fn test_dict_add_two_words_works(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        let res = (functions.dict_add_word_safe)("STAR", &[]);

        match res {
            Ok(()) => {
                write!(writer, "SUCCESS: Succeeded adding word to dictionary\r\n").unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed adding word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the word was added
        let res = (functions.dict_find_safe)("STAR");

        match res {
            Ok(v) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded finding added word in dictionary\r\n"
                )
                .unwrap();
                writeln!(writer, "word found: {:?}", v).unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed finding added word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Double check to make sure it doesn't find junk
        let res = (functions.dict_find_safe)("TEST");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "FAILURE: Should not find unknown word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Should not find unknown word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Add a second word

        write!(writer, "Adding second word to dictionary\r\n").unwrap();
        let res = (functions.dict_add_word_safe)("STAR2", &[]);

        match res {
            Ok(()) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded adding second word to dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed adding second word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the first word is there
        let res = (functions.dict_find_safe)("STAR");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded finding first added word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed finding first added word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the second word is there
        let res = (functions.dict_find_safe)("STAR2");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded finding second added word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed finding second added word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Double check to make sure it doesn't find junk
        let res = (functions.dict_find_safe)("TEST");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "FAILURE: Should not find unknown word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Should not find unknown word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }
    }

    /// Test that finding two words to the dictionary works
    pub fn test_dict_add_hidden_word_works(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        let res = (functions.dict_add_word_safe)("STAR", &[]);

        match res {
            Ok(()) => {
                write!(writer, "SUCCESS: Succeeded adding word to dictionary\r\n").unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed adding word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the word was added
        let res = (functions.dict_find_safe)("STAR");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded finding added word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed finding added word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Double check to make sure it doesn't find junk
        let res = (functions.dict_find_safe)("TEST");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "FAILURE: Should not find unknown word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Should not find unknown word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Add a second word

        write!(writer, "Adding second word to dictionary\r\n").unwrap();
        let res = (functions.dict_add_word_safe)("STAR2", &[Flag::Hidden]);

        match res {
            Ok(()) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded adding second word to dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed adding second word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the first word is there
        let res = (functions.dict_find_safe)("STAR");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded finding first added word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed finding first added word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the second hidden word can't be found
        let res = (functions.dict_find_safe)("STAR2");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "FAILURE: Succeeded finding second hidden word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Failed finding second hidden word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Double check to make sure it doesn't find junk
        let res = (functions.dict_find_safe)("TEST");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "FAILURE: Should not find unknown word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Should not find unknown word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }
    }

    /// Test that adding and finding an immediate word works
    pub fn test_dict_add_immediate_word_works(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        let res = (functions.dict_add_word_safe)("STAR", &[]);

        match res {
            Ok(()) => {
                write!(writer, "SUCCESS: Succeeded adding word to dictionary\r\n").unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed adding word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the word was added
        let res = (functions.dict_find_safe)("STAR");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded finding added word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed finding added word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Double check to make sure it doesn't find junk
        let res = (functions.dict_find_safe)("TEST");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "FAILURE: Should not find unknown word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Should not find unknown word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Add a second word

        write!(writer, "Adding second word to dictionary\r\n").unwrap();
        let res = (functions.dict_add_word_safe)("αστέρι", &[Flag::Immediate]);

        match res {
            Ok(()) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded adding second word to dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed adding second word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the first word is there
        let res = (functions.dict_find_safe)("STAR");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded finding first added word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed finding first added word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the second hidden word can't be found
        let word = "αστέρι";
        let res = (functions.dict_find_safe)(word);

        match res {
            Ok(w) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded finding second immediate word {} in dictionary\r\n",
                    word
                )
                .unwrap();
                // Double check the word is a match
                for (i, c) in word.chars().enumerate() {
                    assert_eq!(c as u32, w.word[i]);
                }
                // Double check the length of the immediate word
                assert_eq!(w.length, 6);
                // Double check the flag is set
                assert!((*w.flags).contains(&Flag::Immediate));
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed finding second immediate word {} in dictionary: {}\r\n",
                    word, e
                )
                .unwrap();
            }
        }

        // Double check to make sure it doesn't find junk
        let res = (functions.dict_find_safe)("TEST");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "FAILURE: Should not find unknown word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Should not find unknown word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }
    }

    /// Test that adding a word that is too long fails
    pub fn test_dict_add_word_too_long_fails(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        write!(writer, "Testing add too long word to dictionary\r\n").unwrap();
        write!(writer, "Initializing dictionary\r\n").unwrap();
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        write!(writer, "Adding word to dictionary\r\n").unwrap();
        let s = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEF";
        write!(
            writer,
            "Length of word: {}\r\n",
            number_of_characters_in_string(s)
        )
        .unwrap();
        let res = (functions.dict_add_word_safe)(s, &[]);

        match res {
            Ok(()) => {
                write!(writer, "FAILURE: Succeeded adding word to dictionary\r\n").unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Failed adding word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
                assert_eq!(
                    e,
                    crate::error::Error::new(crate::error::ErrorKind::WordTooLong)
                );
            }
        }
    }

    /// Test that adding a word that is too long fails
    pub fn test_dict_add_word_too_long_by_two_fails(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        write!(
            writer,
            "Testing word too long by two charactesr to dictionary\r\n"
        )
        .unwrap();
        write!(writer, "Initializing dictionary\r\n").unwrap();
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        write!(writer, "Adding word to dictionary\r\n").unwrap();
        let s = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFG";
        write!(
            writer,
            "Length of word: {}\r\n",
            number_of_characters_in_string(s)
        )
        .unwrap();
        let res = (functions.dict_add_word_safe)(s, &[]);

        match res {
            Ok(()) => {
                write!(writer, "FAILURE: Succeeded adding word to dictionary\r\n").unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Failed adding word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
                assert_eq!(
                    e,
                    crate::error::Error::new(crate::error::ErrorKind::WordTooLong)
                );
            }
        }
    }

    /// Test that adding a word that is too short fails
    pub fn test_dict_add_word_too_short_fails(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        write!(writer, "Initializing dictionary\r\n").unwrap();
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        write!(writer, "Adding word to dictionary\r\n").unwrap();
        let res = (functions.dict_add_word_safe)("", &[]);

        match res {
            Ok(()) => {
                write!(writer, "FAILURE: Succeeded adding word to dictionary\r\n").unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Failed adding word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
                assert_eq!(
                    e,
                    crate::error::Error::new(crate::error::ErrorKind::WordTooShort)
                );
            }
        }
    }

    /// Test that OOM detection doesn't trigger at exactly the amount
    /// of memory.
    pub fn test_dict_add_word_oom_works(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        write!(writer, "Initializing dictionary\r\n").unwrap();
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        write!(writer, "Testing dictionary OOM\r\n").unwrap();

        for i in 1..=7 {
            let res = (functions.dict_add_word_safe)("ABCDEFGHIJKLMNOPQRSTUVWXYZABCDE", &[]);

            match res {
                Ok(()) => {
                    write!(writer, "SUCCESS: OOM test {} should work\r\n", i).unwrap();
                }
                Err(e) => {
                    write!(writer, "FAILURE: OOM test {} should work: {}\r\n", i, e).unwrap();
                    assert_eq!(
                        e,
                        crate::error::Error::new(crate::error::ErrorKind::OutOfMemory)
                    );
                }
            }
        }

        // This should get us to 1024 bytes exactly
        let res = (functions.dict_add_word_safe)("ABCDEFGHIJKLMNOPQRSTUVWXYZABC", &[]);

        match res {
            Ok(()) => {
                write!(writer, "SUCCESS: OOM test 8 should work\r\n").unwrap();
            }
            Err(e) => {
                write!(writer, "FAILURE: OOM test 8 should work: {}\r\n", e).unwrap();
                assert_eq!(
                    e,
                    crate::error::Error::new(crate::error::ErrorKind::OutOfMemory)
                );
            }
        }
    }

    /// Test that OOM detection triggers at more memory
    pub fn test_dict_add_word_oom_fails(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        write!(writer, "Initializing dictionary\r\n").unwrap();
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        write!(writer, "Testing dictionary OOM\r\n").unwrap();

        let s = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDE";

        for i in 1..=7 {
            let res = (functions.dict_add_word_safe)(s, &[]);

            match res {
                Ok(()) => {
                    write!(writer, "SUCCESS: OOM test {} should work\r\n", i).unwrap();
                }
                Err(e) => {
                    write!(writer, "FAILURE: OOM test should work: {}\r\n", e).unwrap();
                    assert_eq!(
                        e,
                        crate::error::Error::new(crate::error::ErrorKind::OutOfMemory)
                    );
                }
            }
        }

        // This should get us to 1028 bytes exactly
        let s = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCD";

        let res = (functions.dict_add_word_safe)(s, &[]);

        match res {
            Ok(()) => {
                write!(writer, "FAILURE: OOM test 8 should fail\r\n").unwrap();
            }
            Err(e) => {
                write!(writer, "SUCCESS: OOM test 8 should fail: {}\r\n", e).unwrap();
                assert_eq!(
                    e,
                    crate::error::Error::new(crate::error::ErrorKind::OutOfMemory)
                );
            }
        }
    }

    /// Test that OOM detection triggers at one less than the required
    /// amount of memory.
    pub fn test_dict_add_word_oom_one_less_fails(
        writer: &mut dyn Write,
        functions: &DictFunctions,
    ) {
        write!(writer, "Initializing dictionary\r\n").unwrap();
        // Initialize with a custom buffer to test off-by-one
        // dictionary storage
        // #[link_section = ".ram2bss"]
        #[allow(unused_mut)]
        let mut tmp_dictionary: [u8; 1023] = [0; 1023];

        (functions.dict_init_safe)(
            tmp_dictionary.as_ptr() as *mut &'static [u32],
            tmp_dictionary.len() as u32,
        );

        write!(writer, "Testing dictionary OOM off-by-one\r\n").unwrap();

        for i in 1..=7 {
            let res = (functions.dict_add_word_safe)("ABCDEFGHIJKLMNOPQRSTUVWXYZABCDE", &[]);

            match res {
                Ok(()) => {
                    write!(writer, "SUCCESS: OOM off-by-one test {} should work\r\n", i).unwrap();
                }
                Err(e) => {
                    write!(
                        writer,
                        "FAILURE: OOM off-by-one test {} should work: {}\r\n",
                        i, e,
                    )
                    .unwrap();
                    assert_eq!(
                        e,
                        crate::error::Error::new(crate::error::ErrorKind::OutOfMemory)
                    );
                }
            }
        }

        // This should get us to 1024 bytes exactly
        let res = (functions.dict_add_word_safe)("ABCDEFGHIJKLMNOPQRSTUVWXYZABC", &[]);

        match res {
            Ok(()) => {
                write!(writer, "FAILURE: OOM off-by-one test 8 should fail\r\n").unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: OOM off-by-one test 8 should fail: {}\r\n",
                    e
                )
                .unwrap();
                assert_eq!(
                    e,
                    crate::error::Error::new(crate::error::ErrorKind::OutOfMemory)
                );
            }
        }
    }

    /// Test that finding a word in the dictionary works
    pub fn test_dict_add_chinese_word_works(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        write!(writer, "Initializing dictionary\r\n").unwrap();
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        write!(
            writer,
            "Adding Chinese (traditional) word to dictionary\r\n"
        )
        .unwrap();

        let res = (functions.dict_add_word_safe)("星星", &[]);
        write!(
            writer,
            "Length of word: {}\r\n",
            number_of_characters_in_string("星星")
        )
        .unwrap();
        match res {
            Ok(()) => {
                write!(writer, "SUCCESS: Succeeded adding word to dictionary\r\n").unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed adding word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the word was added
        let res = (functions.dict_find_safe)("星星");

        match res {
            Ok(c) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded finding added word in dictionary\r\n"
                )
                .unwrap();

                let word = c.word;

                write!(
                    writer,
                    "{} == {:?}, {} == {:?}\r\n",
                    0x661F, word[0], 0x661F, word[1]
                )
                .unwrap();
                assert_eq!(c.length, 0x02);
                assert_eq!(word[0], 0x661F);
                assert_eq!(word[1], 0x661F);
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed finding added word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        write!(writer, "Trying to find an unknown word\r\n").unwrap();
        // Double check to make sure it doesn't find junk
        let res = (functions.dict_find_safe)("TEST");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "FAILURE: Should not find unknown word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Should not find unknown word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }
    }

    /// Test that encoding an ASCII character works
    pub fn test_dict_encode_ascii_as_utf32_works(
        writer: &mut dyn Write,
        functions: &DictFunctions,
    ) {
        write!(writer, "Testing ASCII 0: NUL\r\n").unwrap();
        let character: u32 = 0;
        let res = (functions.dict_encode_ascii_as_utf32_safe)(character);
        match res {
            Ok(c) => {
                write!(writer, "SUCCESS character: {}\r\n", c).unwrap();
                assert_eq!(c, 0);
            }
            Err(_e) => {
                write!(writer, "FAILURE ASCII 0 should be ok\r\n").unwrap();
            }
        }

        write!(writer, "Testing ASCII 126: V\r\n").unwrap();
        let character: u32 = 0x7E;
        let res = (functions.dict_encode_ascii_as_utf32_safe)(character);
        match res {
            Ok(c) => {
                write!(writer, "SUCCESS character: {}\r\n", c).unwrap();
                assert_eq!(c, 0x7E);
            }
            Err(_e) => {
                write!(writer, "FAILURE ASCII 126 should be ok\r\n").unwrap();
            }
        }

        write!(writer, "Testing ASCII 127: W\r\n").unwrap();
        let character: u32 = 0x7F;
        let res = (functions.dict_encode_ascii_as_utf32_safe)(character);
        match res {
            Ok(c) => {
                write!(writer, "SUCCESS character: {}\r\n", c).unwrap();
                assert_eq!(c, 0x7F);
            }
            Err(_e) => {
                write!(writer, "FAILURE ASCII 127 should be ok\r\n").unwrap();
            }
        }

        write!(writer, "Testing character value 128\r\n").unwrap();
        let character: u32 = 0x80;
        let res = (functions.dict_encode_ascii_as_utf32_safe)(character);
        match res {
            Ok(_c) => {
                write!(writer, "FAILURE: Should have failed\r\n").unwrap();
            }
            Err(_e) => {
                write!(
                    writer,
                    "SUCCESS Should have failed with non-ASCII character\r\n"
                )
                .unwrap();
            }
        }
    }

    /// Test that encoding an ASCII character works
    pub fn test_dict_encode_ascii_string_as_utf32_works(
        writer: &mut dyn Write,
        buffer: &mut [u32; 128],
        functions: &DictFunctions,
    ) {
        // if unsafe { buffer.len() } < word_len {
        //     return Err(boop::error::Error::new(boop::error::ErrorKind::WordTooLong));
        // }

        let dst = buffer.as_mut_ptr();

        write!(writer, "Testing encoding ASCII string STAR\r\n").unwrap();
        // let src: [u8; 3] = [0x42, 0x41, 0x50];
        let src = "STAR";
        assert!(src.is_ascii());

        let res = (functions.dict_encode_ascii_string_as_utf32_safe)(src.as_bytes(), dst);
        match res {
            Ok(_c) => {
                write!(writer, "SUCCESS: Should have succeeded\r\n").unwrap();

                for (i, d) in buffer.iter().enumerate().take(src.len()) {
                    let c = src.as_bytes()[i];
                    if c as u32 == *d {
                        write!(writer, "SUCCESS {} == {}\r\n", c, *d).unwrap();
                    } else {
                        write!(writer, "FAILURE {} != {}\r\n", c, *d).unwrap();
                    }
                }
            }
            Err(_e) => {
                write!(
                    writer,
                    "FAILURE Should have succeeded with ASCII character\r\n"
                )
                .unwrap();
            }
        }
    }

    /// Test that encoding an ASCII character works
    pub fn test_dict_encode_ascii_string_as_utf32_too_short_fails(
        writer: &mut dyn Write,
        buffer: &mut [u32; 128],
        functions: &DictFunctions,
    ) {
        let dst = buffer.as_mut_ptr();

        write!(writer, "Testing encoding zero length string\r\n").unwrap();
        let src = "";
        assert!(src.is_ascii());

        let res = (functions.dict_encode_ascii_string_as_utf32_safe)(src.as_bytes(), dst);
        match res {
            Ok(_c) => {
                write!(writer, "FAILURE: Should have failed\r\n").unwrap();
            }
            Err(_e) => {
                write!(
                    writer,
                    "SUCCESS Should have failed with zero length string\r\n"
                )
                .unwrap();
            }
        }
    }

    /// Test that encoding an ASCII character works
    pub fn test_dict_encode_ascii_string_as_utf32_nonascii_fails(
        writer: &mut dyn Write,
        buffer: &mut [u32; 128],
        functions: &DictFunctions,
    ) {
        let dst = buffer.as_mut_ptr();

        write!(writer, "Testing encoding non-ascii string\r\n").unwrap();
        let src = "réalta";
        let mut bytes: [u8; 6] = [0; 6];
        for (i, c) in src.chars().enumerate() {
            bytes[i] = c as u8;
        }

        let res = (functions.dict_encode_ascii_string_as_utf32_safe)(&bytes, dst);
        match res {
            Ok(_c) => {
                write!(writer, "FAILURE: Should have failed\r\n").unwrap();
            }
            Err(_e) => {
                write!(
                    writer,
                    "SUCCESS Should have failed with non-ascii string\r\n"
                )
                .unwrap();
            }
        }
    }

    /// Test that trying to find a word in an empty dictionary fails
    pub fn test_dict_find_empty_dictionary_fails(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        write!(writer, "Initializing dictionary\r\n").unwrap();
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        write!(writer, "Searching for word in empty dictionary\r\n").unwrap();

        let s = "STAR";
        let res = (functions.dict_find_safe)(s);

        match res {
            Ok(_) => {
                write!(
                    writer,
                    "FAILURE: Should not find word in empty dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Should not find word in empty dictionary: {}\r\n",
                    e
                )
                .unwrap();
                assert_eq!(
                    e,
                    crate::error::Error::new(crate::error::ErrorKind::WordNotFound)
                );
            }
        }
    }

    /// Test that finding the length of a dictionary word works
    pub fn test_dict_word_length_works(writer: &mut dyn Write, functions: &DictFunctions) {
        write!(writer, "Testing word length of zero\r\n").unwrap();
        let flag_field: u8 = 0b00000000;
        let len = (functions.dict_word_length_safe)(&flag_field);
        write!(writer, "length: {}\r\n", len).unwrap();
        assert_eq!(len, 0);

        write!(writer, "Testing word length of three\r\n").unwrap();
        let flag_field: u8 = 0b00000011;
        let len = (functions.dict_word_length_safe)(&flag_field);
        write!(writer, "length: {}\r\n", len).unwrap();
        assert_eq!(len, 3);

        write!(writer, "Testing word length of three with other flags\r\n").unwrap();
        let flag_field: u8 = 0b11100011;
        let len = (functions.dict_word_length_safe)(&flag_field);
        write!(writer, "length: {}\r\n", len).unwrap();
        assert_eq!(len, 3);
    }

    struct UnicodeTest<'a> {
        language: &'a str,
        word: &'a str,
        num_chars: u32,
    }

    /// Test counting the number of characters in various strings works
    pub fn test_number_of_characters_in_string_works(writer: &mut dyn Write) {
        let tests: [UnicodeTest; 8] = [
            UnicodeTest {
                language: "English",
                word: "STAR",
                num_chars: 4,
            },
            UnicodeTest {
                language: "Chinese (Traditional)",
                word: "星星",
                num_chars: 2,
            },
            UnicodeTest {
                language: "Japanese",
                word: "スタ",
                num_chars: 2,
            },
            UnicodeTest {
                language: "Hindi",
                word: "सितारा",
                num_chars: 6,
            },
            UnicodeTest {
                language: "Russian",
                word: "звезда",
                num_chars: 6,
            },
            UnicodeTest {
                language: "Hebrew",
                word: "כוכב",
                num_chars: 4,
            },
            UnicodeTest {
                language: "Punjabi",
                word: "ਤਾਰਾ",
                num_chars: 4,
            },
            UnicodeTest {
                language: "Arabic",
                word: "نجمة",
                num_chars: 4,
            },
        ];

        for test in tests {
            let s = test.word;
            let n = number_of_characters_in_string(s);

            if n == test.num_chars {
                write!(
                    writer,
                    "SUCCESS Testing language {}: number of characters in {}: {} == {}\r\n",
                    test.language, test.word, test.num_chars, n
                )
                .unwrap();
            } else {
                write!(
                    writer,
                    "FAILURE Testing language {}: number of characters in {}: {} != {}\r\n",
                    test.language, test.word, test.num_chars, n
                )
                .unwrap();
            }

            assert_eq!(n, test.num_chars);
        }
    }

    /// Test that copying a pstring into a dictionary works
    pub fn test_dict_pstring_copy_works(
        writer: &mut dyn Write,
        settings: &DictSettings,
        pstring_functions: &PStringFunctions,
        dict_functions: &DictFunctions,
        dict_pstring_functions: &DictPStringFunctions,
    ) {
        // array of u32 with length 32
        // 4 bytes for the length, plus 31 u32s (124 bytes) for the character data
        let mut arr: [u32; 32] = [0; 32];

        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        // initialize the pstring
        let mut pstring = PString::new(&handle, pstring_functions).unwrap();

        pstring.put('S' as u32).unwrap();
        pstring.put('T' as u32).unwrap();
        pstring.put('A' as u32).unwrap();
        pstring.put('R' as u32).unwrap();

        // Initialize the dictionary
        (dict_functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        let res = (dict_functions.dict_add_word_safe)("BLRG", &[]);
        write_test_result(
            writer,
            res.is_ok(),
            "dict::tests::test_dict_pstring_copy_works should add original word",
        );
        assert!(res.is_ok());

        // Make sure the word was added
        let res = (dict_functions.dict_find_safe)("BLRG");
        write_test_result(
            writer,
            res.is_ok(),
            "dict::tests::test_dict_pstring_copy_works should find original word",
        );
        assert!(res.is_ok());

        // Replace the word now
        // We know the first word is the start of the dictionary
        let _dict_ptr = settings.dictionary_addr as *const u8;

        let mut word = res.unwrap();

        let res = (dict_pstring_functions.dict_pstring_copy_safe)(&mut word, &mut pstring);

        match res {
            Ok(()) => {
                write!(writer, "SUCCESS: Succeeded adding word to dictionary\r\n").unwrap();

                // We get back a pointer to the word
                // Compare it again here to make sure it's the same word
                let res = (dict_functions.dict_find_safe)("STAR");
                assert!(res.is_ok());
                let c = res.unwrap();
                let word = c.word;

                write!(
                    writer,
                    "{} == {:?}, {} == {:?}, {} == {:?}, {} == {:?}\r\n",
                    0x53, word[0], 0x54, word[1], 0x41, word[2], 0x52, word[3]
                )
                .unwrap();

                assert_eq!(c.length, 0x04);
                writeln!(writer, "SUCCESS: Length is correct").unwrap();
                assert!(c.flags.is_empty());
                writeln!(writer, "SUCCESS: Flags are correct").unwrap();
                assert_eq!(word[0], 0x53);
                assert_eq!(word[1], 0x54);
                assert_eq!(word[2], 0x41);
                assert_eq!(word[3], 0x52);
                writeln!(writer, "SUCCESS: Content is correct").unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed adding word to dictionary: {:?}\r\n",
                    e
                )
                .unwrap();
            }
        }
    }

    /// Test that copying a too-long pstring into a dictionary fails
    pub fn test_dict_pstring_copy_too_short_fails(
        writer: &mut dyn Write,
        settings: &DictSettings,
        pstring_functions: &PStringFunctions,
        dict_functions: &DictFunctions,
        dict_pstring_functions: &DictPStringFunctions,
    ) {
        // array of u32 with length 32
        // 4 bytes for the length, plus 31 u32s (124 bytes) for the character data
        let mut arr: [u32; 32] = [0; 32];

        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        // initialize the pstring
        let mut pstring = PString::new(&handle, pstring_functions).unwrap();

        pstring.put('S' as u32).unwrap();
        pstring.put('T' as u32).unwrap();
        pstring.put('A' as u32).unwrap();

        // Initialize the dictionary
        (dict_functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        let res = (dict_functions.dict_add_word_safe)("BLRG", &[]);
        write_test_result(
            writer,
            res.is_ok(),
            "dict::tests::test_dict_pstring_copy_too_short_fails should add original word",
        );
        assert!(res.is_ok());

        // Make sure the word was added
        let res = (dict_functions.dict_find_safe)("BLRG");
        write_test_result(
            writer,
            res.is_ok(),
            "dict::tests::test_dict_pstring_copy_too_short_fails should find original word",
        );
        assert!(res.is_ok());

        // Replace the word now
        // We know the first word is the start of the dictionary
        let _dict_ptr = settings.dictionary_addr as *const u8;

        let mut word = res.unwrap();

        let res = (dict_pstring_functions.dict_pstring_copy_safe)(&mut word, &mut pstring);
        write_test_result(
            writer,
            res.is_err(),
            "dict::tests::test_dict_pstring_copy_too_short_fails should fail",
        );
    }

    /// Test that copying a too-long pstring into a dictionary fails
    pub fn test_dict_pstring_copy_too_long_fails(
        writer: &mut dyn Write,
        settings: &DictSettings,
        pstring_functions: &PStringFunctions,
        dict_functions: &DictFunctions,
        dict_pstring_functions: &DictPStringFunctions,
    ) {
        // array of u32 with length 32
        // 4 bytes for the length, plus 31 u32s (124 bytes) for the character data
        let mut arr: [u32; 32] = [0; 32];

        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        // initialize the pstring
        let mut pstring = PString::new(&handle, pstring_functions).unwrap();

        pstring.put('S' as u32).unwrap();
        pstring.put('T' as u32).unwrap();
        pstring.put('A' as u32).unwrap();
        pstring.put('R' as u32).unwrap();
        pstring.put('S' as u32).unwrap();

        // Initialize the dictionary
        (dict_functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        let res = (dict_functions.dict_add_word_safe)("BLRG", &[]);
        write_test_result(
            writer,
            res.is_ok(),
            "dict::tests::test_dict_pstring_copy_too_long_fails should add original word",
        );
        assert!(res.is_ok());

        // Make sure the word was added
        let res = (dict_functions.dict_find_safe)("BLRG");
        write_test_result(
            writer,
            res.is_ok(),
            "dict::tests::test_dict_pstring_copy_too_long_fails should find original word",
        );
        assert!(res.is_ok());

        // Replace the word now
        // We know the first word is the start of the dictionary
        let _dict_ptr = settings.dictionary_addr as *const u8;

        let mut word = res.unwrap();

        let res = (dict_pstring_functions.dict_pstring_copy_safe)(&mut word, &mut pstring);
        write_test_result(
            writer,
            res.is_err(),
            "dict::tests::test_dict_pstring_copy_too_long_fails should fail",
        );
    }

    /// Test that finding a word in the dictionary works
    pub fn test_dict_add_pstring_works(
        writer: &mut dyn Write,
        settings: &DictSettings,
        pstring_functions: &PStringFunctions,
        functions: &DictFunctions,
        dict_pstring_functions: &DictPStringFunctions,
    ) {
        // array of u32 with length 32
        // 4 bytes for the length, plus 31 u32s (124 bytes) for the character data
        let mut arr: [u32; 32] = [0; 32];

        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        // initialize the pstring
        let mut pstring = PString::new(&handle, pstring_functions).unwrap();

        pstring.put('S' as u32).unwrap();
        pstring.put('T' as u32).unwrap();
        pstring.put('A' as u32).unwrap();
        pstring.put('R' as u32).unwrap();

        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        let res = (dict_pstring_functions.dict_pstring_add_pstring_safe)(&mut pstring, &[]);

        match res {
            Ok(()) => {
                write!(writer, "SUCCESS: Succeeded adding word to dictionary\r\n").unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed adding word to dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Make sure the word was added
        let res = (functions.dict_find_safe)("STAR");

        match res {
            Ok(c) => {
                write!(
                    writer,
                    "SUCCESS: Succeeded finding added word in dictionary\r\n"
                )
                .unwrap();
                // We get back a pointer to the word
                // Compare it again here to make sure it's the same word
                let word = c.word;

                write!(
                    writer,
                    "{} == {:?}, {} == {:?}, {} == {:?}, {} == {:?}\r\n",
                    0x53, word[0], 0x54, word[1], 0x41, word[2], 0x52, word[3]
                )
                .unwrap();

                assert_eq!(c.length, 0x04);
                assert_eq!(word[0], 0x53);
                assert_eq!(word[1], 0x54);
                assert_eq!(word[2], 0x41);
                assert_eq!(word[3], 0x52);
            }
            Err(e) => {
                write!(
                    writer,
                    "FAILURE: Failed finding added word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }

        // Double check to make sure it doesn't find junk
        let res = (functions.dict_find_safe)("TEST");

        match res {
            Ok(_v) => {
                write!(
                    writer,
                    "FAILURE: Should not find unknown word in dictionary\r\n"
                )
                .unwrap();
            }
            Err(e) => {
                write!(
                    writer,
                    "SUCCESS: Should not find unknown word in dictionary: {}\r\n",
                    e
                )
                .unwrap();
            }
        }
    }
}
