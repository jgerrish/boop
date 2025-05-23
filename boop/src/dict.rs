//! Dictionary module
//!
//! This module provides structures and functions to work with the
//! boop dictionary code.  The primary purpose is to test the assembly
//! code.
#![warn(missing_docs)]

use core::fmt::Write;

use crate::error::Error;

/// The different flags a dictionary word can have
#[derive(PartialEq)]
pub enum Flag {
    /// If the word is hidden, find won't return it.
    Hidden,
    /// Immediate mode
    Immediate,
}

/// A simple structure to hold a dictionary word.
/// This doesn't parse the structure itself, and isa placeholder to get testing
/// up and running.
#[repr(C, align(1))]
pub struct Word {
    /// The link to the previous word in the dictionary
    pub link: u32,
    /// The word length
    pub length: u8,
    /// The word flags
    pub flags: &'static [Flag],
    /// The word itself
    // Rust now does checks on aligned data for core::slice::from_raw_parts
    // Our words aren't always aligned on 32-bit boundaries.
    //
    // So we have to do a complete serialization / deserialization
    // In the meantime, until the rewrite, we can use raw pointers.
    pub word: *const u32,
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

/// Run all the dictionary tests
pub fn run_tests(
    writer: &mut dyn Write,
    buffer: &mut [u32; 128],
    settings: DictSettings,
    functions: DictFunctions,
) {
    tests::test_dict_word_length_works(writer, &functions);
    tests::test_dict_encode_ascii_as_utf32_works(writer, &functions);
    tests::test_dict_encode_ascii_string_as_utf32_works(writer, buffer, &functions);
    tests::test_dict_encode_ascii_string_as_utf32_too_short_fails(writer, buffer, &functions);
    tests::test_dict_encode_ascii_string_as_utf32_nonascii_fails(writer, buffer, &functions);
    tests::test_dict_find_empty_dictionary_fails(writer, &settings, &functions);
    tests::test_dict_add_word_too_long_fails(writer, &settings, &functions);
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
        dict::{number_of_characters_in_string, DictFunctions, DictSettings, Flag, Word},
        error::Error,
    };
    use core::fmt::Write;

    /// Test that finding a word in the dictionary works
    pub fn test_dict_add_word_works(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        write!(
            writer,
            "test_dict_add_word_works::Initializing dictionary\r\n"
        )
        .unwrap();
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        write!(writer, "Adding word to dictionary\r\n").unwrap();
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

                // We do this check even though we know in this
                // application we are not likely to go over the limit.
                // We're using generated test data that is small.
                // But it might get loaded on some test device that is
                // running a large OS or runtime.
                //
                // WARNING: In a future application, we might want to
                // wrap these operations in a lock or other
                // protection.  Future versions with threads will need
                // to take that into account.  That will be tested or
                // documented.
                assert!(word.wrapping_add(3) > word);

                write!(
                    writer,
                    "{} == {:?}, {} == {:?}, {} == {:?}, {} == {:?}\r\n",
                    0x53,
                    unsafe { *word },
                    0x54,
                    unsafe { *(word.wrapping_add(1)) },
                    0x41,
                    unsafe { *(word.wrapping_add(2)) },
                    0x52,
                    unsafe { *(word.wrapping_add(3)) }
                )
                .unwrap();

                assert_eq!(c.length, 0x04);
                assert_eq!(unsafe { *word }, 0x53);
                assert_eq!(unsafe { *(word.wrapping_add(1)) }, 0x54);
                assert_eq!(unsafe { *(word.wrapping_add(2)) }, 0x41);
                assert_eq!(unsafe { *(word.wrapping_add(3)) }, 0x52);
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
        write!(
            writer,
            "test_dict_add_two_words_works::Initializing dictionary\r\n"
        )
        .unwrap();
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        write!(
            writer,
            "test_dict_add_two_words_works::Adding word to dictionary\r\n"
        )
        .unwrap();
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
        write!(
            writer,
            "test_dict_add_hidden_word_works::Initializing dictionary\r\n"
        )
        .unwrap();
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        write!(writer, "Adding word to dictionary\r\n").unwrap();
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
        write!(
            writer,
            "test_dict_add_immediate_word_works::Initializing dictionary\r\n"
        )
        .unwrap();
        (functions.dict_init_safe)(settings.dictionary_addr, settings.dictionary_size);

        write!(writer, "Adding word to dictionary\r\n").unwrap();
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
                    if i > 0 {
                        assert!(w.word.wrapping_add(i) > w.word);
                    }
                    assert_eq!(c as u32, unsafe { *(w.word.wrapping_add(i)) });
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
        write!(
            writer,
            "test_dict_add_word_too_long_fails::Testing add too long word to dictionary\r\n"
        )
        .unwrap();
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

    /// Test that adding a word that is too short fails
    pub fn test_dict_add_word_too_short_fails(
        writer: &mut dyn Write,
        settings: &DictSettings,
        functions: &DictFunctions,
    ) {
        write!(
            writer,
            "test_dict_add_word_too_short_fails::Initializing dictionary\r\n"
        )
        .unwrap();
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

                assert!(word.wrapping_add(1) > word);

                write!(
                    writer,
                    "{} == {:?}, {} == {:?}\r\n",
                    0x661F,
                    unsafe { *word },
                    0x661F,
                    unsafe { *(word.wrapping_add(1)) }
                )
                .unwrap();
                assert_eq!(c.length, 0x02);
                assert_eq!(unsafe { *word }, 0x661F);
                assert_eq!(unsafe { *(word.wrapping_add(1)) }, 0x661F);
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
}
