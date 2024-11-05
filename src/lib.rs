//! A library to expand variables in the style of Kubernetes pod manifests.

use std::{cell::RefCell, collections::HashMap};

const OPERATOR: char = '$';
const REFERENCE_OPENER: char = '(';
const REFERENCE_CLOSER: char = ')';

fn syntax_wrap(input: &str) -> String {
    format!(
        "{}{}{}{}",
        OPERATOR, REFERENCE_OPENER, input, REFERENCE_CLOSER
    )
}

pub fn mapping_func_for<'a>(
    context: &'a [&'a HashMap<String, RefCell<String>>],
) -> impl Fn(&str) -> String + 'a {
    move |input: &str| -> String {
        for vars in context.iter() {
            if let Some(val) = vars.get(input) {
                return val.borrow().clone();
            }
        }
        syntax_wrap(input)
    }
}

pub fn expand(input: &str, mapping: impl Fn(&str) -> String) -> String {
    let mut buf = String::with_capacity(16);
    let mut checkpoint = 0;
    let mut cursor = 0;
    let mut chars = input.chars();
    let mut chars_cursor = 0;

    while cursor < input.len() {
        if chars.nth(chars_cursor) == Some(OPERATOR) && cursor + 1 < input.len() {
            buf.push_str(&input[checkpoint..cursor]);

            let (read, is_var, advance) = try_read_variable_name(&input[cursor + 1..]);
            if is_var {
                buf.push_str(&mapping(&read));
            } else {
                buf.push_str(&read);
            }

            chars_cursor = advance;
            cursor += advance;
            checkpoint = cursor + 1;
        } else {
            chars_cursor = 0;
        }
        cursor += 1;
    }

    buf.push_str(&input[checkpoint..]);
    buf
}

fn try_read_variable_name(input: &str) -> (String, bool, usize) {
    let mut chars = input.chars();
    let next = chars.next().unwrap();
    match next {
        OPERATOR => (OPERATOR.into(), false, 1),
        REFERENCE_OPENER => {
            for (i, c) in chars.enumerate() {
                if c == REFERENCE_CLOSER {
                    return (input[1..i + 1].into(), true, i + 2);
                }
            }
            (format!("{}{}", OPERATOR, REFERENCE_OPENER), false, 1)
        }
        _ => (format!("{}{}", OPERATOR, next), false, 1),
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use super::*;
    use pretty_assertions::assert_eq;

    struct NameValue<'a> {
        name: &'a str,
        value: &'a str,
    }

    #[test]
    fn test_map_reference() {
        let envs = vec![
            NameValue {
                name: "FOO",
                value: "bar",
            },
            NameValue {
                name: "ZOO",
                value: "$(FOO)-1",
            },
            NameValue {
                name: "BLU",
                value: "$(ZOO)-2",
            },
        ];

        let declared_env: HashMap<String, RefCell<String>> = HashMap::from_iter(
            vec![("FOO", "bar"), ("ZOO", "$(FOO)-1"), ("BLU", "$(ZOO)-2")]
                .into_iter()
                .map(|(k, v)| (k.to_string(), v.to_string().into())),
        );

        let maps = vec![&declared_env];
        let mapping = mapping_func_for(&maps);

        for entry in envs {
            let vrc = declared_env.get(entry.name).unwrap();
            *vrc.borrow_mut() = expand(entry.value.into(), &mapping);
        }

        let expected_env: HashMap<String, String> = HashMap::from_iter(
            vec![("FOO", "bar"), ("ZOO", "bar-1"), ("BLU", "bar-1-2")]
                .into_iter()
                .map(|(k, v)| (k.to_string(), v.to_string())),
        );

        for (k, v) in expected_env {
            let expanded = declared_env.get(&k).unwrap().borrow();
            assert_eq!(*expanded, v);
        }
    }

    #[test]
    fn test_mapping() {
        let context: HashMap<String, RefCell<String>> = HashMap::from_iter(
            vec![
                ("VAR_A", "A"),
                ("VAR_B", "B"),
                ("VAR_C", "C"),
                ("VAR_REF", "$(VAR_A)"),
                ("VAR_EMPTY", ""),
            ]
            .into_iter()
            .map(|(k, v)| (k.to_string(), v.to_string().into())),
        );
        let maps = vec![&context];
        let mapping = mapping_func_for(&maps);

        do_expansion_test(mapping)
    }

    #[test]
    fn test_mapping_dual() {
        let context: HashMap<String, RefCell<String>> = HashMap::from_iter(
            vec![("VAR_A", "A"), ("VAR_EMPTY", "")]
                .into_iter()
                .map(|(k, v)| (k.to_string(), v.to_string().into())),
        );
        let context2: HashMap<String, RefCell<String>> = HashMap::from_iter(
            vec![("VAR_B", "B"), ("VAR_C", "C"), ("VAR_REF", "$(VAR_A)")]
                .into_iter()
                .map(|(k, v)| (k.to_string(), v.to_string().into())),
        );
        let maps = vec![&context, &context2];
        let mapping = mapping_func_for(&maps);

        do_expansion_test(mapping)
    }

    fn do_expansion_test(mapping: impl Fn(&str) -> String) {
        struct Case<'a> {
            name: &'a str,
            input: &'a str,
            expected: &'a str,
        }
        let cases = vec![
            Case {
                name: "whole string",
                input: "$(VAR_A)",
                expected: "A",
            },
            Case {
                name: "repeat",
                input: "$(VAR_A)-$(VAR_A)",
                expected: "A-A",
            },
            Case {
                name: "beginning",
                input: "$(VAR_A)-1",
                expected: "A-1",
            },
            Case {
                name: "middle",
                input: "___$(VAR_B)___",
                expected: "___B___",
            },
            Case {
                name: "end",
                input: "___$(VAR_C)",
                expected: "___C",
            },
            Case {
                name: "compound",
                input: "$(VAR_A)_$(VAR_B)_$(VAR_C)",
                expected: "A_B_C",
            },
            Case {
                name: "escape & expand",
                input: "$$(VAR_B)_$(VAR_A)",
                expected: "$(VAR_B)_A",
            },
            Case {
                name: "compound escape",
                input: "$$(VAR_A)_$$(VAR_B)",
                expected: "$(VAR_A)_$(VAR_B)",
            },
            Case {
                name: "mixed in escapes",
                input: "f000-$$VAR_A",
                expected: "f000-$VAR_A",
            },
            Case {
                name: "backslash escape ignored",
                input: "foo\\$(VAR_C)bar",
                expected: "foo\\Cbar",
            },
            Case {
                name: "backslash escape ignored",
                input: "foo\\\\$(VAR_C)bar",
                expected: "foo\\\\Cbar",
            },
            Case {
                name: "lots of backslashes",
                input: "foo\\\\\\\\$(VAR_A)bar",
                expected: "foo\\\\\\\\Abar",
            },
            Case {
                name: "nested var references",
                input: "$(VAR_A$(VAR_B))",
                expected: "$(VAR_A$(VAR_B))",
            },
            Case {
                name: "nested var references second type",
                input: "$(VAR_A$(VAR_B)",
                expected: "$(VAR_A$(VAR_B)",
            },
            Case {
                name: "value is a reference",
                input: "$(VAR_REF)",
                expected: "$(VAR_A)",
            },
            Case {
                name: "value is a reference x 2",
                input: "%%$(VAR_REF)--$(VAR_REF)%%",
                expected: "%%$(VAR_A)--$(VAR_A)%%",
            },
            Case {
                name: "empty var",
                input: "foo$(VAR_EMPTY)bar",
                expected: "foobar",
            },
            Case {
                name: "unterminated expression",
                input: "foo$(VAR_Awhoops!",
                expected: "foo$(VAR_Awhoops!",
            },
            Case {
                name: "expression without operator",
                input: "f00__(VAR_A)__",
                expected: "f00__(VAR_A)__",
            },
            Case {
                name: "shell special vars pass through",
                input: "$?_boo_$!",
                expected: "$?_boo_$!",
            },
            Case {
                name: "bare operators are ignored",
                input: "$VAR_A",
                expected: "$VAR_A",
            },
            Case {
                name: "undefined vars are passed through",
                input: "$(VAR_DNE)",
                expected: "$(VAR_DNE)",
            },
            Case {
                name: "multiple (even) operators, var undefined",
                input: "$$$$$$(BIG_MONEY)",
                expected: "$$$(BIG_MONEY)",
            },
            Case {
                name: "multiple (even) operators, var defined",
                input: "$$$$$$(VAR_A)",
                expected: "$$$(VAR_A)",
            },
            Case {
                name: "multiple (odd) operators, var undefined",
                input: "$$$$$$$(GOOD_ODDS)",
                expected: "$$$$(GOOD_ODDS)",
            },
            Case {
                name: "multiple (odd) operators, var defined",
                input: "$$$$$$$(VAR_A)",
                expected: "$$$A",
            },
            Case {
                name: "missing open expression",
                input: "$VAR_A)",
                expected: "$VAR_A)",
            },
            Case {
                name: "shell syntax ignored",
                input: "${VAR_A}",
                expected: "${VAR_A}",
            },
            Case {
                name: "trailing incomplete expression not consumed",
                input: "$(VAR_B)_______$(A",
                expected: "B_______$(A",
            },
            Case {
                name: "trailing incomplete expression, no content, is not consumed",
                input: "$(VAR_C)_______$(",
                expected: "C_______$(",
            },
            Case {
                name: "operator at end of input string is preserved",
                input: "$(VAR_A)foobarzab$",
                expected: "Afoobarzab$",
            },
            Case {
                name: "shell escaped incomplete expr",
                input: "foo-\\$(VAR_A",
                expected: "foo-\\$(VAR_A",
            },
            Case {
                name: "lots of $( in middle",
                input: "--$($($($($--",
                expected: "--$($($($($--",
            },
            Case {
                name: "lots of $( in beginning",
                input: "$($($($($--foo$(",
                expected: "$($($($($--foo$(",
            },
            Case {
                name: "lots of $( at end",
                input: "foo0--$($($($(",
                expected: "foo0--$($($($(",
            },
            Case {
                name: "escaped operators in variable names are not escaped",
                input: "$(foo$$var)",
                expected: "$(foo$$var)",
            },
            Case {
                name: "newline not expanded",
                input: "\n",
                expected: "\n",
            },
        ];

        for case in cases {
            let expanded = expand(case.input.into(), &mapping);
            assert_eq!(expanded, case.expected, "{}", case.name);
        }
    }
}
