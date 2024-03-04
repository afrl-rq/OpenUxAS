import difflib
import re
import argparse


def normalize_string(s: str) -> list[str]:
    # Prepare the string for diffing
    return re.sub(
        r'[ ]+',
        " ",
        ",\n".join(s.lower().split(","))
    ).splitlines(keepends=True)


def diff_strings(string1: str, string2: str) -> str:
    # Normalize the strings
    s1_norm = normalize_string(string1)
    s2_norm = normalize_string(string2)

    # Check if the normalized strings are equal
    if s1_norm == s2_norm:
        return "The strings are the same."

    # Use difflib to generate the differences
    diff = difflib.ndiff(s1_norm, s2_norm)

    return ''.join(diff)


# main
if __name__ == "__main__":
    # use argparse to get two filenames
    parser = argparse.ArgumentParser(description='Diff two files')
    parser.add_argument('file1', help='First file')
    parser.add_argument('file2', help='Second file')
    args = parser.parse_args()

    # Read the files
    with open(args.file1, 'r') as f:
        s1 = f.read()

    with open(args.file2, 'r') as f:
        s2 = f.read()

    # Print the differences
    print(diff_strings(s1, s2))
