"""
Don't
Repeat
Yourself

This scrpt finds places where you copy and pasted code, and didn't go back to clean it up ;)

Usage:
  python dry.py --file <path to your file!>
"""

import argparse

# TODO:
# - [ ] Fix to only check if the substring is repeated 3 or more times!
# - [ ] Move all files to all.txt
#     * Remove imports with `const [a-z0-9_]+ = @import\("[a-z0-9_/-.]+(.zig)?"\).*`
#     * Recursively remove newlines with `\n *\n`
# - [ ] Output actual regex
#     * Replace ', ' with ''
#     * Strip off first and last ','
#     * Prepend regex characters with \
#     * Replace '' with (\n* *)
#     * Replace X with [a-z0-9_]+


def main():
    args = parse_args()

    with open(args.file) as f:
        text = f.read()
        scanner = Scanner(text)
        tokens = scanner.tokenize()
        longest_repeated_substring(tokens)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--file", help="file to DRY out")

    return parser.parse_args()


def alnumus(c):
    return c == "_" or c.isalnum()


keywords = [  # TODO: Add these to a text file, and read it in
    "fn",
    "if",
    "while",
    "for",
    "switch",
    "else",
    "break",
    "continue",
    "and",
    "or",
    "unreachable",
    "return",
    "pub",
    "const",
    "struct",
    "union",
    "true",
    "false",
    "try",
    "null",
    "catch",
    "orelse",
    "inline",
    "usingnamespace",
    "defer",
    "var",
    "comptime",
    "extern",
    "enum",
    "undefined",
]


class Scanner:
    def __init__(self, text):
        self.cursor = 0
        self.text = text

    def eof(self):
        return self.cursor >= len(self.text)

    def curr(self):
        return self.text[self.cursor]

    def curr_or_empty(self):
        return self.curr() if not self.eof() else ""

    def skip_while(self, filter):
        while not self.eof() and filter(self.curr()):
            self.cursor += 1

    def tokenize(self):
        open_close = "(){}[]"
        tokens = []
        prev_char = self.text[0]
        token = prev_char
        self.cursor += 1
        prev_token_was_dot = False
        while not self.eof():
            char = self.curr()

            if token[0] in {'"', "'"}:
                token += char
                if char == token[0]:
                    tokens.append(token)
                    self.skip_while(lambda x: x.isspace() or x == token[0])
                    token = self.curr_or_empty()
            elif (
                alnumus(char) != alnumus(prev_char)
                or char.isspace()
                or prev_char in open_close
                or char in open_close
                or char == '"'
            ):
                if token.startswith("//"):
                    self.skip_while(lambda x: self.curr() != "\n")
                elif (
                    all([alnumus(x) for x in token])
                    and token not in keywords
                    and not prev_token_was_dot
                ):
                    tokens.append("X")
                else:
                    prev_token_was_dot = "." in token or token == "fn"
                    tokens.append(token)
                self.skip_while(lambda x: x.isspace())
                token = self.curr_or_empty()
            else:
                token += char

            prev_char = self.curr_or_empty()
            self.cursor += 1
        print(tokens)
        return tokens


def longest_repeated_substring(tokens):
    """
    Finds and prints the longest repeated substring in a list of tokens by comparing each pair's longest common prefix.
    """
    lrs = []
    num_tokens = len(tokens)
    prev_progress = -1

    # For each token
    for i in range(0, num_tokens - 1):
        # For every other token afterward
        for j in range(i + 1, num_tokens):
            # Find the longest common prefix length of the two token sequences
            lcp_length = longest_common_prefix(tokens, i, j)
            # If the lcp length is greater than the previous lcp, replace it
            if lcp_length > len(lrs):
                lrs = tokens[i : i + lcp_length]
                print("New longest repeated substring:", lrs)

        # Print out progress each 1%
        progress = (100 * i) // num_tokens
        if progress > prev_progress:
            print(f"{progress}%")
            prev_progress = progress
    print("Longest repeated substring:", lrs)


def longest_common_prefix(tokens, i, j):
    """
    Returns the length of the longest common prefix of the tokens lists starting at indices i and j.
    """
    length = 0
    while (
        i + length < len(tokens)
        and j + length < len(tokens)
        and tokens[i + length] == tokens[j + length]
    ):
        length += 1
    return length


if __name__ == "__main__":
    main()
