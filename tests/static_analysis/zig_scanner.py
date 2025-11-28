class Token:
    def __init__(self, lines: list[str], value: str, col: int, line: int):
        self.value = value
        self.col = col
        self.line = line
        self.line_text = lines[line - 1]

    def __repr__(self) -> str:
        return self.value

    def __lt__(self, other):
        return self.value < other.value

    def __gt__(self, other):
        return self.value > other.value

    def __eq__(self, other):
        return self.value == other.value

    def __le__(self, other):
        return self.value <= other.value

    def __ge__(self, other):
        return self.value >= other.value


def remove_comments(text: str) -> str:
    ret = ""
    in_comment = False
    for i in range(0, len(text) - 2):
        if in_comment:
            if text[i] == "\n":
                in_comment = False
        elif text[i] == "/" and text[i + 1] == "/":
            in_comment = True
        else:
            ret += text[i]
    ret += text[len(text) - 2 :]
    return ret


class Scanner:
    def __init__(self, text: str):
        self.cursor = 0
        self.text = text
        self.line = 1
        self.col = 1
        self.lines = self.text.split("\n")

    def curr(self) -> str:
        return self.text[self.cursor]

    def peek(self, offset: int = 1) -> str:
        pos = self.cursor + offset
        return self.text[pos] if pos < len(self.text) else ""

    def eof(self) -> bool:
        return self.cursor >= len(self.text)

    def advance(self) -> str:
        if self.eof():
            return ""
        char = self.curr()
        self.cursor += 1
        if char == "\n":
            self.line += 1
            self.col = 0
        else:
            self.col += 1
        return char

    def skip_whitespace(self):
        while not self.eof() and self.curr().isspace():
            self.advance()

    def make_token(self, value: str) -> Token:
        return Token(self.lines, value, self.col, self.line)

    def scan_string(self, quote: str) -> Token:
        start_col = self.col
        start_line = self.line
        value = quote
        self.advance()

        while not self.eof():
            char = self.curr()
            if char == "\\":
                # escape
                value += char
                self.advance()
                if not self.eof():
                    value += self.curr()
                    self.advance()
            elif char == quote:
                # closing quote
                value += char
                self.advance()
                break
            else:
                value += char
                self.advance()

        return Token(self.lines, value, start_col, start_line)

    def scan_comment(self):
        if self.curr() == "/" and self.peek() == "/":
            while not self.eof() and self.curr() != "\n":
                self.advance()

    def scan_ident_or_keyword(self) -> Token:
        start_col = self.col
        start_line = self.line
        value = ""
        while not self.eof() and (self.curr().isalnum() or self.curr() == "_"):
            value += self.curr()
            self.advance()

        return Token(self.lines, value, start_col, start_line)

    def scan_number(self) -> Token:
        start_col = self.col
        start_line = self.line
        value = ""
        if self.curr() == "0" and self.peek() in "xXoObB":
            value += self.curr()
            self.advance()
            value += self.curr()
            self.advance()

        while not self.eof() and (self.curr().isalnum() or self.curr() in "._"):
            value += self.curr()
            self.advance()

        return Token(self.lines, value, start_col, start_line)

    def scan_operator(self) -> Token:
        start_col = self.col
        start_line = self.line

        operators = [
            # 4-character operators
            "<<|=",
            # 3-character operators
            "+%=",
            "+|=",
            "-%=",
            "-|=",
            "*%=",
            "*|=",
            "<<=",
            ">>=",
            "<<|",
            # 2-character operators
            "+=",
            "+%",
            "+|",
            "-=",
            "-%",
            "-|",
            "*=",
            "*%",
            "*|",
            "/=",
            "%=",
            "<<",
            ">>",
            "&=",
            "|=",
            "^=",
            ".?",
            "==",
            "!=",
            ">=",
            "<=",
            "++",
            "**",
            ".*",
            "||",
            "=>",
            "..",
            "->",
            "::",
            ".>",
            # 1-character operators
            "+",
            "-",
            "*",
            "/",
            "%",
            "&",
            "|",
            "^",
            "~",
            "!",
            ">",
            "<",
        ]

        longest_match = ""
        for op in operators:
            can_match = True
            for i, ch in enumerate(op):
                peek_pos = self.cursor + i
                if peek_pos >= len(self.text) or self.text[peek_pos] != ch:
                    can_match = False
                    break
            if can_match and len(op) > len(longest_match):
                longest_match = op

        if longest_match:
            for _ in longest_match:
                self.advance()
            return Token(self.lines, longest_match, start_col, start_line)

        value = self.curr()
        self.advance()
        return Token(self.lines, value, start_col, start_line)

    def tokenize(self) -> list[Token]:
        tokens: list[Token] = []

        while not self.eof():
            self.skip_whitespace()
            if self.eof():
                break

            char = self.curr()

            # string literals
            if char in ('"', "'"):
                tokens.append(self.scan_string(char))

            # comments
            elif char == "/" and self.peek() == "/":
                comment = self.scan_comment()
                if comment:
                    tokens.append(comment)

            # idents and keywords
            elif char.isalpha() or char == "_":
                tokens.append(self.scan_ident_or_keyword())

            # numbers
            elif char.isdigit():
                tokens.append(self.scan_number())

            # ops and punctuation
            else:
                tokens.append(self.scan_operator())

        return tokens


def alnumus(c):
    return c == "_" or c.isalnum() or c == "@"


keywords = [
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
