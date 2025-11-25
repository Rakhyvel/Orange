from collections import defaultdict, Counter
import random
from static_analysis import collect_files, zig_scanner


class NGram:
    def __init__(self, n):
        self.n = n
        self.ngrams = defaultdict(Counter)

    def train(self, tokens: list[str]):
        if len(tokens) < self.n:
            return

        padded_tokens = [None] * self.n + tokens + [None] * self.n

        for i in range(len(padded_tokens) - self.n + 1):
            context = tuple(padded_tokens[i : i + self.n - 1])
            next_token = padded_tokens[i + self.n - 1]
            self.ngrams[context][next_token] += 1

    def random_token(self):
        context = random.choice(list(self.ngrams.keys()))
        counter = self.ngrams[context]
        tokens, weights = zip(*counter.items())  # separate keys and their frequencies
        choice = random.choices(tokens, weights=weights, k=1)[0]
        return choice

    def predict(self, context):
        if len(context) != self.n - 1:
            raise ValueError(f"context length must be {self.n - 1} got {len(context)}")

        candidates = self.ngrams.get(tuple(context))
        if not candidates:
            return None  # unknown context

        return self._sample(candidates)

    def _sample(self, counter):
        inv = {tok: 1.0 / cnt for tok, cnt in counter.items()}

        total = sum(inv.values())
        rand = random.random() * total
        i = 0
        for token, count in inv.items():
            i += count
            if rand <= i:
                return token

    def generate(self, ctx: tuple[str]) -> list[str]:
        if len(ctx) != self.n - 1:
            raise ValueError(f"context length must be {self.n - 1}, got {len(ctx)}")

        retval = list(ctx)

        while ctx[self.n - 2]:
            next_token = self.predict(ctx)
            if next_token:
                retval.append(next_token)
            ctx = (*ctx[1:], next_token)

        return retval

    def generate_until(self, ctx: tuple[str], until) -> list[str]:
        if len(ctx) != self.n - 1:
            raise ValueError(f"context length must be {self.n - 1}, got {len(ctx)}")

        retval = list(ctx)

        while ctx[self.n - 2] and not until(ctx[self.n - 2]):
            next_token = self.predict(ctx)
            if next_token and not until(next_token):
                retval.append(next_token)
            ctx = (*ctx[1:], next_token)

        return retval


def main():
    files = collect_files.collect_files(
        [
            "integration/expressions",
        ]
    )
    n = 5
    model = NGram(n)
    for file in files:
        with open(file, "r") as f:
            text = f.read()
        scanner = zig_scanner.Scanner(text + "\n")
        tokens = list(map(lambda token: token.value, scanner.tokenize()))
        model.train(tokens)

    print(model.generate_until(("fn", "main", "(", ")"), lambda x: x == "0"))


if __name__ == "__main__":
    main()
