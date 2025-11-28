import argparse
from static_analysis import collect_files, zig_scanner


def build_suffix_array(arr):
    """Build suffix array for a list of any comparable elements."""
    n = len(arr)
    # Initial ordering based on first element
    suffixes = list(range(n))
    # Use elements directly instead of ord()
    rank = list(arr)
    tmp = [0] * n
    k = 1

    # Create a sentinel value that's "smaller" than any real element
    # We'll use a custom class for this purpose
    class Sentinel:
        def __lt__(self, other):
            return True

        def __gt__(self, other):
            return False

        def __eq__(self, other):
            return isinstance(other, Sentinel)

        def __le__(self, other):
            return True

        def __ge__(self, other):
            return isinstance(other, Sentinel)

    SENTINEL = Sentinel()

    while k < n:
        # Use tuple comparison for stable sorting
        def suffix_key(x):
            # Handle boundary case for second element
            second = rank[x + k] if x + k < n else SENTINEL
            return (rank[x], second)

        # Sort based on current and next k elements
        suffixes.sort(key=suffix_key)

        # Update ranks
        tmp[suffixes[0]] = 0
        for i in range(1, n):
            prev = suffixes[i - 1]
            curr = suffixes[i]
            # Compare tuples for ranking
            if suffix_key(prev) == suffix_key(curr):
                tmp[curr] = tmp[prev]
            else:
                tmp[curr] = tmp[prev] + 1

        rank = tmp[:]
        k *= 2

    return suffixes


def build_lcp_array(arr, suffix_array):
    """Build LCP array for a list of any comparable elements."""
    n = len(arr)
    lcp = [0] * (n - 1)
    rank = [0] * n

    for i in range(n):
        rank[suffix_array[i]] = i

    k = 0
    for i in range(n):
        if rank[i] == n - 1:
            k = 0
            continue

        j = suffix_array[rank[i] + 1]
        # Compare elements directly
        while i + k < n and j + k < n and arr[i + k] == arr[j + k]:
            k += 1

        lcp[rank[i]] = k
        k = max(0, k - 1)

    return lcp


def find_longest_repeated_subsequence(arr, min_occurrences=3):
    """Find the longest subsequence that appears at least min_occurrences times."""
    if not arr:
        return []

    n = len(arr)
    suffix_array = build_suffix_array(arr)
    lcp_array = build_lcp_array(arr, suffix_array)

    def check_length(length):
        count = 1
        max_count = 1
        start_pos = None

        for i in range(n - 1):
            if lcp_array[i] >= length:
                count += 1
                if count > max_count:
                    max_count = count
                    start_pos = suffix_array[i]
            else:
                count = 1

        return max_count >= min_occurrences, start_pos

    # Binary search on the length of the subsequence
    left, right = 0, min(max(lcp_array) if lcp_array else 0, n)
    result = []
    result_start = None

    while left <= right:
        mid = (left + right) // 2
        is_valid, start_pos = check_length(mid)

        if is_valid:
            result = arr[start_pos : start_pos + mid]
            result_start = start_pos
            left = mid + 1
        else:
            right = mid - 1

    return result


def main():
    args = parse_args()

    if args.directory:
        # Copy all files from directory to file name
        files = collect_files.collect_files(".", ext=".zig")

        def filter_filename(x: str) -> bool:
            return "zig-cache" not in x and "zig-string" not in x

        files = list(filter(filter_filename, files))
        with open(args.file, "w") as dst:
            for file in files:
                with open(file) as src:
                    dst.write(src.read())
                    dst.write("\n\n\n")

    with open(args.file) as f:
        text = f.read()
        scanner = zig_scanner.Scanner(text)
        tokens = scanner.tokenize()
        with open("tokens.txt", "w") as token_f:
            token_f.write(str(tokens))
        result = find_longest_repeated_subsequence(tokens)
        print(result)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--file", help="file to DRY out")
    parser.add_argument(
        "--directory",
        action="store_true",
        help="directory to collect zig files recursively from",
        required=False,
    )

    return parser.parse_args()


if __name__ == "__main__":
    main()
