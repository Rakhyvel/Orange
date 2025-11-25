import random
import subprocess
import shutil
import os
from lxml import etree
import tempfile
from concurrent.futures import ProcessPoolExecutor
from static_analysis import ngram, collect_files, zig_scanner
from multiprocessing import Manager
import time
import cProfile

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SRC_DIR = os.path.normpath(os.path.join(TEST_DIR, "..", "src"))

manager = Manager()
lines_hit = manager.dict()
lines_hit_lock = manager.Lock()
errors = manager.dict()


def add_error_program(error: str):
    errors[error] = True


def convert_to_string(genes: list[str]):
    TOKEN_CONVERSION = {" ": "", "NEWLINE": "\n"}
    return " ".join(
        TOKEN_CONVERSION.get(token, token) for token in genes if token != " "
    )


def run_program(input_path: str, kcov_out: str, program_tokens: list[str]):
    program_display = " ".join(program_tokens)
    try:
        result = subprocess.run(
            [
                "kcov",
                "--include-path",
                SRC_DIR,
                kcov_out,
                "./zig-out/bin/orng-test",
                "coverage",
                input_path,
            ],
            timeout=15,
            # capture_output=True,
            stderr=subprocess.DEVNULL,
            cwd=None,
            preexec_fn=os.setsid if os.name != "nt" else None,
        )
        res = result.returncode
    except subprocess.TimeoutExpired:
        try:
            os.killpg(os.getpgid(result.pid), 9)  # no zombies
        except:
            pass
        print("timeout")
        res = 0
        program_display += "// timeout"

    if res != 0:
        # print(result.stdout)
        add_error_program(program_display)


COVERAGE_XPATH = etree.XPath(
    "//class[@filename]/lines/line[@hits and @number and number(@hits) > 0]"
)


def find_new_lines(xml_file):
    try:
        tree = etree.parse(xml_file)
        root = tree.getroot()
    except (etree.XMLSyntaxError, FileNotFoundError):
        return 0

    new_lines = 0

    line_elements = COVERAGE_XPATH(root)

    new_lines_set = set()
    for line_elem in line_elements:
        class_elem = line_elem.getparent().getparent()
        filename = class_elem.get("filename")

        if filename:
            line_number = line_elem.get("number")
            new_lines_set.add((filename, line_number))

    with lines_hit_lock:
        for key in new_lines_set:
            if key not in lines_hit:
                lines_hit[key] = True
                new_lines += 1

    return new_lines


def worker(thread_id: int, models: dict[int, ngram.NGram]):
    model = models[3 + thread_id % 5]

    with tempfile.TemporaryDirectory() as tmpdir:
        # construct files needed
        kcov_out = os.path.join(tmpdir, "kcov-out")
        cov_xml_path = os.path.join(kcov_out, "orng-test.164bd816b5697cd7", "cov.xml")
        input_path = os.path.join(tmpdir, "out.orng")

        iterations = 0

        while True:
            start_time = time.perf_counter()

            # generate a random program and write it to the input file
            program_tokens = random_program(model)
            program_text = convert_to_string(program_tokens)
            with open(input_path, "w") as f:
                f.write(program_text)

            # run orng integration test on it with kcov
            run_program(input_path, kcov_out, program_tokens)

            # figure out if there are any new lines covered
            new_lines = find_new_lines(cov_xml_path)
            end_time = time.perf_counter()

            if new_lines > 0:
                total = len(lines_hit)
                errs_total = len(errors)
                total_time = end_time - start_time
                print(
                    f"[{thread_id}:{iterations}] lines: {total} (+{new_lines}) errors: {errs_total} time: {total_time:.1f}s"
                )

            iterations += 1


def main(num_threads: int = os.cpu_count() or 8):
    shutil.rmtree("zig-cache", ignore_errors=True)
    shutil.rmtree("zig-out", ignore_errors=True)

    res = subprocess.run(
        ["zig", "build", "orng-test", "--release=safe", "-Doptimize=Debug"]
    ).returncode
    if res != 0:
        exit(1)

    models = build_ngram_model()
    start_time = time.perf_counter()
    try:
        with ProcessPoolExecutor(max_workers=num_threads) as executor:
            for i in range(0, num_threads):
                executor.submit(worker, i, models)
    except KeyboardInterrupt:
        for error in errors.keys():
            print("ERROR:", error)
        raise
    end_time = time.perf_counter()
    print(f"{(end_time - start_time)/num_threads:.1f}s")


def build_ngram_model():
    files = collect_files.collect_files(
        [
            "tests/integration",
            # "tests/negative",
            "examples",
            "std",
        ]
    )
    models = {n: ngram.NGram(n) for n in range(3, 9)}
    for file in files:
        with open(file, "r") as f:
            text = f.read() + "\n"
        text = zig_scanner.remove_comments(text)
        if len(text) == 0:
            continue
        scanner = zig_scanner.Scanner(text.replace("\n", " NEWLINE "))
        tokens = list(map(lambda token: token.value, scanner.tokenize()))
        for n in models.keys():
            models[n].train(tokens)
    return models


STARTING_TOKENS = [
    "fn",
    "const",
    "import",
    "extern",
    "trait",
    "impl",
]


def random_program(model: ngram.NGram):
    first_token = random.choice(STARTING_TOKENS)
    context = (None,) * (model.n - 2) + (first_token,)
    gen = model.generate(context)
    return [token for token in gen if token is not None]


if __name__ == "__main__":
    num_threads = 16
    main(num_threads)
