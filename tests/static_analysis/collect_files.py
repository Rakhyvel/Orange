"""
Util file for recursively collecitng files from a directory
"""

import os
import subprocess


def collect_files_args(args, base, ext=".orng"):
    # Extract paths in the args.test list, if there are any
    # Prepend with base
    if (
        args.test and len(args.test) > 0 and args.test[0] != "."
    ):  # If the user enters `-t .`, treat that as a test of the whole directory
        full_path = [os.path.join(base, path) for path in args.test]
    elif args.count:
        full_path = [os.path.join(base, path) for path in args.count]
    else:
        full_path = [base]
    return collect_files(full_path, ext)


def collect_files(full_path, ext=".orng"):
    # Collect all .orng files recursively in any directories
    files = []
    for path in full_path:
        files += collect_files_recursive(path, ext)
    return list(dict.fromkeys(files))


def collect_modified_files(args, base):
    res = (
        subprocess.run(
            ["git", "diff", "--name-only", "--diff-filter=AM", "origin/main...HEAD"],
            capture_output=True,
        )
        .stdout.decode("utf-8")
        .split("\n")
    )
    res = list(filter(lambda x: x.startswith(base) and x.endswith(".orng"), res))
    return res


def collect_files_recursive(root_filename, ext):
    files = []
    if os.path.isdir(root_filename):
        dir_list = os.listdir(root_filename)
    elif root_filename.endswith(ext):
        return [root_filename]
    else:
        return []

    for file_name in dir_list:
        root_and_filename = os.path.join(root_filename, file_name)
        if os.path.isdir(root_and_filename):
            # Directory
            files += collect_files_recursive(root_and_filename, ext)
        elif root_and_filename.endswith(ext):
            # File
            files.append(root_and_filename)
    files.sort()
    return files
