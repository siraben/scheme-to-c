#!/usr/bin/env python3
import os
import glob
import subprocess
import sys
import difflib

CC = os.environ.get("CC", "clang")
CFLAGS = ["-O2", "-Wall", "-Wshadow", "-Wextra"]
LIBS = ["-lgc"]

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
VM_OBJECT = os.path.join(REPO_ROOT, "vm.o")
VM_SOURCE = os.path.join(REPO_ROOT, "vm.c")


def build_vm_object():
    if not os.path.exists(VM_OBJECT) or os.path.getmtime(VM_SOURCE) > os.path.getmtime(VM_OBJECT):
        subprocess.check_call([CC, *CFLAGS, "-c", VM_SOURCE, "-o", VM_OBJECT])


def run_test(base_name):
    scm = os.path.join(REPO_ROOT, "tests", f"test_{base_name}.scm")
    expected_file = os.path.join(REPO_ROOT, "tests", f"test_{base_name}.expected")
    output_dir = os.path.join(REPO_ROOT, "tests", "output")
    os.makedirs(output_dir, exist_ok=True)
    c_file = os.path.join(output_dir, f"test_{base_name}.c")
    exe_file = os.path.join(output_dir, f"test_{base_name}_runner")

    subprocess.check_call(["python3", os.path.join(REPO_ROOT, "scheme_to_c.py"), scm, c_file])
    subprocess.check_call([CC, *CFLAGS, "-o", exe_file, c_file, VM_OBJECT, *LIBS])

    result = subprocess.run([exe_file], capture_output=True, text=True)
    # Match the old shell-based runner which stripped newlines from each line
    actual = "".join(line.rstrip("\r\n") for line in result.stdout.splitlines())
    with open(os.path.join(output_dir, f"test_{base_name}.actual"), "w") as f:
        f.write(actual)

    with open(expected_file) as f:
        expected = f.read().strip()

    if actual.split() == expected.split():
        print(f"PASS: {base_name}")
        return True
    else:
        print(f"FAIL: {base_name}")
        for line in difflib.unified_diff(
            expected.splitlines(),
            actual.splitlines(),
            fromfile=expected_file,
            tofile="actual",
            lineterm="",
        ):
            print(line)
        return False


def main():
    build_vm_object()
    test_files = sorted(glob.glob(os.path.join(REPO_ROOT, "tests", "test_*.scm")))
    bases = [os.path.basename(f)[5:-4] for f in test_files]
    print("Running tests...")
    success = True
    for base in bases:
        if not run_test(base):
            success = False
    if not success:
        sys.exit(1)


if __name__ == "__main__":
    main()
