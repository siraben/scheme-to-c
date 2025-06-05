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
    if not os.path.exists(VM_OBJECT):
        subprocess.check_call([CC, *CFLAGS, "-c", VM_SOURCE, "-o", VM_OBJECT])

def run_test(base_name):
    scm = os.path.join(REPO_ROOT, "tests", f"test_{base_name}.scm")
    output_dir = os.path.join(REPO_ROOT, "tests", "output")
    os.makedirs(output_dir, exist_ok=True)
    c_file = os.path.join(output_dir, f"test_{base_name}.c")
    exe_file = os.path.join(output_dir, f"test_{base_name}_runner")

    # compile Scheme to C and then to native code
    subprocess.check_call(["python3", os.path.join(REPO_ROOT, "scheme_to_c.py"), scm, c_file])
    subprocess.check_call([CC, *CFLAGS, "-o", exe_file, c_file, VM_OBJECT, *LIBS])

    result = subprocess.run([exe_file], capture_output=True, text=True)
    actual = "".join(line.rstrip("\r\n") for line in result.stdout.splitlines())

    prelude = os.path.join(REPO_ROOT, "tests", "guile_prelude.scm")
    guile_result = subprocess.run(["guile", "-l", prelude, scm], capture_output=True, text=True)
    expected = "".join(line.rstrip("\r\n") for line in guile_result.stdout.splitlines())

    with open(os.path.join(output_dir, f"test_{base_name}.actual"), "w") as f:
        f.write(actual)
    with open(os.path.join(output_dir, f"test_{base_name}.expected"), "w") as f:
        f.write(expected)

    if actual.split() == expected.split():
        print(f"PASS: {base_name}")
        return True
    else:
        print(f"FAIL: {base_name}")
        for line in difflib.unified_diff(
            expected.splitlines(),
            actual.splitlines(),
            fromfile="guile",
            tofile="actual",
            lineterm="",
        ):
            print(line)
        return False

def main():
    build_vm_object()
    test_files = sorted(glob.glob(os.path.join(REPO_ROOT, "tests", "test_*.scm")))
    bases = [os.path.basename(f)[5:-4] for f in test_files]
    print("Running tests against guile...")
    success = True
    for base in bases:
        if not run_test(base):
            success = False
    if not success:
        sys.exit(1)

if __name__ == "__main__":
    main()
