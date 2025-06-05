#!/usr/bin/env python3
import os
import subprocess
import time

CC = os.environ.get("CC", "clang")
CFLAGS = ["-O2", "-Wall", "-Wshadow", "-Wextra"]
LIBS = ["-lgc"]

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
VM_OBJECT = os.path.join(REPO_ROOT, "vm.o")
VM_SOURCE = os.path.join(REPO_ROOT, "vm.c")
NEW_COMPILER = os.path.join(REPO_ROOT, "scheme_to_c.py")
OUTPUT_DIR = os.path.join(REPO_ROOT, "benchmarks", "output")
OLD_COMPILER_PATH = os.path.join(OUTPUT_DIR, "scheme_to_c_prev.py")

BENCHMARKS = [
    "benchmark_tail_loop",
    "benchmark_cps_fact",
    "benchmark_cps_loop",
    "benchmark_closure_calls",
    "benchmark_map",
    "benchmark_reverse",
    "benchmark_fold",
    "benchmark_fib",
    "benchmark_let_star_chain",
    "benchmark_arith",
]


def build_vm_object():
    if not os.path.exists(VM_OBJECT):
        subprocess.check_call([CC, *CFLAGS, "-c", VM_SOURCE, "-o", VM_OBJECT])


def write_previous_compiler():
    """Write scheme_to_c.py from the previous commit to OLD_COMPILER_PATH."""
    prev_commit = subprocess.check_output([
        "git",
        "rev-parse",
        "HEAD^",
    ], cwd=REPO_ROOT, text=True).strip()
    compiler_content = subprocess.check_output([
        "git",
        "show",
        f"{prev_commit}:scheme_to_c.py",
    ], cwd=REPO_ROOT, text=True)
    with open(OLD_COMPILER_PATH, "w") as f:
        f.write(compiler_content)


def compile_and_run(compiler: str, bench_base: str, suffix: str) -> float:
    scm = os.path.join(REPO_ROOT, "benchmarks", f"{bench_base}.scm")
    c_file = os.path.join(OUTPUT_DIR, f"{bench_base}_{suffix}.c")
    exe_file = os.path.join(OUTPUT_DIR, f"{bench_base}_{suffix}_runner")

    try:
        subprocess.check_call(["python3", compiler, scm, c_file])
        subprocess.check_call([CC, *CFLAGS, "-o", exe_file, c_file, VM_OBJECT, *LIBS])
    except subprocess.CalledProcessError:
        return float('nan')

    start = time.perf_counter()
    subprocess.check_call([exe_file], stdout=subprocess.DEVNULL)
    end = time.perf_counter()
    return end - start


def main():
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    build_vm_object()

    results = []
    write_previous_compiler()

    for bench in BENCHMARKS:
        t_old = compile_and_run(OLD_COMPILER_PATH, bench, "old")
        t_new = compile_and_run(NEW_COMPILER, bench, "new")
        results.append((bench, t_old, t_new))

    print("Benchmark results (time in seconds):")
    print("{:<25} {:>10} {:>10}".format("Benchmark", "Old", "New"))
    for name, old, new in results:
        print("{:<25} {:>10.6f} {:>10.6f}".format(name, old, new))

    if os.path.exists(OLD_COMPILER_PATH):
        os.remove(OLD_COMPILER_PATH)


if __name__ == "__main__":
    main()
