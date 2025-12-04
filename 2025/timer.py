#!/usr/bin/env python3

import contextlib
import time
import subprocess
import sys

@contextlib.contextmanager
def timer():
    start = time.perf_counter_ns()
    try:
        yield
    finally:
        duration = (time.perf_counter_ns() - start) // 1000000
        print(f"[duration] {duration} ms", file=sys.stderr)

def main():
    with timer():
        proc = subprocess.run(sys.argv[1:])
    if proc.returncode != 0:
        sys.exit(1)

if __name__ == "__main__":
    main()
