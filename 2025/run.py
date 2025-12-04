#!/usr/bin/env python3

import argparse
import contextlib
import dataclasses
import signal
import sqlite3
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).absolute().parent
OUTDIR = HERE / "build"

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input", type=Path)
    args = parser.parse_args()

    datafile = args.input
    day = datafile.name.removesuffix(".txt").removesuffix("-sample")

    result_file = datafile.with_name(datafile.name + ".result")
    if result_file.exists():
        print("\n===== Expected results =====")
        print(result_file.read_text().strip())

    langs = [RunC(), RunHaskell(), RunSqlite()]
    success = True
    for lang in langs:
        if Path(f"{day}.{lang.suffix}").exists():
            print(f"\n===== {lang.name} =====")
            outdir = OUTDIR / lang.suffix / day
            outdir.mkdir(parents=True, exist_ok=True)
            try:
                lang.run(datafile=datafile, day=day, outdir=outdir)
            except Exception as e:
                print(f"!!! ERROR: {e}", file=sys.stderr)
                success = False

    if not success:
        sys.exit(1)

class RunC:
    name = "C"
    suffix = "c"

    def run(self, *, datafile: Path, day: str, outdir: Path) -> None:
        src = HERE / f"{day}.c"
        exe = outdir / "main"

        if not exe.exists() or src.stat().st_mtime > exe.stat().st_mtime:
            subprocess.run(["gcc", "-Wall", "-O3", "-o", exe, src], check=True)

        with datafile.open() as f:
            with timer():
                subprocess.run([exe], stdin=f)

class RunHaskell:
    name = "Haskell"
    suffix = "hs"

    def run(self, *, datafile: Path, day: str, outdir: Path) -> None:
        src = HERE / f"{day}.hs"
        exe = outdir / "main"

        subprocess.run([
            "ghc-9.12",
            "-Wall", "-O2",
            "-package", "text",
            "-odir", outdir,
            "-hidir", outdir,
            "-o", exe,
            src,
        ], check=True)

        with datafile.open() as f:
            with timer():
                subprocess.run([exe, "+RTS", "-t", "-RTS"], stdin=f)

class RunSqlite:
    name = "Sqlite"
    suffix = "sql"

    def run(self, *, datafile: Path, day: str, outdir: Path) -> None:
        src = HERE / f"{day}.sql"
        db_path = outdir / "run.db"

        lines = [(line,) for line in datafile.read_text().splitlines()]

        db_path.unlink(missing_ok=True)
        with sqlite3.connect(db_path) as db:
            c = db.cursor()
            with timer():
                c.execute("create table input (line TEXT)")
                c.executemany("insert into input (line) values (?)", lines)
                c.execute("create table output (part INT, result TEXT)")
                c.executescript(src.read_text())
                c.execute("select * from output order by part")
                for part, result in c.fetchall():
                    print(f"Part {part}: {result}")

@contextlib.contextmanager
def timer():
    start = time.perf_counter_ns()
    try:
        yield
    finally:
        duration = ((time.perf_counter_ns() - start) // 1000) / 1000
        print(f"[duration] {duration} ms", file=sys.stderr)

if __name__ == "__main__":
    main()
