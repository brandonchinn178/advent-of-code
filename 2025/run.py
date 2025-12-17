#!/usr/bin/env python3

import abc
import argparse
import contextlib
import dataclasses
import functools
import queue
import re
import signal
import sqlite3
import subprocess
import sys
import threading
import time
from pathlib import Path
from typing import Any

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
            src = HERE / f"{day}.{lang.suffix}"
            outdir = OUTDIR / lang.suffix / day
            outdir.mkdir(parents=True, exist_ok=True)
            try:
                lang.run(datafile=datafile, src=src, outdir=outdir)
            except Exception as e:
                print(f"!!! ERROR({type(e).__name__}): {e}", file=sys.stderr)
                success = False

    if not success:
        sys.exit(1)

class Runner(abc.ABC):
    name: str
    suffix: str

    @abc.abstractmethod
    def run(self, *, datafile: Path, src: Path, outdir: Path) -> None:
        pass

class MakeRunner(Runner):
    @abc.abstractmethod
    def get_build_cmd(self, *, src: Path, exe: Path) -> list[str]:
        pass

    @abc.abstractmethod
    def get_run_cmd(self, *, exe: Path) -> list[str]:
        pass

    def run(self, *, datafile: Path, src: Path, outdir: Path) -> None:
        exe = outdir / "main"

        if not exe.exists() or src.stat().st_mtime > exe.stat().st_mtime:
            build_cmd = self.get_build_cmd(src=src, exe=exe)
            subprocess.run(build_cmd, check=True)

        with datafile.open() as f:
            run_cmd = self.get_run_cmd(exe=exe)
            with timer():
                subprocess.run(run_cmd, check=True, stdin=f)

class RunC(MakeRunner):
    name = "C"
    suffix = "c"

    def get_build_cmd(self, *, src: Path, exe: Path) -> list[str]:
        return [
            "gcc",
            "-Wall",
            "-O3",
            *("-Dmain=__real_main", "utils/add_c_timer.c"),
            *("-o", exe.as_posix()),
            src.as_posix(),
        ]

    def get_run_cmd(self, *, exe: Path) -> list[str]:
        return [exe.as_posix()]

class RunHaskell(MakeRunner):
    name = "Haskell"
    suffix = "hs"

    def get_build_cmd(self, *, src: Path, exe: Path) -> list[str | Path]:
        return [
            "ghc-9.12",
            *("-Wall", "-O2"),
            *("-odir", exe.parent),
            *("-hidir", exe.parent),
            *("-o", exe),
            *("-F", "-pgmF=utils/add_hs_timer.py"),
            src,
        ]

    def get_run_cmd(self, *, exe: Path) -> list[str]:
        return [exe.as_posix(), "+RTS", "-t", "-RTS"]

class RunSqlite(Runner):
    name = "Sqlite"
    suffix = "sql"

    def run(self, *, datafile: Path, src: Path, outdir: Path) -> None:
        # handle ctrl-c on long sqlite3 queries
        db_queue = queue.SimpleQueue()
        def interrupt(sig, frame):
            with contextlib.suppress(queue.Empty):
                db = db_queue.get(block=False)
                db.interrupt()
        signal.signal(signal.SIGINT, interrupt)

        script = self._replace_macros(src.read_text())
        with timer():
            debug, output = fork_and_await(
                functools.partial(
                    self._run,
                    datafile=datafile,
                    script=script,
                    db_queue=db_queue,
                )
            )
            for line in debug:
                print(f"[debug] {line}")
            if isinstance(output, Exception):
                raise output
            if result := output.get(1):
                print(f"Part 1: {result}")
            if result := output.get(2):
                print(f"Part 2: {result}")

    def _run(
        self,
        *,
        datafile: Path,
        script: str,
        db_queue: queue.SimpleQueue[sqlite3.Connection],
    ) -> tuple[list[str], dict[int, Any] | Exception]:
        with sqlite3.connect(":memory:") as db:
            db_queue.put(db)

            lines = [(line,) for line in datafile.read_text().splitlines()]

            c = db.cursor()
            c.execute("create table input (line TEXT)")
            c.executemany("insert into input (line) values (?)", lines)
            c.execute("create table debug (line TEXT)")
            c.execute("create table output (part INT, result TEXT)")

            try:
                c.executescript(script)
            except sqlite3.OperationalError as e:
                output = e
            else:
                output = None

            c.execute("select line from debug order by _rowid_")
            debug = [line for (line,) in c.fetchall()]

            if output is None:
                c.execute("select * from output order by part")
                output = dict(c.fetchall())

            return debug, output

    def _replace_macros(self, s: str) -> str:
        MACROS = {
            "DEBUG": r"insert into debug (line) \1",
            "PART1": r"insert into output (part, result) select 1, (\1)",
            "PART2": r"insert into output (part, result) select 2, (\1)",
        }
        for macro, replace in MACROS.items():
            s = re.sub(
                r"^" + macro + r"\((.*?)^\)",
                replace,
                s,
                flags=re.MULTILINE | re.DOTALL,
            )
        return s

@contextlib.contextmanager
def timer():
    start = time.perf_counter_ns()
    try:
        yield
    finally:
        duration = ((time.perf_counter_ns() - start) // 1000) / 1000
        print(f"[duration] {duration} ms", file=sys.stderr)

def fork_and_await[T](func: Callable[[], T]) -> T:
    result_queue = queue.SimpleQueue()

    def run() -> None:
        try:
            result = func()
            result_queue.put(result, block=False)
        except Exception as e:
            result_queue.put(e, block=False)

    thread = threading.Thread(target=run)
    thread.start()
    thread.join()

    result = result_queue.get(block=False)
    if isinstance(result, Exception):
        raise result
    return result

if __name__ == "__main__":
    main()
