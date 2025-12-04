#!/usr/bin/env python3

import abc
import argparse
import contextlib
import dataclasses
import functools
import queue
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
                subprocess.run(run_cmd, stdin=f)

class RunC(MakeRunner):
    name = "C"
    suffix = "c"

    def get_build_cmd(self, *, src: Path, exe: Path) -> list[str]:
        return ["gcc", "-Wall", "-O3", "-o", exe.as_posix(), src.as_posix()]

    def get_run_cmd(self, *, exe: Path) -> list[str]:
        return [exe.as_posix()]

class RunHaskell(MakeRunner):
    name = "Haskell"
    suffix = "hs"

    def get_build_cmd(self, *, src: Path, exe: Path) -> list[str]:
        packages = ["containers", "text"]
        return [
            "ghc-9.12",
            *("-Wall", "-O2"),
            *(
                flag
                for package in packages
                for flag in ("-package", package)
            ),
            *("-odir", exe.parent),
            *("-hidir", exe.parent),
            *("-o", exe),
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

        with timer():
            result = fork_and_await(
                functools.partial(
                    self._run,
                    datafile=datafile,
                    src=src,
                    db_queue=db_queue,
                )
            )
            print(f"Part 1: {result[1]}")
            print(f"Part 2: {result[2]}")

    def _run(
        self,
        *,
        datafile: Path,
        src: Path,
        db_queue: queue.SimpleQueue[sqlite3.Connection],
    ) -> dict[int, Any]:
        with sqlite3.connect(":memory:") as db:
            db_queue.put(db)

            lines = [(line,) for line in datafile.read_text().splitlines()]
            c = db.cursor()
            c.execute("create table input (line TEXT)")
            c.executemany("insert into input (line) values (?)", lines)
            c.execute("create table output (part INT, result TEXT)")
            c.executescript(src.read_text())
            c.execute("select * from output order by part")
            return dict(c.fetchall())

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
