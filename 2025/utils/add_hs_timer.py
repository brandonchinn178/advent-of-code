#!/usr/bin/env python3

import sys
from pathlib import Path


def main():
    # https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#options-affecting-a-haskell-pre-processor
    fp, input, output = sys.argv[1:]

    s = Path(input).read_text()
    s = s.replace("main = do", "main = withTimer $ do")
    s += TIMER_CODE
    s = add_import(fp, s, IMPORTS)
    Path(output).write_text(s)


def add_import(fp: str, s: str, imp: str) -> str:
    lines = s.splitlines()

    try:
        idx = next(i for i, line in enumerate(lines) if not line.startswith("{-#"))
    except StopIteration:
        idx = 0

    lines = lines[:idx] + [imp, f'{{-# LINE {idx + 1} "{fp}" #-}}'] + lines[idx:]
    return "\n".join(lines)


IMPORTS = ";".join(
    [
        "import Data.Time qualified as TIMER",
        "import Numeric qualified as NUMERIC",
    ]
)

TIMER_CODE = """
withTimer :: IO a -> IO a
withTimer m = do
  start <- TIMER.getCurrentTime
  a <- m
  end <- TIMER.getCurrentTime
  let duration = end `TIMER.diffUTCTime` start
      durationMillis = (fromInteger . round) (duration * 1000000) / 1000 :: Double
      durationRender = NUMERIC.showFFloat Nothing durationMillis ""
  putStrLn $ "[duration.hs] " <> durationRender <> " ms"
  pure a
"""

if __name__ == "__main__":
    main()
