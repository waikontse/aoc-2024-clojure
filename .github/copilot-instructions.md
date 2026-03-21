# Copilot instructions for `advent-of-code-2024`

## Build, test, and lint commands

- This is a Leiningen project. Use `lein test` for the full test suite.
- Run a single test namespace with `lein test advent-of-code-2024.week1.day1-test`.
- Run a single test var with `lein test :only advent-of-code-2024.week1.day3-test/solve-part-1-example-test`.
- Lint with `clj-kondo --lint src test`. Repository config lives in `.clj-kondo/config.edn`.
- The current baseline is not clean:
  - `lein test` currently stops on `test/advent_of_code_2024/utils/board_test.clj` because it calls unresolved symbol `fail`, and `test/advent_of_code_2024/week2/day13_test.clj` expects a missing `solve-part-1`.
  - `clj-kondo --lint src test` currently reports those same test errors plus many warnings in exploratory namespaces.

## High-level architecture

- The codebase is organized primarily by puzzle year, then week, then day:
  - `src/advent_of_code_2024/...`
  - `src/advent_of_code_2025/...`
- Most day namespaces are self-contained puzzle solvers that expose `solve-part-1` and `solve-part-2`, with tests under mirrored namespaces in `test/...`.
- Shared reusable logic lives in `src/advent_of_code_2024/utils/`:
  - `utils.io` handles resource-relative file reads and small data helpers.
  - `utils.board` defines the grid/board abstraction used by multiple 2024 puzzles.
  - `utils.algorithms` builds on `utils.board` for flood fill, geometry, and matrix helpers.
- Resource files are split by year:
  - 2024 inputs live under `resources/dayN/...`
  - 2025 inputs live under `resources/y2025/dayN/...`
- `src/advent_of_code_2024/snippets/` and some later namespaces such as `week3/day15` and `advent_of_code_2025/week2/ojalgo.clj` are exploratory/scratch areas rather than stable, well-tested puzzle solutions.

## Key conventions

- Namespace naming follows normal Clojure path mapping: hyphenated namespaces such as `advent-of-code-2024.week1.day1` live in underscored paths like `src/advent_of_code_2024/week1/day1.clj`.
- Input handling is year-specific and not fully uniform:
  - 2024 solution files usually call `advent-of-code-2024.utils.io/read-input` and often accept a resource-relative filename like `"day1/example.txt"`.
  - 2025 solution files often `slurp` `example` and `input` into top-level vars and pass raw string contents into `solve-part-1` / `solve-part-2`.
  - Preserve the existing style of the namespace you are editing instead of mixing both approaches in one file.
- The shared board representation is a map with `:width`, `:height`, and a flat `:board` vector. Position maps consistently use `{:x-pos ... :y-pos ...}` across `utils.board`, `utils.algorithms`, and board-based puzzle solvers.
- Tests typically mirror the solver namespace, use `clojure.test`, and assert exact answers for both example data and real puzzle input. They commonly import solver functions with `:refer :all`.
- Some older 2024 namespaces accept a `filename` argument but still hardcode the actual resource they read internally. Check the target file before assuming the function parameter is authoritative.
- Several puzzle files contain top-level debug expressions or commented-out `println` calls. Be careful not to add new top-level evaluation with side effects in namespaces that are loaded by tests.
