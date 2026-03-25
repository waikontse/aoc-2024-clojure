# AGENTS: How to work in this repository

Purpose: quick, actionable guidance so an automated coding agent can be immediately productive in this project.

- Quick start (commands)
  - Run full test suite: `lein test`
  - Run a single test namespace: `lein test advent-of-code-2024.week1.day1-test`
  - Run a single test var: `lein test :only advent-of-code-2024.week1.day3-test/solve-part-1-example-test`
  - Lint edited files: `clj-kondo --lint src test`
  - Project descriptor: `project.clj` (Clojure 1.12.3, deps include ojalgo)

- High-level architecture (the "why")
  - Year-based separation: code organized by year then week then day under `src/`.
    Example: `advent-of-code-2024.week1.day1` → `src/advent_of_code_2024/week1/day1.clj`.
  - Puzzle solvers are usually self-contained namespaces exposing `solve-part-1` and `solve-part-2`.
  - Shared utilities live under `src/advent_of_code_2024/utils/` (notably `io.clj`, `board.clj`, `algorithms.clj`). These provide common data helpers and a board abstraction reused by multiple puzzles.

- Important project-specific conventions and patterns
  - Namespace → file path mapping: hyphenated namespace names map to underscored directory names (see the `src/` layout above).
  - Board abstraction:
    - Board is a map with keys `:width`, `:height`, and a flat `:board` vector.
    - Positions are maps with `:x-pos` and `:y-pos`.
    - Example API (in `src/advent_of_code_2024/utils/board.clj`): `get-pos`, `set-pos`, `parse-to-board`, `get-data-*` helpers.
  - Input handling differs by year:
    - 2024-style: use `advent-of-code-2024.utils.io/read-input` with a resource-relative filename string (e.g. `"day1/example.txt"`). Example usage in `src/advent_of_code_2024/week1/day1.clj`.
    - 2025-style: many namespaces `slurp` example/input into top-level vars and pass raw strings into solver functions. Preserve the existing style of the namespace you edit.
  - Tests mirror solver namespaces and commonly `:refer :all` to import functions.

- Integration points & external dependencies
  - `project.clj` lists project deps (Clojure, math libs, ojalgo). Changes to deps require `lein`-based workflow.
  - Resources are under `resources/` and are year-split (2024 top-level day directories, 2025 under `resources/y2025/`). When adding/reading sample inputs, use the existing `utils.io/append-path` behavior which expects `./resources/<filename>`.

- Known baseline issues & guardrails (must-read before editing)
  - `test/advent_of_code_2024/utils/board_test.clj` contains `(fail "impelemtn me")` and will make the full test run fail. Path: `test/advent_of_code_2024/utils/board_test.clj`.
  - `test/advent_of_code_2024/week2/day13_test.clj` expects a missing `solve-part-1` — some tests are intentionally incomplete and will fail CI.
  - Many exploratory namespaces under `src/.../snippets/` and some `week3/` files are noisy and have clj-kondo warnings; avoid sweeping changes there.
  - DO NOT introduce top-level side effects (println, slurp at top-level) in namespaces loaded by tests — tests expect pure namespace loading.

- Safe-edit checklist for automated agents
  - Prefer small, local changes that preserve public API names (`solve-part-1`, `solve-part-2`).
  - Run the smallest relevant test(s) for your change (single namespace or var) before pushing.
  - Run `clj-kondo --lint` on only the files you changed.
  - Avoid editing exploratory `snippets/` or experimental 2025 files unless requested.
  - If adding resources, follow existing resource layout (e.g. `resources/dayN/...` or `resources/y2025/dayN/...`).

- Where to look for examples in the codebase (quick links)
  - Input helpers: `src/advent_of_code_2024/utils/io.clj`
  - Board abstraction: `src/advent_of_code_2024/utils/board.clj`
  - Representative solver: `src/advent_of_code_2024/week1/day1.clj`
  - Exploratory code (avoid unless necessary): `src/advent_of_code_2024/snippets/` and `src/advent_of_code_2024/week3/`
  - Tests: `test/` mirrors `src/` — run targeted tests here.

- Minimal PR checklist for maintainers/agents
  - Include targeted test command(s) in PR description.
  - Run `clj-kondo --lint` for changed files and paste results if non-trivial.
  - Comment when intentionally changing test scaffolding (some tests are placeholders).





