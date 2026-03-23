# AGENTS.md

## Scope

- Applies to the whole repository.
- Covers: **code edits, tests, and documentation**.
- Default mode is **minimal, surgical modification** unless explicitly
  asked otherwise.

## Language and dependencies

- Use **base R (≥ 4.1)**.
- Native pipe `|>` is allowed and preferred.
- **Do not introduce new dependencies**.
- Existing dependencies (e.g. `cli`, `glue`) may be used.
- Use [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  for string interpolation.
- Avoid [`paste()`](https://rdrr.io/r/base/paste.html) unless strictly
  necessary for performance-critical code.

## General principles

- Assume inputs are **valid and well-formed**.
- Do not add defensive programming or guards unless explicitly
  requested.
- Prefer the **shortest correct implementation**.
- Avoid unnecessary line breaks and verbosity.
- Avoid introducing new variables unless they:
  - prevent recomputation, or
  - are required for correctness or performance.

## Performance

- **Performance is a priority**.
- Always prefer:
  - vectorized operations
  - matrix/array operations
  - `rowSums`, `colSums`, `sweep`, `%*%`, `outer`
- Prefer faster primitives over
  [`apply()`](https://rdrr.io/r/base/apply.html) when possible.
- [`apply()`](https://rdrr.io/r/base/apply.html) and
  [`Map()`](https://rdrr.io/r/base/funprog.html) are allowed when they
  are efficient and appropriate.
- Avoid:
  - unnecessary copies
  - repeated computation
  - hidden coercions
- Do not optimize memory vs speed unless explicitly requested.

## Code style and structure

### Pipes

- Prefer compact pipe chains:

``` r
x |> f() |> g() |> h()
```

- Do not introduce intermediate variables unless justified.

### Comments

- Add minimal comments per logical section.
- Comments describe **intent**, not obvious behavior.
- Use short section headers when needed:

``` r
# Compute pressure mismatch
# Normalize likelihood surface
```

### Function edits

- Make the **smallest possible patch**.
- Do not refactor unrelated code.
- Do not reorder code unless required.

## Validation and checks

- Input validation belongs **only at public function entry points**.
- Internal/helper functions should have **no validation**.
- Do not add checks unless there is a **real risk of silent failure or
  corrupted results**.

## Numerical stability

- Preserve existing numerical behavior by default.
- You may introduce safeguards **only when there is clear instability
  risk**, such as:
  - `log(0)`
  - division by zero
  - unstable normalization
- When adding such safeguards:
  - keep them minimal
  - **explicitly explain why they are needed**

## Output invariants (strict)

Edits must preserve: - class - dimensions - ordering - column names -
attributes

If a change would improve performance or correctness but break these: -
**do not apply it silently** - explain the tradeoff and ask or clearly
flag it

## Error handling

- Do not introduce
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) or defensive
  error handling.
- Errors should occur naturally unless explicitly handled at entry
  points.

## Matrix and vector behavior

- Prefer matrix/array operations for performance.
- Avoid unnecessary conversion between matrix and data.frame.
- Preserve original data structures.

## Silent failure risks

Only intervene if there is clear risk of: - unintended recycling -
dimension dropping - implicit coercion - invalid numerical operations

Otherwise, assume correct usage.

## Rewriting vs patching

- Default: **minimal patch**
- If code is clearly inconsistent, inefficient, or unclear:
  - rewriting the function is allowed
  - **must explicitly warn the user**

## Tests and documentation

- Do not add or modify tests unless explicitly requested.
- Do not add documentation (roxygen, README, vignettes) unless
  requested.

## Hard constraints (never do)

- Do not introduce new dependencies
- Do not add unnecessary validation
- Do not refactor unrelated code
- Do not change output structure silently
- Do not convert data structures unnecessarily
- Do not add helper functions used only once

## Uncertainty

- If requirements are unclear or incomplete, **ask for clarification**.
- Do not guess when behavior may change.
