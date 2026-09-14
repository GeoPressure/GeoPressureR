# AGENTS.md

## Scope
- Applies to the whole repository.
- Covers: **code edits, tests, and documentation**.
- Default mode is **minimal, surgical modification** unless explicitly asked otherwise.


## Language and dependencies
- Use **base R (≥ 4.1)**.
- Native pipe `|>` is allowed and preferred.
- **Do not introduce new dependencies**.
- Existing dependencies (e.g. `cli`, `glue`) may be used.
- Use `glue::glue()` for string interpolation.
- Avoid `paste()` unless strictly necessary for performance-critical code.


## General principles
- Assume inputs are **valid and well-formed**.
- Do not add defensive programming or guards unless explicitly requested.
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
- Prefer faster primitives over `apply()` when possible.
- `apply()` and `Map()` are allowed when they are efficient and appropriate.
- Avoid:
  - unnecessary copies
  - repeated computation
  - hidden coercions
- Do not optimize memory vs speed unless explicitly requested.


## Code style and structure

### Pipes
- Prefer compact pipe chains:
```r
x |> f() |> g() |> h()
```
- Do not introduce intermediate variables unless justified.

### Comments
- Add minimal comments per logical section.
- Comments describe **intent**, not obvious behavior.
- Use short section headers when needed:
```r
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
- Do not add checks unless there is a **real risk of silent failure or corrupted results**.


## Numerical stability
- Preserve existing numerical behavior by default.
- You may introduce safeguards **only when there is clear instability risk**, such as:
  - `log(0)`
  - division by zero
  - unstable normalization
- When adding such safeguards:
  - keep them minimal
  - **explicitly explain why they are needed**


## Output invariants (strict)
Edits must preserve:
- class
- dimensions
- ordering
- column names
- attributes

If a change would improve performance or correctness but break these:
- **do not apply it silently**
- explain the tradeoff and ask or clearly flag it


## Error handling
- Do not introduce `tryCatch()` or defensive error handling.
- Errors should occur naturally unless explicitly handled at entry points.


## Matrix and vector behavior
- Prefer matrix/array operations for performance.
- Avoid unnecessary conversion between matrix and data.frame.
- Preserve original data structures.


## Silent failure risks
Only intervene if there is clear risk of:
- unintended recycling
- dimension dropping
- implicit coercion
- invalid numerical operations

Otherwise, assume correct usage.


## Rewriting vs patching
- Default: **minimal patch**
- If code is clearly inconsistent, inefficient, or unclear:
  - rewriting the function is allowed
  - **must explicitly warn the user**


## Tests and documentation
- Do not add or modify tests unless explicitly requested.
- Do not add documentation (roxygen, README, vignettes) unless requested.


## Mandatory CI checks
- Before committing or pushing agent-made changes, run from the repository root:
  - `air format . --check`
  - `jarl check .`
- Both commands must pass with no warnings or errors introduced by the changes.
- Fix failures before committing or pushing; do not defer them to CI.
- Never use `:::` in package code. Call internal GeoPressureR functions directly and exported functions with `::` when a namespace qualifier is needed.


## Development, PR, and release workflow

### Branch flow
- Develop on `dev`: make focused commits, run the mandatory checks, and push to `origin/dev`.
- Open a pull request from `dev` to `main`. Merge `main` into `dev` and resolve conflicts before the PR is merged.
- Use a draft PR while release metadata or release checks are incomplete. Mark it ready only when the final package version, NEWS entry, and required GitHub checks are complete.

### One canonical release block
- For every release, write one Markdown release block first. It is the source of truth and must be copied **verbatim** to:
  1. the new top section of `NEWS.md`;
  2. the pull-request body; and
  3. the GitHub Release body created for the version tag.
- Do not shorten, paraphrase, reorder, or add items independently in any of those three places.
- Use the release version as the PR title (for example, `v3.6.1`) and as the top NEWS heading.
- Keep the heading, subsection headings, bullet text, Markdown links, and the full-changelog comparison link identical in all three copies.

```md
# GeoPressureR vX.Y.Z

## Main

- [Describe the principal user-facing change](https://github.com/GeoPressure/GeoPressureR/commit/<sha>).

## Minor

- [Describe a smaller change or fix](https://github.com/GeoPressure/GeoPressureR/commit/<sha>).

**Full Changelog**: <https://github.com/GeoPressure/GeoPressureR/compare/vX.Y.(Z-1)...vX.Y.Z>
```

### Release checklist
- Set `DESCRIPTION`, `CITATION.cff`, and `codemeta.json` to the final `X.Y.Z` version; do not merge a release with `.9000`.
- Run `cffr::cff_write()` after finalizing `DESCRIPTION`, then review and commit the generated `CITATION.cff` changes.
- Add the canonical release block to `NEWS.md` before opening the PR, then paste that exact block into the PR body.
- Resolve all `R CMD check` warnings and release-relevant notes, and confirm the PR's GitHub Actions matrix is green.
- After merging to `main`, create tag `vX.Y.Z` and paste the unchanged canonical release block into the GitHub Release description.


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
