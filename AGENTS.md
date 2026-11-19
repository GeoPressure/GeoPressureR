<!-- BEGIN GeoPressure shared standard. Do not edit here: change
https://github.com/GeoPressure/GeoPressureSkills/blob/main/plugins/geopressure-dev/skills/geopressure-dev/standards.md
and copy it into every repo's AGENTS.md at the same time. -->
# GeoPressure development standard

Shared rules for the GeoPressure R repositories. Apply each rule where the repo has what it names
(`DESCRIPTION`, roxygen, `NEWS.md`, releases). The repo's own rules follow this block and take
precedence where they differ.

## Scope
- Covers code edits, tests and documentation.
- Default to a minimal, surgical patch. Do not refactor, reorder or reformat unrelated code.
- If code is clearly inconsistent, inefficient or unclear, you may rewrite the function, but warn
  the user explicitly.
- If a requirement is unclear or could change behaviour, ask. Do not guess.
- Flag any change that breaks compatibility with the last release. Unreleased changes on `dev`
  need no backward compatibility.

## General principles
- Assume inputs are valid and well-formed.
- Prefer the shortest correct implementation. Avoid verbose patterns and unnecessary line breaks.
- Introduce a variable only to avoid recomputation, or when correctness or performance needs it.
- Do not add a helper used only once; inline it. Add a helper only to remove real duplication.
- Do not introduce hidden side effects (options, global state, working directory).

## R style
- Use R (>= 4.1) and the native pipe `|>`. Prefer compact chains: `x |> f() |> g() |> h()`.
- Use `glue::glue()` (or `glue::glue_collapse()`) for strings. Avoid `paste()` and `paste0()`
  unless performance needs them.
- Performance is a priority. Prefer vectorised, matrix and array operations (`rowSums`,
  `colSums`, `sweep`, `%*%`, `outer`) over loops and `apply()`. `apply()` and `Map()` are fine
  when efficient.
- Avoid unnecessary copies, repeated computation and hidden coercions. Do not trade memory for
  speed, or speed for memory, unless asked.
- Add a short comment per logical section (roughly every 1–10 lines) that states its intent, not
  obvious R behaviour (e.g. `# Normalize likelihood surface`). Explain deliberate, non-obvious
  design choices. No tutorial-style comments.

## Dependencies
- Prefer base R and the packages already imported (e.g. `cli`, `glue`).
- Do not add a dependency unless the maintainer approves it.
- Never use `:::`. Call the package's own internal functions directly, and exported functions of
  other packages with `pkg::` when a namespace qualifier is needed.

## Validation and errors
- Validate inputs only at public entry points and external I/O (files, network, user input).
  Internal helpers have no validation.
- Add a check only for a real risk of silent failure or corrupted results: unintended recycling,
  dimension dropping, implicit coercion or invalid numerical operations.
- Do not add `tryCatch()`, fallbacks or defensive guards unless asked. Mention a possible fallback
  instead of adding it. Let errors occur naturally.
- Preserve numerical behaviour. Add a safeguard only for a clear instability (`log(0)`, division
  by zero, unstable normalisation). Keep it minimal and explain why it is needed.

## Output invariants
- Preserve the class, dimensions, ordering, column names and attributes of outputs.
- Keep existing data structures. Avoid needless conversion between matrix and data frame.
- If a better change would break one of these, do not apply it silently: explain the trade-off
  and ask.

## Argument names and units
- No unit suffixes in argument names, public or internal. Document the unit in `@param` (e.g.
  `time_shift` in hours, `twl_time_tolerance` in seconds).
- Use one fixed unit, consistent with related arguments. Add a unit-selection argument only if
  callers need to choose.
- For a rate, document the numerator, the denominator and any fixed duration (e.g. `time_drift`
  in hours per year, a year being 365.25 days).
- When renaming an argument, keep names, call sites, stored parameter keys, tests and
  documentation aligned. Flag every change to a public argument or configuration key.

## Docs and tests
- Do not add or modify tests, roxygen docs, README or vignettes unless asked.
- Edit roxygen in `R/`, never `man/*.Rd` or `NAMESPACE`. When roxygen changes, run
  `devtools::document()` and commit the regenerated files with the change.
- Edit `README.Rmd`, never `README.md`. Rebuild with `devtools::build_readme()` and commit both.
- Do not run the full test suite while iterating; run focused tests on what changed. Run the
  suite before opening or updating a PR, before a release and when behaviour changes. Report
  what it said.

## Mandatory checks
- Before committing or pushing, run from the repo root: `air format . --check` and
  `jarl check .`.
- Both must pass with no warnings or errors introduced by your changes. If formatting fails, run
  `air format .`, review the changes and rerun both checks. Fix failures before committing; do
  not defer them to CI.

## Branches and pull requests
- R packages develop on `dev`: make focused commits, run the mandatory checks and push to
  `origin/dev`. Repos that are not packages work on `main`, as their own rules say.
- Open a PR from `dev` to `main`. Merge `main` into `dev` and resolve conflicts before the PR is
  merged.
- Merge PRs with a merge commit, never squash or rebase: release notes link to the original
  commits.
- Use a draft PR while release metadata or checks are incomplete. Mark it ready only when the
  final version, NEWS entry and required GitHub checks are complete.
- For a patch while `dev` holds unreleased minor changes: branch `release-X.Y.Z` from `main`,
  cherry-pick the patch commits, release, then merge `main` back into `dev`. Never give a branch
  the name of a tag.

## Versioning
- Choose `X.Y.Z` by what a user must do to take the release, not by the size of the diff.
- Patch (`Z`): fixes and internal changes only. The same call on the same input behaves as before.
- Minor (`Y`): new features, or any behaviour or result a user can notice. An exported function
  or argument that has been deprecated, with a warning, for at least one release may be removed
  in a minor release.
- Major (`X`): an exported function or argument is removed or changed without that deprecation
  period, or data that used to be readable no longer is.
- After a release, set `DESCRIPTION` on `dev` to `X.Y.Z.9000`.

## NEWS and the release block
- Add each user-facing change to `NEWS.md` under `# <Package> (development version)`, linked to
  its commit.
- For a release, write one Markdown release block first. It is the source of truth. Copy it
  verbatim to the new top section of `NEWS.md`, the PR body and the GitHub Release body for the
  tag. Do not shorten, paraphrase, reorder or add items in any copy.
- Use `vX.Y.Z` as the PR title and `# <Package> vX.Y.Z` as the NEWS heading.
- Link every bullet to the commit or commits that support it. Use several inline links when a
  bullet sums up several changes.
- Say in the block when behaviour a user relied on has changed.

```md
# <Package> vX.Y.Z

## Main

- [Describe the principal user-facing change](https://github.com/GeoPressure/<Package>/commit/<sha>).

## Minor

- [Describe a smaller change or fix](https://github.com/GeoPressure/<Package>/commit/<sha>).

**Full Changelog**: <https://github.com/GeoPressure/<Package>/compare/vX.Y.(Z-1)...vX.Y.Z>
```

## Release checklist
- Set `DESCRIPTION` to the final `X.Y.Z`. Never merge a release with `.9000`.
- Run `cffr::cff_write()` after finalising `DESCRIPTION`. Review and commit the changes to
  `CITATION.cff`.
- Keep title, version and DOI consistent across `DESCRIPTION`, `CITATION.cff`, `inst/CITATION`
  and `README.Rmd`. Do not add a `codemeta.json`.
- Add the release block to `NEWS.md` before opening the PR, then paste it into the PR body.
- Resolve all `R CMD check` warnings and release-relevant notes. The PR's GitHub Actions checks
  must be green.
- When the release needs a new version of another GeoPressure package, release that one first
  and raise the minimum version in `DESCRIPTION`.
- After merging to `main`, tag `vX.Y.Z` and paste the unchanged release block into the GitHub
  Release.
<!-- END GeoPressure shared standard -->

# GeoPressureR rules

No rules beyond the shared standard.
