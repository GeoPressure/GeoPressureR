Project-wide R coding rules for GeoPressureR:

Language and style
- Base R only.
- Do not introduce new dependencies unless explicitly approved.
- Prefer the shortest correct implementation.
- Favor compact code and avoid unnecessary line breaks.
- Avoid declaring new variables unless strictly required for correctness or performance.
- Always use cli() and glue() rather than paste and sprintf()

Validation and checks
- Input validation belongs at the initial public function entry point only.
- Do not revalidate inputs downstream.
- Internal/helper functions should have no or minimal validation.
- You may suggest adding a check ONLY if there is a real risk of silent failure or corrupted results.
- Do not add defensive checks by default.

Performance
- Always prioritize optimized, vectorized solutions.
- Avoid loops unless they are provably necessary.
- Avoid intermediate objects and repeated computations.
- Optimize for large time series and likelihood grids.

Comments
- Add minimal comments per logical code section (roughly every 1–10 lines).
- Comments should describe the purpose of the section, not obvious R behavior.
- Explicitly comment deliberate design choices when they are non-obvious and explain the reason.
- Do not add redundant or tutorial-style comments.

Structure
- Do not refactor function signatures unless explicitly requested.
- Preserve object classes and avoid unintended type coercion.
- Do not add documentation (roxygen or otherwise) unless explicitly requested.

General behavior
- Assume inputs are valid and well-formed.
- Do not add error handling, guards, or fallback logic unless asked or clearly justified.
- Output only the minimal correct R code and required comments.

Use the jarl rules: https://jarl.etiennebacher.com/rules
 - any(!x) should be !all(x)