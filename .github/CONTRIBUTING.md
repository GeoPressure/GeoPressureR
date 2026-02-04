# Contributing to GeoPressureR

This outlines how to propose a change to GeoPressureR.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file.
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file.
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed.
If you’ve found a bug, please file an issue that illustrates the bug with a minimal [reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

### Pull request process

1. Fork the package and clone it. If you haven't done this before, we recommend `usethis::create_from_github("Rafnuss/GeoPressureR", fork = TRUE)`.
2. Install development dependencies with `devtools::install_dev_deps()`.
3. Ensure the package passes checks with `devtools::check()`. If it doesn't pass cleanly, please ask for help before continuing.
4. Create a Git branch for your PR (e.g., `usethis::pr_init("brief-description-of-change")`).
5. Make your changes, commit, and create a PR (e.g., `usethis::pr_push()`).
6. The PR title should briefly describe the change and the PR body should contain `Fixes #issue-number`.
7. For user-facing changes, add a bullet at the top of `NEWS.md` (just below the first header). Follow the style described in https://style.tidyverse.org/news.html.

### Code style

New code should follow the project conventions:

- Base R only; do not introduce new dependencies without prior discussion.
- Prefer compact, vectorized code and avoid unnecessary intermediate objects.
- Use `cli` and `glue` for messages and string interpolation.
- Follow the jarl rules: https://jarl.etiennebacher.com/rules

We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.
We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. Contributions with test cases included are easier to accept.
