# R-universe readiness check

Date: 2026-04-16 (UTC)

## What was checked
- Package metadata files (`DESCRIPTION`, `NAMESPACE`, `README.md`, `man/ugPlot.Rd`).
- Runtime dependencies referenced by `library(...)` calls in `R/app.R`.
- Local ability to run `R CMD build`.

## Findings
1. **Environment limitation**: this container does not have R installed (`R: command not found`), so `R CMD build` / `R CMD check` could not be executed here.
2. **Dependency gap fixed**: package code calls `library(gam)` but `gam` was missing from `DESCRIPTION`/`NAMESPACE`.
3. **Metadata improvement**: Added `URL` and `BugReports` fields in `DESCRIPTION` to improve package metadata quality on r-universe.

## Next validation commands (to run in an R-enabled environment)
```bash
R CMD build .
R CMD check --as-cran ugplot_1.0.tar.gz
```

If both commands finish with no ERROR (and ideally no WARNING), the package is typically ready for r-universe publication.

## Common install blockers on Linux
- Some transitive dependencies (notably `curl` and `fs`) require system development headers.
- If installation fails with messages like `dependency 'curl' is not available` or `dependency 'fs' is not available`, install OS packages first, then retry `install.packages()`.
- Typical Debian/Ubuntu packages: `build-essential`, `libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`, `libuv1-dev`.

