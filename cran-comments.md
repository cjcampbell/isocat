## Submission

This is a significant update (0.3.0 -> 1.0.0). isocat's spatial backend has been
migrated from the maintenance-mode `raster`/`sp` packages to `terra` (`sp` is
removed; `raster` is now only a guarded, back-compatible `Suggests`). The release
also fixes several bugs surfaced during the migration. User-facing changes are
listed in NEWS.md.

## Test environments
* local macOS, R 4.4.3
* win-builder (devel and release)
* GitHub Actions / R-hub: Ubuntu, Windows, and macOS (R release and devel)

## R CMD check results
0 errors | 0 warnings | 2 notes

The one NOTE, "unable to verify current time", is environmental (the check
machine has no time-server access) and unrelated to the package.

On machines with an outdated HTML Tidy, R CMD check may additionally report HTML
validation NOTEs about the HTML5 `<main>` element that R's own `Rd2HTML`
generates; these do not appear with a current Tidy (e.g. on CRAN's
infrastructure) and require no changes to the package.

## Reverse dependencies

There are no reverse dependencies on CRAN.
