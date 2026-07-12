## Submission

This is a patch release (1.0.0 -> 1.0.1) that fixes a data-handling regression
introduced in the 1.0.0 raster-to-terra migration. `isotopeAssignmentModel()`,
`schoenersD()`, and `surfaceSimilarityMatrix()` did not pass `na.rm = TRUE` in
their internal normalizing sums, so an isoscape containing masked (NA) cells
returns an all-NA surface (or, in `schoenersD()`, an error). `schoenersD()` also
now returns a length-1 numeric value rather than a 1x1 data.frame. User-facing
changes are listed in NEWS.md.

I am submitting this soon after the 1.0.0 release because the regression silently
returns incorrect probability-of-origin surfaces for the common case of a
spatially masked study area, which affects users' results. Sorry for the
quick resubmission!

## Test environments
* local macOS, R 4.4.3
* GitHub Actions (r-lib standard matrix):
  - macOS-latest (release)
  - windows-latest (release)
  - ubuntu-latest (devel, release, oldrel-1)
* win-builder (devel and release)
* R-hub v2 (GitHub Actions), R-devel: Linux, Windows, macOS, macOS (arm64),
  and macOS M1 with sanitizers (m1-san)

## R CMD check results
0 errors | 0 warnings | 1 note

The NOTE is the CRAN incoming feasibility note flagging the short interval since
the 1.0.0 release ("Days since last update"); the reason for the quick
resubmission is explained above.

On local checks the single note is instead the environmental "unable to verify
current time" (the check machine has no time-server access); machines with an
outdated HTML Tidy may additionally report HTML5 `<main>` validation notes that
R's own `Rd2HTML` generates. Neither appears on CRAN's infrastructure, and both
are unrelated to the package.

## Reverse dependencies

There are no reverse dependencies on CRAN.
