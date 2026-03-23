 ## Test environments
* local Windows 11, R 4.5.1
* win-builder R-devel (2026-03-22)
* win-builder R-release (R 4.5.3)
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macos-latest, windows-latest — all passing
* R-hub: linux (R-devel), macos (R-devel), windows (R-devel)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Notes on flagged items
* "Possibly misspelled words": Cinelli, Hazlett, VanderWeele are author
  surnames in cited references, not misspellings.
* "Non-standard file cran-comments.md": added to .Rbuildignore.

## New submission
First submission to CRAN.
