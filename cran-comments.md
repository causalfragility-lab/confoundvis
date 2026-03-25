## Test environments
* local Windows 11, R 4.5.1
* win-builder R-devel (2026-03-22)
* win-builder R-release (R 4.5.3)
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macos-latest, windows-latest — all passing
* R-hub: linux (R-devel), macos (R-devel), windows (R-devel)

## R CMD check results
0 errors | 0 warnings | 0 note

## Notes on flagged items
* "checking for future file timestamps: unable to verify current time":
  this is a transient network/firewall issue on the local Windows 11
  machine that prevents the time-check from reaching an external server.
  It is not reproducible on win-builder, R-hub, or GitHub Actions, and
  is unrelated to package code.
* "Possibly misspelled words": Cinelli, Hazlett, VanderWeele are author
  surnames in cited references, not misspellings.
* "Non-standard file cran-comments.md": added to .Rbuildignore.

## Resubmission
This is a resubmission following a second review by Uwe Ligges.

Changes from first resubmission:
* Replaced invalid local URI `[LICENSE](LICENSE)` in README.md with a
  shield.io badge linking to the full GPL-3 URL
  (https://www.gnu.org/licenses/gpl-3.0), resolving the flagged
  "possibly invalid file URI" note from urlchecker.
* Rewrote README.md for consistency with package style conventions:
  added logo, Overview section, Quick Start workflow, Plot Methods
  section, Sensitivity Frameworks table, Author section, and blockquote
  citation block.

Changes from original submission (first resubmission, retained here for
reference):
* Changed License field from "GPL-3 + file LICENSE" to "GPL-3"
* Removed the LICENSE file as it contained no additional restrictions
  beyond the standard GPL-3 terms
