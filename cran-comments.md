## Test environments
* local OS X install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0
* win-builder (devel and release)

## R CMD check results

## Reverse dependencies

I have run R CMD check on the 23 downstream dependencies using the latest development build of DiagrammeR (via the `devtools::revdep_check()` function). Here are the dependant packages that contain any errors/warnings/notes:

Checked HydeNet            : 1 error  | 0 warnings | 0 notes
Checked lavaanPlot         : 0 errors | 0 warnings | 1 note 
Checked markovchain        : 1 error  | 0 warnings | 0 notes
Checked pMineR             : 0 errors | 1 warning  | 0 notes
Checked radiant.model      : 0 errors | 1 warning  | 0 notes
Checked sbfc               : 0 errors | 0 warnings | 1 note 
Checked sem                : 1 error  | 0 warnings | 0 notes
Checked simmer.plot        : 0 errors | 1 warning  | 0 notes
Checked SpaDES             : 0 errors | 0 warnings | 1 note 
Checked teachingApps       : 0 errors | 1 warning  | 0 notes
Checked umx                : 2 errors | 0 warnings | 0 notes
Checked Wmisc              : 1 error  | 1 warning  | 2 notes
Checked xgboost            : 1 error  | 0 warnings | 0 notes

Many of the errors are because I do not have the necessary **BioC** packages available on my development system. I have emailed all package maintainers.

All revdep maintainers were emailed about the release on August 20, 2017.
