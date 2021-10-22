# bioCancer

<details>

* Version: 
* Source code: ???
* URL: https://github.com/rich-iannone/DiagrammeR
* BugReports: https://github.com/rich-iannone/DiagrammeR/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
         binary source needs_compilation
pkgbuild  1.0.7  1.0.8             FALSE
tidyr     1.0.2  1.0.3              TRUE

  Binaries will be installed


installing the source packages ‘DO.db’, ‘GO.db’, ‘org.Hs.eg.db’, ‘pkgbuild’, ‘reactome.db’



```
### CRAN

```

  There are binary versions available but the source versions are later:
         binary source needs_compilation
pkgbuild  1.0.7  1.0.8             FALSE
tidyr     1.0.2  1.0.3              TRUE

  Binaries will be installed


installing the source packages ‘DO.db’, ‘GO.db’, ‘org.Hs.eg.db’, ‘pkgbuild’, ‘reactome.db’



```
# esATAC

<details>

* Version: 
* Source code: ???
* URL: https://github.com/rich-iannone/DiagrammeR
* BugReports: https://github.com/rich-iannone/DiagrammeR/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There is a binary version available but the source version is later:
      binary source needs_compilation
tidyr  1.0.2  1.0.3              TRUE

  Binaries will be installed


installing the source packages ‘BSgenome.Hsapiens.UCSC.hg19’, ‘DO.db’, ‘GenomeInfoDbData’, ‘GO.db’, ‘JASPAR2016’, ‘org.Hs.eg.db’, ‘TxDb.Hsapiens.UCSC.hg19.knownGene’



```
### CRAN

```

  There is a binary version available but the source version is later:
      binary source needs_compilation
tidyr  1.0.2  1.0.3              TRUE

  Binaries will be installed


installing the source packages ‘BSgenome.Hsapiens.UCSC.hg19’, ‘DO.db’, ‘GenomeInfoDbData’, ‘GO.db’, ‘JASPAR2016’, ‘org.Hs.eg.db’, ‘TxDb.Hsapiens.UCSC.hg19.knownGene’



```
# netSEM

<details>

* Version: 0.5.1
* Source code: https://github.com/cran/netSEM
* Date/Publication: 2018-11-28 17:00:03 UTC
* Number of recursive dependencies: 86

Run `revdep_details(,"netSEM")` for more info

</details>

## In both

*   checking whether package ‘netSEM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/rich/Documents/R_projects/DiagrammeR/revdep/checks.noindex/netSEM/new/netSEM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘netSEM’ ...
** package ‘netSEM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/rich/Documents/R_projects/DiagrammeR/revdep/library.noindex/netSEM/systemfonts/libs/systemfonts.so':
  dlopen(/Users/rich/Documents/R_projects/DiagrammeR/revdep/library.noindex/netSEM/systemfonts/libs/systemfonts.so, 6): Library not loaded: /opt/X11/lib/libfreetype.6.dylib
  Referenced from: /Users/rich/Documents/R_projects/DiagrammeR/revdep/library.noindex/netSEM/systemfonts/libs/systemfonts.so
  Reason: image not found
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘netSEM’
* removing ‘/Users/rich/Documents/R_projects/DiagrammeR/revdep/checks.noindex/netSEM/new/netSEM.Rcheck/netSEM’

```
### CRAN

```
* installing *source* package ‘netSEM’ ...
** package ‘netSEM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/rich/Documents/R_projects/DiagrammeR/revdep/library.noindex/netSEM/systemfonts/libs/systemfonts.so':
  dlopen(/Users/rich/Documents/R_projects/DiagrammeR/revdep/library.noindex/netSEM/systemfonts/libs/systemfonts.so, 6): Library not loaded: /opt/X11/lib/libfreetype.6.dylib
  Referenced from: /Users/rich/Documents/R_projects/DiagrammeR/revdep/library.noindex/netSEM/systemfonts/libs/systemfonts.so
  Reason: image not found
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘netSEM’
* removing ‘/Users/rich/Documents/R_projects/DiagrammeR/revdep/checks.noindex/netSEM/old/netSEM.Rcheck/netSEM’

```
# Rcwl

<details>

* Version: 1.2.1
* Source code: https://github.com/cran/Rcwl
* Date/Publication: 2019-11-01
* Number of recursive dependencies: 106

Run `revdep_details(,"Rcwl")` for more info

</details>

## In both

*   checking whether package ‘Rcwl’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/rich/Documents/R_projects/DiagrammeR/revdep/checks.noindex/Rcwl/new/Rcwl.Rcheck/00install.out’ for details.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

## Installation

### Devel

```
* installing *source* package ‘Rcwl’ ...
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in rm(.Random.seed, envir = .GlobalEnv) :
  object '.Random.seed' not found
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Warning in rm(.Random.seed, envir = .GlobalEnv) :
  object '.Random.seed' not found
Warning in system("which cwltool", intern = TRUE) :
  running command 'which cwltool' had status 1
Error: package or namespace load failed for ‘Rcwl’:
 .onLoad failed in loadNamespace() for 'Rcwl', details:
  call: fun(libname, pkgname)
  error: cwltool is not found, Please install cwltool first!
https://github.com/common-workflow-language/cwltool#install
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/rich/Documents/R_projects/DiagrammeR/revdep/checks.noindex/Rcwl/new/Rcwl.Rcheck/Rcwl’

```
### CRAN

```
* installing *source* package ‘Rcwl’ ...
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in rm(.Random.seed, envir = .GlobalEnv) :
  object '.Random.seed' not found
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Warning in rm(.Random.seed, envir = .GlobalEnv) :
  object '.Random.seed' not found
Warning in system("which cwltool", intern = TRUE) :
  running command 'which cwltool' had status 1
Error: package or namespace load failed for ‘Rcwl’:
 .onLoad failed in loadNamespace() for 'Rcwl', details:
  call: fun(libname, pkgname)
  error: cwltool is not found, Please install cwltool first!
https://github.com/common-workflow-language/cwltool#install
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/rich/Documents/R_projects/DiagrammeR/revdep/checks.noindex/Rcwl/old/Rcwl.Rcheck/Rcwl’

```
# trialr

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/trialr
* URL: https://github.com/brockk/trialr
* BugReports: https://github.com/brockk/trialr/issues
* Date/Publication: 2020-04-06 13:20:02 UTC
* Number of recursive dependencies: 102

Run `revdep_details(,"trialr")` for more info

</details>

## In both

*   R CMD check timed out
    

