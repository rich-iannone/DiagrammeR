# ahp

Version: 0.2.11

## Newly broken

*   checking examples ... ERROR
    ```
    ...
       Accord Hybrid Pilot Inconsistency
    1          14.1% 11.5%          7.4%
    2           4.8%  4.0%          1.5%
    3           0.6%  0.6%          6.8%
    4           1.7%  2.6%          0.0%
    5           0.9%  0.3%          3.2%
    6           1.6%  0.4%          2.3%
    7           5.1%  1.8%          8.1%
    8           2.8%  5.6%          0.0%
    9           2.4%  4.9%          0.0%
    10          0.3%  0.7%          0.4%
    11          1.5%  0.2%         10.2%
    > AnalyzeTable(carAhp)
    > 
    > #the vacation.ahp file provides an example with multiple decision makers
    > ahpFile <- system.file("extdata", "vacation.ahp", package="ahp")
    > vacationAhp <- Load(ahpFile)
    > Calculate(vacationAhp)
    > Visualize(vacationAhp)
    Error: 'set_global_graph_attrs' is not an exported object from 'namespace:DiagrammeR'
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: ahp
      [31m──[39m [31m1. Error: Visualize (@test-Visualize.R#8) [39m [31m─────────────────────────[39m
      'set_global_graph_attrs' is not an exported object from 'namespace:DiagrammeR'
      1: GetGraph(carAhp) at testthat/test-Visualize.R:8
      2: DiagrammeR::set_global_graph_attrs
      3: getExportedValue(pkg, name)
      4: stop(gettextf("'%s' is not an exported object from 'namespace:%s'", name, 
             getNamespaceName(ns)), call. = FALSE, domain = NA)
      
      ══ testthat results  ══════════════════════════════════════════════════
      OK: 463 SKIPPED: 0 FAILED: 1
      1. Error: Visualize (@test-Visualize.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘DiagrammeR::set_global_graph_attrs’
    ```

## In both

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//RtmpmXnGzB/Rd2pdf8fef3cc355c7'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# deisotoper

Version: 0.0.3

## In both

*   checking whether package ‘deisotoper’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/deisotoper/new/deisotoper.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘deisotoper’ ...
** package ‘deisotoper’ successfully unpacked and MD5 sums checked
** R
** inst
** preparing package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/deisotoper/rJava/libs/rJava.so':
  dlopen(/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/deisotoper/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/deisotoper/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘deisotoper’
* removing ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/deisotoper/new/deisotoper.Rcheck/deisotoper’

```
### CRAN

```
* installing *source* package ‘deisotoper’ ...
** package ‘deisotoper’ successfully unpacked and MD5 sums checked
** R
** inst
** preparing package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/deisotoper/rJava/libs/rJava.so':
  dlopen(/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/deisotoper/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/deisotoper/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘deisotoper’
* removing ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/deisotoper/old/deisotoper.Rcheck/deisotoper’

```
# greta

Version: 0.2.3

## In both

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//Rtmp5wmak8/Rd2pdf9d642402322e'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# gSEM

Version: 0.4.3.4

## In both

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//RtmpI04TSW/Rd2pdf9ab33aa2bd65'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# HydeNet

Version: 0.10.7

## In both

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > data(PE, package="HydeNet")
    > Net <- HydeNetwork(~ wells + 
    +                      pe | wells + 
    +                      d.dimer | pregnant*pe + 
    +                      angio | pe + 
    +                      treat | d.dimer*angio + 
    +                      death | pe*treat,
    +                      data = PE) 
    >   
    >                  
    > compiledNet <- compileJagsModel(Net, n.chains=5)
    Error: .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/HydeNet/rjags/libs/rjags.so':
      dlopen(/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/HydeNet/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/HydeNet/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      16: tryCatch(loadNamespace(name), error = function(e) stop(e))
      17: tryCatchList(expr, classes, parentenv, handlers)
      18: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      19: value[[3L]](cond)
      
      ══ testthat results  ══════════════════════════════════════════════════
      OK: 60 SKIPPED: 0 FAILED: 5
      1. Error: (unknown) (@test-HydePosterior.R#11) 
      2. Error: (unknown) (@test-bindPosterior.R#12) 
      3. Error: compileJagsModel returns an object of class 'compiledHydeNetwork' (@test-compileJagsModel.R#14) 
      4. Error: (unknown) (@test-print.HydePosterior.R#11) 
      5. Error: compileDecisionModel (@test_compileDecisionModel.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//RtmpUYyeyl/Rd2pdfb8b048708765'
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: nnet
    Quitting from lines 314-325 (DecisionNetworks.Rmd) 
    Error: processing vignette 'DecisionNetworks.Rmd' failed with diagnostics:
    .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/HydeNet/rjags/libs/rjags.so':
      dlopen(/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/HydeNet/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/HydeNet/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# lavaanPlot

Version: 0.3.0

## In both

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//Rtmp8safLL/Rd2pdfb36c3fc939'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        doc   4.9Mb
    ```

# m2b

Version: 1.0

## In both

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//RtmpUvqexu/Rd2pdfc512342b3cda'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘e1071’ ‘utils’
      All declared Imports should be used.
    ```

# markovchain

Version: 0.6.9.8-1

## In both

*   checking whether package ‘markovchain’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/markovchain/new/markovchain.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘markovchain’ ...
** package ‘markovchain’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 0_classesAndMethods.cpp -o 0_classesAndMethods.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 0_ctmcClassesAndMethodsSAI.cpp -o 0_ctmcClassesAndMethodsSAI.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 1_ctmcFunctions4Fitting.cpp -o 1_ctmcFunctions4Fitting.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 1_ctmcProbabilistic.cpp -o 1_ctmcProbabilistic.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 1_fitHigherOrder.cpp -o 1_fitHigherOrder.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 1_functions4Fitting.cpp -o 1_functions4Fitting.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 2_probabilistic.cpp -o 2_probabilistic.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 3_multinomCI.cpp -o 3_multinomCI.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c sampler.cpp -o sampler.o
sampler.cpp:107:27: warning: 'sort_index<arma::Mat<double> >' is deprecated [-Wdeprecated-declarations]
  arma::uvec perm = arma::sort_index(prob, 1); //descending sort of index
                          ^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/fn_sort_index.hpp:40:1: note: 'sort_index<arma::Mat<double> >' has been explicitly marked deprecated here
arma_deprecated
^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:273:44: note: expanded from macro 'arma_deprecated'
    #define arma_deprecated __attribute__((__deprecated__))
                                           ^
sampler.cpp:108:16: warning: 'sort<arma::Col<double> >' is deprecated [-Wdeprecated-declarations]
  prob = arma::sort(prob, 1);  // descending sort of prob
               ^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/fn_sort.hpp:45:1: note: 'sort<arma::Col<double> >' has been explicitly marked deprecated here
arma_deprecated
^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:273:44: note: expanded from macro 'arma_deprecated'
    #define arma_deprecated __attribute__((__deprecated__))
                                           ^
sampler.cpp:169:27: warning: 'sort_index<arma::Mat<double> >' is deprecated [-Wdeprecated-declarations]
  arma::uvec perm = arma::sort_index(prob, 1); //descending sort of index
                          ^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/fn_sort_index.hpp:40:1: note: 'sort_index<arma::Mat<double> >' has been explicitly marked deprecated here
arma_deprecated
^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:273:44: note: expanded from macro 'arma_deprecated'
    #define arma_deprecated __attribute__((__deprecated__))
                                           ^
sampler.cpp:170:16: warning: 'sort<arma::Col<double> >' is deprecated [-Wdeprecated-declarations]
  prob = arma::sort(prob, 1);  // descending sort of prob
               ^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/fn_sort.hpp:45:1: note: 'sort<arma::Col<double> >' has been explicitly marked deprecated here
arma_deprecated
^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:273:44: note: expanded from macro 'arma_deprecated'
    #define arma_deprecated __attribute__((__deprecated__))
                                           ^
4 warnings generated.
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o markovchain.so 0_classesAndMethods.o 0_ctmcClassesAndMethodsSAI.o 1_ctmcFunctions4Fitting.o 1_ctmcProbabilistic.o 1_fitHigherOrder.o 1_functions4Fitting.o 2_probabilistic.o 3_multinomCI.o RcppExports.o sampler.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [markovchain.so] Error 1
ERROR: compilation failed for package ‘markovchain’
* removing ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/markovchain/new/markovchain.Rcheck/markovchain’

```
### CRAN

```
* installing *source* package ‘markovchain’ ...
** package ‘markovchain’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 0_classesAndMethods.cpp -o 0_classesAndMethods.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 0_ctmcClassesAndMethodsSAI.cpp -o 0_ctmcClassesAndMethodsSAI.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 1_ctmcFunctions4Fitting.cpp -o 1_ctmcFunctions4Fitting.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 1_ctmcProbabilistic.cpp -o 1_ctmcProbabilistic.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 1_fitHigherOrder.cpp -o 1_fitHigherOrder.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 1_functions4Fitting.cpp -o 1_functions4Fitting.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 2_probabilistic.cpp -o 2_probabilistic.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c 3_multinomCI.cpp -o 3_multinomCI.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/Rcpp/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppParallel/include" -I"/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c sampler.cpp -o sampler.o
sampler.cpp:107:27: warning: 'sort_index<arma::Mat<double> >' is deprecated [-Wdeprecated-declarations]
  arma::uvec perm = arma::sort_index(prob, 1); //descending sort of index
                          ^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/fn_sort_index.hpp:40:1: note: 'sort_index<arma::Mat<double> >' has been explicitly marked deprecated here
arma_deprecated
^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:273:44: note: expanded from macro 'arma_deprecated'
    #define arma_deprecated __attribute__((__deprecated__))
                                           ^
sampler.cpp:108:16: warning: 'sort<arma::Col<double> >' is deprecated [-Wdeprecated-declarations]
  prob = arma::sort(prob, 1);  // descending sort of prob
               ^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/fn_sort.hpp:45:1: note: 'sort<arma::Col<double> >' has been explicitly marked deprecated here
arma_deprecated
^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:273:44: note: expanded from macro 'arma_deprecated'
    #define arma_deprecated __attribute__((__deprecated__))
                                           ^
sampler.cpp:169:27: warning: 'sort_index<arma::Mat<double> >' is deprecated [-Wdeprecated-declarations]
  arma::uvec perm = arma::sort_index(prob, 1); //descending sort of index
                          ^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/fn_sort_index.hpp:40:1: note: 'sort_index<arma::Mat<double> >' has been explicitly marked deprecated here
arma_deprecated
^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:273:44: note: expanded from macro 'arma_deprecated'
    #define arma_deprecated __attribute__((__deprecated__))
                                           ^
sampler.cpp:170:16: warning: 'sort<arma::Col<double> >' is deprecated [-Wdeprecated-declarations]
  prob = arma::sort(prob, 1);  // descending sort of prob
               ^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/fn_sort.hpp:45:1: note: 'sort<arma::Col<double> >' has been explicitly marked deprecated here
arma_deprecated
^
/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/library.noindex/markovchain/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:273:44: note: expanded from macro 'arma_deprecated'
    #define arma_deprecated __attribute__((__deprecated__))
                                           ^
4 warnings generated.
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o markovchain.so 0_classesAndMethods.o 0_ctmcClassesAndMethodsSAI.o 1_ctmcFunctions4Fitting.o 1_ctmcProbabilistic.o 1_fitHigherOrder.o 1_functions4Fitting.o 2_probabilistic.o 3_multinomCI.o RcppExports.o sampler.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [markovchain.so] Error 1
ERROR: compilation failed for package ‘markovchain’
* removing ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/markovchain/old/markovchain.Rcheck/markovchain’

```
# muir

Version: 0.1.0

## In both

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//RtmpZYXA4i/Rd2pdfc63830e28359'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# pmap

Version: 0.3.2

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    create_pmap_graph: possible error in add_edges_from_table(p, table =
      edges %>% select(-amount), from_col = "from", to_col = "to",
      ndf_mapping = "name_without_space"): unused argument (ndf_mapping =
      "name_without_space")
    ```

## In both

*   checking examples ... ERROR
    ```
    ...
      [3m[90m<chr>[39m[23m            [3m[90m<chr>[39m[23m              [3m[90m<int>[39m[23m
    [90m1[39m Event 1 (normal) Event 1 (normal)      10
    [90m2[39m Event 1 (normal) Event 10 (target)      9
    [90m3[39m Event 1 (normal) Event 2 (normal)       4
    [90m4[39m Event 1 (normal) Event 3 (normal)       9
    [90m5[39m Event 1 (normal) Event 4 (normal)       7
    [90m6[39m Event 1 (normal) Event 5 (normal)      13
    > #  # A tibble: 6 x 3
    > #    from             to                amount
    > #    <chr>            <chr>              <int>
    > #  1 Event 1 (normal) Event 1 (normal)       8
    > #  2 Event 1 (normal) Event 10 (target)     10
    > #  3 Event 1 (normal) Event 2 (normal)      12
    > #  4 Event 1 (normal) Event 3 (normal)       9
    > #  5 Event 1 (normal) Event 4 (normal)       7
    > #  6 Event 1 (normal) Event 5 (normal)      10
    > p <- create_pmap_graph(nodes, edges, target_types = c("target"))
    Error in add_edges_from_table(p, table = edges %>% select(-amount), from_col = "from",  : 
      unused argument (ndf_mapping = "name_without_space")
    Calls: create_pmap_graph
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ══════════════════════════════════════════════════
      OK: 59 SKIPPED: 0 FAILED: 9
      1. Error: create_pmap() should handle simple graph (@test_create_pmap.R#14) 
      2. Error: create_pmap() should handle complex graph (@test_create_pmap.R#40) 
      3. Error: create_pmap_graph() (@test_create_pmap_graph.R#30) 
      4. Error: prune_edges() should be able prune nothing (@test_prune_edges.R#4) 
      5. Error: prune_edges() should be able prune half of the edges (@test_prune_edges.R#30) 
      6. Error: prune_edges() should be able prune all of the edges (@test_prune_edges.R#58) 
      7. Error: prune_nodes() should be able prune nothing (@test_prune_nodes.R#4) 
      8. Error: prune_nodes() should be able prune half of the nodes (@test_prune_nodes.R#33) 
      9. Error: prune_nodes() should be able prune all of the nodes (@test_prune_nodes.R#63) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//RtmpRaw1Rt/Rd2pdfd9036715bd07'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# pMineR

Version: 0.31

## In both

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//RtmpfkJHs9/Rd2pdfd91dcde8315'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# PRISMAstatement

Version: 1.0.1

## In both

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//Rtmp0diZJr/Rd2pdfe401145e281f'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# processmapR

Version: 0.2.1

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    process_map: no visible global function definition for
      ‘set_global_graph_attrs’
    Undefined global functions or variables:
      set_global_graph_attrs
    ```

## In both

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//RtmpqNhKfD/Rd2pdfeab54944f4c5'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# radiant.model

Version: 0.8.0

## In both

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//RtmpAPXJZL/Rd2pdff19b2db1c190'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# sbfc

Version: 1.0.1

## Newly broken

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//RtmpANnhBc/Rd2pdff6ec9a62755'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

## Newly fixed

*   R CMD check timed out
    

# sem

Version: 3.1-9

## In both

*   checking whether package ‘sem’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/sem/new/sem.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sem’ ...
** package ‘sem’ successfully unpacked and MD5 sums checked
** libs
clang -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c csemnlm.c -o csemnlm.o
clang -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c uncmin.c -o uncmin.o
clang++  -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c csem.cpp -o csem.o
clang -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c init.c -o init.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o sem.so csemnlm.o uncmin.o csem.o init.o -lm -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [sem.so] Error 1
ERROR: compilation failed for package ‘sem’
* removing ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/sem/new/sem.Rcheck/sem’

```
### CRAN

```
* installing *source* package ‘sem’ ...
** package ‘sem’ successfully unpacked and MD5 sums checked
** libs
clang -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c csemnlm.c -o csemnlm.o
clang -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c uncmin.c -o uncmin.o
clang++  -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c csem.cpp -o csem.o
clang -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c init.c -o init.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o sem.so csemnlm.o uncmin.o csem.o init.o -lm -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [sem.so] Error 1
ERROR: compilation failed for package ‘sem’
* removing ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/sem/old/sem.Rcheck/sem’

```
# simmer.plot

Version: 0.1.12

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘simmer.plot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.trajectory
    > ### Title: Plot Method for 'trajectory' Objects
    > ### Aliases: plot.trajectory
    > 
    > ### ** Examples
    > 
    > x <- trajectory() %>%
    +   seize("res", 1) %>%
    +   timeout(1) %>%
    +   release("res", 1) %>%
    +   rollback(3)
    > 
    > plot(x)
    `()` INFO: created a new selection of 1 node
    `()` INFO: there is an active selection of 1 node
    `()` INFO: cleared an existing selection of 1 node
    Error: 'set_global_graph_attrs' is not an exported object from 'namespace:DiagrammeR'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘DiagrammeR::set_global_graph_attrs’
    ```

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      15: DiagrammeR::set_global_graph_attrs
      16: getExportedValue(pkg, name)
      17: stop(gettextf("'%s' is not an exported object from 'namespace:%s'", name, 
             getNamespaceName(ns)), call. = FALSE, domain = NA)
      
      ══ testthat results  ══════════════════════════════════════════════════
      OK: 52 SKIPPED: 0 FAILED: 5
      1. Error: a single-node trajectory is correctly converted to graph (@test-plot-trajectory.R#23) 
      2. Error: a simple trajectory is correctly converted to graph (@test-plot-trajectory.R#31) 
      3. Error: a rollback with variable amount is correctly converted to graph (@test-plot-trajectory.R#35) 
      4. Error: a complex trajectory is correctly converted to graph (@test-plot-trajectory.R#68) 
      5. Error: edges are removed for ignored subtrajectories in Clone (@test-plot-trajectory.R#92) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking PDF version of manual without hyperrefs or index ... ERROR
    ```
    Re-running with no redirection of stdout/stderr.
    Hmm ... looks like a package
    You may want to clean up by 'rm -rf /var/folders/62/_vr8mj_16ys7r5hz03vnlh_c0000gn/T//RtmpcWiqx7/Rd2pdf102d05858db82'
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# teachingApps

Version: 1.0.2

## In both

*   R CMD check timed out
    

# umx

Version: 2.0.2

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘OpenMx’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# xgboost

Version: 0.6.4.1

## In both

*   checking whether package ‘xgboost’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/xgboost/new/xgboost.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘xgboost’ ...
** package ‘xgboost’ successfully unpacked and MD5 sums checked
configure: creating ./config.status
config.status: creating src/Makevars
** libs
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG -I./include -I./dmlc-core/include -I./rabit/include -I. -DXGBOOST_STRICT_R_MODE=1 -DDMLC_LOG_BEFORE_THROW=0 -DDMLC_ENABLE_STD_THREAD=0 -DDMLC_DISABLE_STDIN=1 -DDMLC_LOG_CUSTOMIZE=1 -DXGBOOST_CUSTOMIZE_LOGGER=1 -DRABIT_CUSTOMIZE_MSG_ -DRABIT_STRICT_CXX98_  -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -c xgboost_R.cc -o xgboost_R.o
clang: error: unsupported option '-fopenmp'
make: *** [xgboost_R.o] Error 1
ERROR: compilation failed for package ‘xgboost’
* removing ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/xgboost/new/xgboost.Rcheck/xgboost’

```
### CRAN

```
* installing *source* package ‘xgboost’ ...
** package ‘xgboost’ successfully unpacked and MD5 sums checked
configure: creating ./config.status
config.status: creating src/Makevars
** libs
clang++ -std=gnu++11 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG -I./include -I./dmlc-core/include -I./rabit/include -I. -DXGBOOST_STRICT_R_MODE=1 -DDMLC_LOG_BEFORE_THROW=0 -DDMLC_ENABLE_STD_THREAD=0 -DDMLC_DISABLE_STDIN=1 -DDMLC_LOG_CUSTOMIZE=1 -DXGBOOST_CUSTOMIZE_LOGGER=1 -DRABIT_CUSTOMIZE_MSG_ -DRABIT_STRICT_CXX98_  -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -c xgboost_R.cc -o xgboost_R.o
clang: error: unsupported option '-fopenmp'
make: *** [xgboost_R.o] Error 1
ERROR: compilation failed for package ‘xgboost’
* removing ‘/Users/riannone/Documents/R_oss_work/DiagrammeR/revdep/checks.noindex/xgboost/old/xgboost.Rcheck/xgboost’

```
