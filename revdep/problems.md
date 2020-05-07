# dm

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/dm
* Date/Publication: 2020-05-04 11:20:02 UTC
* Number of recursive dependencies: 119

Run `revdep_details(,"dm")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    src_tbls:
      function(x, ...)
    src_tbls.dm:
      function(x)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# escalation

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/escalation
* Date/Publication: 2020-04-14 15:10:02 UTC
* Number of recursive dependencies: 88

Run `revdep_details(,"escalation")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      names for target but not for current
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════
      [ OK: 3148 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 8 ]
      1. Failure: get_dose_paths does what it should (@test_dose_paths.R#25) 
      2. Failure: get_dose_paths does what it should (@test_dose_paths.R#32) 
      3. Failure: get_dose_paths does what it should (@test_dose_paths.R#89) 
      4. Failure: get_dose_paths does what it should (@test_dose_paths.R#96) 
      5. Failure: get_dose_paths does what it should (@test_dose_paths.R#103) 
      6. Failure: get_dose_paths does what it should (@test_dose_paths.R#161) 
      7. Failure: get_dose_paths does what it should (@test_dose_paths.R#168) 
      8. Failure: get_dose_paths does what it should (@test_dose_paths.R#175) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

