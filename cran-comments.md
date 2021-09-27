## Test environments
* ubuntu 20.04 (local + GitHub Actions), R 4.1.1
* macOS 10.15 (local + GitHub Actions), R 4.1.1
* windows 2019 (on GitHub Actions), R 4.1.1

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs

## Downstream dependencies
* There are four downstream dependencies on CRAN: `hal9001`, `haldensify`,
  `cvCovEst`, `lmtp`.
* There is one downstream dependency on Bioconductor: `scPCA`.

## Resubmission
* This is an update to an existing CRAN package, submitted after fixing:
  ```
    Found the following (possibly) invalid URLs:
         URL: https://tlverse.org/origami (moved to
    https://tlverse.org/origami/)
           From: DESCRIPTION
           Status: 301
           Message: Moved Permanently

    Please change http --> https, add trailing slashes, or follow moved
    content as appropriate.

    Please fix and resubmit.
  ```
