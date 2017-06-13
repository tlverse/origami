library(origami)
library(data.table)
context("Combine Results")

raw_input=list(a=list(1,2,3,4))
output=combine_results(raw_input, combiners=NULL, smart_combiners = F)
test_that("combine_results falls back by returning unprocessed input",expect_equal(raw_input,output))

nrows=10
ncols=10
matrix_results=list(a=list(array(0,dim=c(nrows,ncols,1))))