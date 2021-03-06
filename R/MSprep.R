#' This module implements parsing and data frame construction for sieve
#' MS and MS/MS data. See the function example() for a demostration of
#' its capabilities.
#'
#' @examples
#' EXAMPLE of how to use this package
#' example <- function(){
#' 
#' library(ggplot2)
#' library(reshape2)
#' library(magrittr)
#' 
#' writeOut <- function(dd, n){
#'     write.csv(dd, file=n, quote=F, row.names=F)
#'     return(dd)
#' }
#' 
#' 
#' fname <- "example_sieve_data_file.csv"
#' 
#' example_prepare_data <- function(fname){
#' 
#'     fname %>% 
#'     #
#'     # Read in the data file, and immediately write out a copy as v1.csv
#'     p(read_data_file)() %>% 
#'     p(writeOut)("v1.csv") %>%
#' 
#'     # Remove columns that contain "blank", "pool", or "pr", and write out a
#'     # copy of this intermediate data as v2.csv
#'     p(remove_cols)(c(function(x){grep("blank",names(x),ignore.case=T)}
#'                      , function(x){grep("pool",names(x),ignore.case=T)}
#'                      , function(x){grep("pr",names(x),ignore.case=T)})) %>% 
#'     p(writeOut)("v2.csv") %>%
#' 
#'     # Transpose this smaller frame, treating the 1st three columns as row-information. This row information
#'     # is processed into the names(...) of the transposed data frame, and the original names(...) become the
#'     # "sample_id" column.
#'     p(transpose)(1:3, "sample_id") %>% 
#'     p(writeOut)("v3.csv")
#' 
#' 
#' }
#' 
#' 
#' }
"_PACKAGE"
