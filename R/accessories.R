

#' writeOut writes dataframe dd out to a csv file named n.
#'
#' @param dd the input data frame
#' @param n the name of the output csv file (include extension)
#' @return This returns the data frame for further chaining with magrittr
#' @export
writeOut <- function(dd, n){
    write.csv(dd, file=n, quote=F, row.names=F)
    return(dd)
}

