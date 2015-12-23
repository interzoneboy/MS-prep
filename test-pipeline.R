##f1 <- function(x){return(x+1)}
##f2 <- function(x){return(x+2)}
##f3 <- function(x){return(x+3)}
##
##appl <- function(x,f){
##    #ret <- function(x){
##    #    y(x())
##    #}
##    #return(ret)
##    f(x)
##}
##
##bla <- Reduce(appl, c(f1, f2, f3), init=0)

library(ggplot2)
library(reshape2)
library(magrittr)

wrr <- function(dd, n){
    write.csv(dd, file=n, quote=F, row.names=F)
    return(dd)
}


fname <- ""

example_prepare_data <- function(fname){

    fname %>% 
    p(read_data_file)() %>% p(wrr)("v1.csv") %>%
    p(remove_cols)(c(function(x){grep("blank",names(x),ignore.case=T)}
                     , function(x){grep("pool",names(x),ignore.case=T)}
                     , function(x){grep("pr",names(x),ignore.case=T)})) %>% p(wrr)("v2.csv") %>%
    p(transpose)(1:3, "sample_id") %>% p(wrr)("v3.csv")


}
