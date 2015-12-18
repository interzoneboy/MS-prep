

f1 <- function(x){return(x+1)}
f2 <- function(x){return(x+2)}
f3 <- function(x){return(x+3)}

appl <- function(x,f){
    #ret <- function(x){
    #    y(x())
    #}
    #return(ret)
    f(x)
}

bla <- Reduce(appl, c(f1, f2, f3), init=0)
