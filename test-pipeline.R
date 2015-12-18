

f1 <- function(x){return(x+1)}
f2 <- function(x){return(x+2)}
f3 <- function(x){return(x+3)}

appl <- function(x,y){
    ret <- function(x){
        y(x())
    }
    return(ret)
}

bla <- Reduce(appl, c(f1, f2, f3), init=function(i){return(i)})
