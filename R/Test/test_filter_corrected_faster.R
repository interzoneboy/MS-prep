
source("../filter_corrected_faster.R", chdir=TRUE)


getUni <- function(item){
    return( runif(1, min=item$min, max=item$max) )
}


item <- function(mass, rt, fragRelInt){
    return(list(mass=mass, rt=rt, fragRelInt=fragRelInt))
}

test_peaks <- c(
    item(62.0, 5.0, T, T, F, 0.5),
    item(64.0, 5.0, T, T, F, 0.5)
)


getPeak <- function(peakList, mass, rt, intensity){
    
    peakList_local <- peakList
    perhaps <- function( chance, f ){
        ret <- function( ... ){
            if( runif(1) < chance ){
                return( f(...) )
            }else{
                return( NA )
            }
        }
        return(ret)
    }

    appendIfGood <- function(pl, f){
        ret <- function(...){
            ret <- f(...)
            if(!is.na(ret)){
                return( append(pl, ret) )
            }else{
                return( pl )
            }
        }
        return(ret)
    }

    peakList_local <- append()

    myself <- list(mass=mass, rt=rt, intensity=intensity)

    peakList_local <- appendIfGood(peakList_local, perhaps(0.85, create_iso))(m1, mass, rt, intensity)
    peakList_local <-appendIfGood(peakList_local, perhaps(0.45, create_iso))(m2, mass, rt, intensity)
    peakList_local <-appendIfGood(peakList_local, perhaps(0.25, create_iso))(m3, mass, rt, intensity)


    myFrag <- perhaps(0.5, create_fragment)(frag1, mass, rt, intensity)
    peakList_local <- appendIfGood(peakList_local, myFrag)
    if(!is.na(myFrag)){
        peakList_local <- appendIfGood(peakList_local, perhaps(0.85, create_iso))(m1, myFrag$mass, myFrag$rt, myFrag$intensity)
        peakList_local <- appendIfGood(peakList_local, perhaps(0.45, create_iso))(m2, myFrag$mass, myFrag$rt, myFrag$intensity)
        peakList_local <- appendIfGood(peakList_local, perhaps(0.25, create_iso))(m3, myFrag$mass, myFrag$rt, myFrag$intensity)
    }

    return(peakList_local)
}

nuPeaks <- list()

counter <- 1
for(zz in test_peaks){

    if(zz$iso1==TRUE){
        nuPeaks[[counter]] <- create_iso(zz$mass, zz$rt, m1)
        counter <- counter + 1
    }
    if(zz$iso2==TRUE){
        nuPeaks[[counter]] <- create_iso(zz$mass, zz$rt, m2)
        counter <- counter + 1
    }
    if(zz$iso3==TRUE){
        nuPeaks[[counter]] <- create_iso(zz$mass, zz$rt, m3)
    }
