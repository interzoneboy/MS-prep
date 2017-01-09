
source("../filter_corrected_faster.R", chdir=TRUE)





create_iso <- function(isoDat, mz, rt, int, type_parent){

    massShift <- runif(1, min=isoDat$min, max=isoDat$max)
    newInt <- int * 0.2 #isoDat$pctInt
    return(list(mass=(mz + massShift), rt=rt, intensity=newInt, type=paste0(type_parent,"_",isoDat$type)))

}

create_fragment <- function(fragDat, mz, rt, int, type_parent){

    massShift <- runif(1, min=fragDat$min, max=fragDat$max)
    newInt <- int * runif(1, min=0.0, max=0.7)
    return(list(mass=(mz - massShift), rt=rt, intensity=newInt, type=paste0(type_parent,"_",fragDat$type)))

}


getPeak <- function(peakList, mass, rt, intensity, type_in){
    
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
                return( c(pl, list(ret)) )
            }else{
                return( pl )
            }
        }
        return(ret)
    }


    myself <- list(mass=mass, rt=rt, intensity=intensity, type=type_in)
    peakList_local <- c(peakList_local, list(myself))

    peakList_local <- appendIfGood(peakList_local, perhaps(0.85, create_iso))(m1, mass, rt, intensity, type_in)
    peakList_local <- appendIfGood(peakList_local, perhaps(0.45, create_iso))(m2, mass, rt, intensity, type_in)
    peakList_local <- appendIfGood(peakList_local, perhaps(0.25, create_iso))(m3, mass, rt, intensity, type_in)


    myFrag <- perhaps(0.5, create_fragment)(frag1, mass, rt, intensity, type_in)
    peakList_local <- appendIfGood(peakList_local, function(){myFrag})()
    if(!is.na(myFrag)){
        peakList_local <- appendIfGood(peakList_local, perhaps(0.85, create_iso))(m1, myFrag$mass, myFrag$rt, myFrag$intensity, myFrag$type)
        peakList_local <- appendIfGood(peakList_local, perhaps(0.45, create_iso))(m2, myFrag$mass, myFrag$rt, myFrag$intensity, myFrag$type)
        peakList_local <- appendIfGood(peakList_local, perhaps(0.25, create_iso))(m3, myFrag$mass, myFrag$rt, myFrag$intensity, myFrag$type)
    }

    return(peakList_local)
}


#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################

item <- function(mass, rt, intens, type){
    return(list(mass=mass, rt=rt, intensity=intens, type=type))
}

test_peaks <- list(
    item(62.0, 5.0, 1000.0, "p"),
    item(64.0, 15.0, 500.0, "p"),
    item(84.0, 17.0, 1000.0, "p"),
    item(324.0, 20.0, 1000.0, "p")
)



nuPeaks <- list()

counter <- 1
for(zz in test_peaks){

    nuPeaks <- getPeak(nuPeaks, zz$mass, zz$rt, zz$intensity, zz$type)

}

g <- function(nn){
    return( sapply(nuPeaks, function(x){x[[nn]]}) )
}


ret <- data.frame(mass=g("mass"), rt=g("rt"), intensity=g("intensity"), type=g("type"))
