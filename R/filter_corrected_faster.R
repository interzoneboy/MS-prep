library(reshape2)
library(plyr)
library(ggplot2)

#' @export
mass.error <- 0.005

#' @export
max.rt.drift_in <- 0.05

#' @export
m1 <- list(min=(1.0034 - mass.error), max=(1.0034 + mass.error), type="m1")

#' @export
m2 <- list(min=(2.0043 - mass.error), max=(2.0043 + mass.error), type="m2")

#' @export
m3 <- list(min=(3.0077 - mass.error), max=(3.0077 + mass.error), type="m3")


#' @export
frag1 <- list(min=(60.0212 - mass.error), max=(60.0212 + mass.error), type="frag1")

#' @export
to.elim.2 <- list(m1=list(m1,"+"), 
                m2=list(m2,"+"), 
                m3=list(m3,"+")) 

#' @export
to.elim.1 <- list(frag1=list(frag1, "-"))

countEnv <- new.env()
assign("testCount1", 0, envir=countEnv)
assign("testCount2", 0, envir=countEnv)

filter <- function(peakFrameIn_in, mzColName, timeColName, idColName, massShiftList1=to.elim.1, massShiftList2=to.elim.2, max.rt.drift=max.rt.drift_in){

    # Sort the frames by retention time
    peakFrameIn <- peakFrameIn_in[order(peakFrameIn_in[[timeColName]]),]

    # This is the order of masses to consider, largest to smallest = first to last.
    frameOrder <- order(peakFrameIn[[mzColName]], decreasing=TRUE, na.last=TRUE)

    ref.dir <- vector(mode="character", length=dim(peakFrameIn)[[1]])
    ref.type <- vector(mode="character", length=dim(peakFrameIn)[[1]])
    ref.parent <- vector(mode="character", length=dim(peakFrameIn)[[1]])


    # Main Loop Through all the peaks (go through in frameOrder)
    for(i in frameOrder) {

        peak1 <- peakFrameIn[i,]
        peak1.mz <- peak1[[mzColName]]
        peak1.rt <- peak1[[timeColName]]
        peak1.id <- peak1[[idColName]]

        print(paste0("Working on mz ",peak1.mz," time ", peak1.rt, " frame ", i, ". Num discarded: ", get("testCount1",envir=countEnv)))
        
        # If this peak has not been marked as an isotope...
        if(!(ref.type[[i]] %in% names(to.elim.2)) ){
            # Then grab an RT-error sized window centred on it...
            topInd <- i
            bottomInd <- i
            while( (topInd > 1)  && (abs(peakFrameIn[(topInd-1), timeColName] - peakFrameIn[i, timeColName]) <= max.rt.drift)) {
                topInd <- topInd - 1
            }
            while( (bottomInd < dim(peakFrameIn)[[1]]) && (abs(peakFrameIn[(bottomInd+1), timeColName] - peakFrameIn[i, timeColName]) <= max.rt.drift)) {
                bottomInd <- bottomInd + 1
            }

            #subFrame <- peakFrameIn[topInd:bottomInd,]
            for(j in topInd:bottomInd){
                if(j != i){

                    peak2 <- peakFrameIn[j,]
                    peak2.mz <- peak2[[mzColName]]
                    peak2.rt <- peak2[[timeColName]]
                    peak2.id <- peak2[[idColName]]

                    # Only do this loop if the peak hasn't been marked as a fragment.
                    if(!(ref.type[[i]] %in% names(to.elim.1))){
                        for(elim in names(massShiftList1)){

                            dm.low <- massShiftList1[[elim]][[1]][["min"]]
                            dm.high <- massShiftList1[[elim]][[1]][["max"]]

                            if(massShiftList1[[elim]][[2]] == "+"){
                                if((massShiftList1[[elim]][[1]]$type=="frag1") || (peak1.id < peak2.id)){
                                    if((peak2.mz >= (peak1.mz + dm.low)) && (peak2.mz <= (peak1.mz + dm.high)) ){
                                        assign("testCount1", (get("testCount1",envir=countEnv)+1), envir=countEnv)
                                        ref.type[[j]] <- elim
                                        ref.dir[[j]] <- "higher"
                                        ref.parent[[j]] <- peak1.id
                                    }
                                }
                            }

                            if(massShiftList1[[elim]][[2]] == "-"){
                                wtf1 <<- list(p1=peak1, p2=peak2, lo=dm.low, hi=dm.high, topInd=topInd, bottomInd=bottomInd)
                                if((massShiftList1[[elim]][[1]]$type=="frag1") || (peak1.id < peak2.id)){
                                    if((peak2.mz <= (peak1.mz - dm.low)) && (peak2.mz >= (peak1.mz - dm.high)) ){
                                        assign("testCount1", (get("testCount1",envir=countEnv)+1), envir=countEnv)
                                        ref.type[[j]] <- elim
                                        ref.dir[[j]] <- "lower"
                                        ref.parent[[j]] <- peak1.id
                                    }
                                }
                            }
                        }
                    }

                    for(elim in names(massShiftList2)){

                        dm.low <- massShiftList2[[elim]][[1]][["min"]]
                        dm.high <- massShiftList2[[elim]][[1]][["max"]]

                        if(massShiftList2[[elim]][[2]] == "+"){
                            if((massShiftList2[[elim]][[1]]$type=="frag1") || (peak1.id < peak2.id)){
                                if((peak2.mz >= (peak1.mz + dm.low)) && (peak2.mz <= (peak1.mz + dm.high)) ){
                                    assign("testCount1", (get("testCount1",envir=countEnv)+1), envir=countEnv)
                                    ref.type[[j]] <- elim
                                    ref.dir[[j]] <- "higher"
                                    ref.parent[[j]] <- peak1.id
                                }
                            }
                        }
                        if(massShiftList2[[elim]][[2]] == "-"){
                            wtf2 <<- list(p1=peak1, p2=peak2, lo=dm.low, hi=dm.high, topInd=topInd, bottomInd=bottomInd)
                            if((massShiftList2[[elim]][[1]]$type=="frag1") || (peak1.id < peak2.id)){
                                if((peak2.mz <= (peak1.mz - dm.low)) && (peak2.mz >= (peak1.mz - dm.high)) ){
                                    assign("testCount1", (get("testCount1",envir=countEnv)+1), envir=countEnv)
                                    ref.type[[j]] <- elim
                                    ref.dir[[j]] <- "lower"
                                    ref.parent[[j]] <- peak1.id
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return(list(sortedFrame=peakFrameIn,
                ref.dir=ref.dir,
                ref.type=ref.type,
                ref.parent=ref.parent,
                keep=peakFrameIn$id[which(ref.type=="")],
                discard=peakFrameIn$id[which(ref.type!="")]))
}
    

# filter_fragLowerIntensity is the same as filter, but 
# is more restrictive in the sense that the fragment peaks
# must have lower intensity (so higher frame number) than
# their respective parent peaks.
filter_fragLowerIntensity <- function(peakFrameIn_in, mzColName, timeColName, idColName, massShiftList1=to.elim.1, massShiftList2=to.elim.2, max.rt.drift=max.rt.drift_in){

    # Sort the frames by retention time
    peakFrameIn <- peakFrameIn_in[order(peakFrameIn_in[[timeColName]]),]

    # This is the order of masses to consider, largest to smallest = first to last.
    frameOrder <- order(peakFrameIn[[mzColName]], decreasing=TRUE, na.last=TRUE)

    ref.dir <- vector(mode="character", length=dim(peakFrameIn)[[1]])
    ref.type <- vector(mode="character", length=dim(peakFrameIn)[[1]])
    ref.parent <- vector(mode="character", length=dim(peakFrameIn)[[1]])


    # Main Loop Through all the peaks (go through in frameOrder)
    for(i in frameOrder) {

        peak1 <- peakFrameIn[i,]
        peak1.mz <- peak1[[mzColName]]
        peak1.rt <- peak1[[timeColName]]
        peak1.id <- peak1[[idColName]]

        print(paste0("Working on mz ",peak1.mz," time ", peak1.rt, " frame ", i, ". Num discarded: ", get("testCount1",envir=countEnv)))
        
        # If this peak has not been marked as an isotope...
        if(!(ref.type[[i]] %in% names(to.elim.2)) ){
            # Then grab an RT-error sized window centred on it...
            topInd <- i
            bottomInd <- i
            while( (topInd > 1)  && (abs(peakFrameIn[(topInd-1), timeColName] - peakFrameIn[i, timeColName]) <= max.rt.drift)) {
                topInd <- topInd - 1
            }
            while( (bottomInd < dim(peakFrameIn)[[1]]) && (abs(peakFrameIn[(bottomInd+1), timeColName] - peakFrameIn[i, timeColName]) <= max.rt.drift)) {
                bottomInd <- bottomInd + 1
            }

            #subFrame <- peakFrameIn[topInd:bottomInd,]
            for(j in topInd:bottomInd){
                if(j != i){

                    peak2 <- peakFrameIn[j,]
                    peak2.mz <- peak2[[mzColName]]
                    peak2.rt <- peak2[[timeColName]]
                    peak2.id <- peak2[[idColName]]

                    # Only do this loop if the peak hasn't been marked as a fragment.
                    if(!(ref.type[[i]] %in% names(to.elim.1))){
                        for(elim in names(massShiftList1)){

                            dm.low <- massShiftList1[[elim]][[1]][["min"]]
                            dm.high <- massShiftList1[[elim]][[1]][["max"]]

                            if(massShiftList1[[elim]][[2]] == "+"){
                                if(peak1.id < peak2.id){
                                    if((peak2.mz >= (peak1.mz + dm.low)) && (peak2.mz <= (peak1.mz + dm.high)) ){
                                        assign("testCount1", (get("testCount1",envir=countEnv)+1), envir=countEnv)
                                        ref.type[[j]] <- elim
                                        ref.dir[[j]] <- "higher"
                                        ref.parent[[j]] <- peak1.id
                                    }
                                }
                            }

                            if(massShiftList1[[elim]][[2]] == "-"){
                                wtf1 <<- list(p1=peak1, p2=peak2, lo=dm.low, hi=dm.high, topInd=topInd, bottomInd=bottomInd)
                                if(peak1.id < peak2.id){
                                    if((peak2.mz <= (peak1.mz - dm.low)) && (peak2.mz >= (peak1.mz - dm.high)) ){
                                        assign("testCount1", (get("testCount1",envir=countEnv)+1), envir=countEnv)
                                        ref.type[[j]] <- elim
                                        ref.dir[[j]] <- "lower"
                                        ref.parent[[j]] <- peak1.id
                                    }
                                }
                            }
                        }
                    }

                    for(elim in names(massShiftList2)){

                        dm.low <- massShiftList2[[elim]][[1]][["min"]]
                        dm.high <- massShiftList2[[elim]][[1]][["max"]]

                        if(massShiftList2[[elim]][[2]] == "+"){
                            if(peak1.id < peak2.id){
                                if((peak2.mz >= (peak1.mz + dm.low)) && (peak2.mz <= (peak1.mz + dm.high)) ){
                                    assign("testCount1", (get("testCount1",envir=countEnv)+1), envir=countEnv)
                                    ref.type[[j]] <- elim
                                    ref.dir[[j]] <- "higher"
                                    ref.parent[[j]] <- peak1.id
                                }
                            }
                        }
                        if(massShiftList2[[elim]][[2]] == "-"){
                            wtf2 <<- list(p1=peak1, p2=peak2, lo=dm.low, hi=dm.high, topInd=topInd, bottomInd=bottomInd)
                            if(peak1.id < peak2.id){
                                if((peak2.mz <= (peak1.mz - dm.low)) && (peak2.mz >= (peak1.mz - dm.high)) ){
                                    assign("testCount1", (get("testCount1",envir=countEnv)+1), envir=countEnv)
                                    ref.type[[j]] <- elim
                                    ref.dir[[j]] <- "lower"
                                    ref.parent[[j]] <- peak1.id
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return(list(sortedFrame=peakFrameIn,
                ref.dir=ref.dir,
                ref.type=ref.type,
                ref.parent=ref.parent,
                keep=which(ref.type==""),
                discard=which(ref.type!="")))
}
 
test_negMode <- function(){

    d <- read.csv("150319_NegIon-AllSamples-hhpoolref_1.csv", header=T, stringsAsFactors=F)
    d2 <- d[,1:3]
    names(d2) <- c("id","mz","rt")
    d2$mz <- as.numeric(d2$mz)
    d2$rt <- as.numeric(d2$rt)
    print("Starting filtering...")
    t1 <- Sys.time()
    ret <- filter(d2, "mz", "rt", "id")
    t2 <- Sys.time()
    print("Finished filtering.")
    return(list(t1=t1, t2=t2, ret=ret))
}


test_sleepKAB <- function(){

    d <- read.csv("~/160817-Sleep-KAB_NegIon_export_1.csv", header=T, stringsAsFactors=F)
    d2 <- d[,1:3]
    names(d2) <- c("id","mz","rt")
    d2$mz <- as.numeric(d2$mz)
    d2$rt <- as.numeric(d2$rt)
    print("Starting filtering...")
    t1 <- Sys.time()
    ret <- filter(d2, "mz", "rt", "id")
    t2 <- Sys.time()
    print("Finished filtering.")
    return(list(t1=t1, t2=t2, ret=ret))

}






