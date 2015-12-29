library(magrittr)
source("R/remCorlowIntensity.R")
source("R/filter_corrected.R")

#' vasant_filter runs Vasant's vectorized correlation filter on data frame d.
#'
#' @param d data frame (already transposed) to filter.
#' @param corr_cutoff Correlation value above which to remove the less median-intense column. DEFAULT:0.8
#' @param inds Vector of inds over which to operate
#' @return Copy of data frame d, but missing the columns (optionally specified by inds) that don't make the cutoff
#' @export
vasant_filter <- function(d, corr_cutoff=0.8, measure=median, inds=NULL){
    
    all_inds <- 1:ncol(d)
    if (is.null(inds)){
        compute_inds <- 1:ncol(d)
    }else if (is.function(inds)){
        compute_inds <- inds(d)
    }else{
        compute_inds <- inds
    }
    saved_inds <- setdiff(all_inds, compute_inds)
    d_call <- d[,compute_inds] 
    # Call Vasant's thing.
    v_inds <- remCor.lowintensity(d_call, corr_cutoff, measure)
    inds_keep <- compute_inds[v_inds]
    inds_out <- c(saved_inds, inds_keep)
    oo <- order(inds_out)
    inds_out_2 <- inds_out[oo]
    d_out <- d[,inds_out_2]
    return(d_out)
}

#' massShift_filter removes peaks that correspond to specific mass shifts.
#'
#' @param mzColName This is the name of the MZ column in the sieve data to be filtered.
#' @param timeColName This is the retention time column name in the sieve data to be filtered.
#' @param shiftList1 Primary list of mass shifts. These should correspond to things like adducts and fragments
#'      that additionally have other shifts (like isotopes), provided in shiftList2, calculated.
#' @param shiftList2 Secondary list of mass shifts. This should correspond to things like isotopes, that need to be
#'      calculated for each peak in the dataset, and also for each added fragment peak considered from shiftList1.
#' @param rt_window_width Peaks are only considered as fragments/isotopes if they're within a particular chromatographic
#'      retention time window of the parent peak. How wide is that window? Default: 0.05
#' @param mass_acc The mass accuracy of the instrument, when we're within the rt window, looking for the mass of a fragment peak,
#'      we consider everything within a +/- mass_acc window from the target value.
#' @export
massShift_filter <- function(d, mzColName="MZ", timeColName="Time", shiftList1=to.elim.1, shiftList2=to.elim.2, rt_window_width=0.05){

    # Call Rose's filter and return.
    filter(d, mzColName, timeColName, shiftList1, shiftList2, rt_window_width)
}
