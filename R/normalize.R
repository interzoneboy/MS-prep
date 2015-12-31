library(magrittr)

#' norm_to_frame normalizes all the frame-columns by one of them.
#'
#' Set this to the first frame to normalize by the most intense frame, or set this to
#' the internal standard frame.
#'
#' @param dd input data frame to normalize
#' @param normCol either a string column name, or a numeric column index
#' @param inds A span of indices over which to operate. These should be the data columns, and this is
#'    just so that you can have other ignored columns be in there. This can be NULL, in which case all columns
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @return returns the input data frame, but with the data cols normalized.
#' @export
norm_to_frame <- function(dd, normCol, inds=NULL){

    norm_stat <- unlist(dd[,normCol])

    if(is.null(inds)){
        all_inds <- 1:ncol(dd)
        data_mat <- dd
    }else if(is.function(inds)){
        all_inds <- inds(dd)
        data_mat <- dd[,all_inds]
    }else{
        all_inds <- inds
        data_mat <- dd[,all_inds]
    }

    normed_mat <- sweep(data_mat, 2, norm_stat, FUN="/")
    dd_out <- dd
    dd_out[,all_inds] <- normed_mat
    return(dd_out)
}

#' norm_to_median normalizes all the frame-columns by the median of a number of them..
#'
#' @param dd input data frame to normalize
#' @param normCols A vector of indices that pick columns. Medians are calculated from these in row-wise manner, and
#'      they are used to normalize.    
#' @param inds A span of indices over which to operate. These should be the data columns, and this is
#'    just so that you can have other ignored columns be in there. 
#' @return returns the input data frame, but with the data cols normalized.
#' @export
norm_to_median <- function(dd, normCols, inds=NULL){

    norm_stat_pre <- dd[,normCols]
    norm_stat <- as.vector(apply(norm_stat_pre,1,function(x){median(x, na.rm=T)}))

    if(is.null(inds)){
        all_inds <- 1:ncol(dd)
        data_mat <- dd
    }else if(is.function(inds)){
        all_inds <- inds(dd)
        data_mat <- dd[,all_inds]
    }else{
        all_inds <- inds
        data_mat <- dd[,all_inds]
    }

    normed_mat <- sweep(data_mat, 2, norm_stat, FUN="/")
    dd_out <- dd
    dd_out[,all_inds] <- normed_mat
    return(dd_out)
}

#' norm_null
#'
#' norm_null doesn't do any normalizing, but represents. Use this if you need a default normalizing arg
#' that does nothing.
#' @param dd input data frame
#' @param normCols dummy variable for column to normalize with. This whole function does nothing.
#' @param inds dummy variable
#' @return returns the input data frame, exactly the way it was.
#' @export
norm_null <- function(dd, normCols, inds=NULL){
    return(dd)
}
