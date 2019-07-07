library(magrittr)

#' row_norm_to_vector normalizes all the frame-rows by a supplied vector
#'
#' Pre-compute some other vector by which to normalize (like from a different but identically dimensioned matrix),
#' and use it to normalize the rows  of the input matrix dd.
#' 
#' @param dd input data frame to normalize
#' @param normVector This is a vector that will properly normalize the rows of this data frame, BEFORE
#'        any non-processed rows are removed from it. (i.e. it has to have exactly the same dimension as
#'        the pre-processed data frame it's going to normalize. The length of the columns (dim(...)[[1]]))
#' @param rowInds A span of row indices over which to operate. These should be the data rows, and this is
#'    just so that you can have other ignored rows be in there. This can be NULL, in which case all rows
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @param colInds A span of column indices over which to operate. These should be the data columns, and this is
#'    just so that you can have other ignored columns be in there. This can be NULL, in which case all columns
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @return return the input data frame, but with the specified data rows normalized by the input column vector.
#' @export
row_norm_to_vector <- function(dd, normVector, rowInds=NULL, colInds=NULL){

    norm_stat <- unlist(normVector)

    if(is.null(colInds)){
        all_col_inds <- 1:ncol(dd)
    }else if(is.function(colInds)){
        all_col_inds <- colInds(dd)
    }else{
        all_col_inds <- colInds
    }
    data_mat <- dd[,all_col_inds]

    if(is.null(rowInds)){
        all_row_inds <- 1:nrow(dd)
    }else if(is.function(rowInds)){
        all_row_inds <- rowInds(dd)
    }else{
        all_row_inds <- rowInds
    }

    data_mat_2 <- data_mat[all_row_inds,]
    norm_stat_2 <- norm_stat[all_row_inds]


    normed_mat <- sweep(data_mat_2, 1, norm_stat_2, FUN="/")
    dd_out <- dd
    dd_out[all_row_inds, all_col_inds] <- normed_mat
    return(dd_out)
}


#' row_norm_to_frame normalizes all the frame-rows by one of them.
#'
#' Set this to the first frame to normalize by the most intense frame, or set this to
#' the internal standard frame, in cases where the frames are in columns.
#'
#' @param dd input data frame to normalize
#' @param normCol either a string column name, or a numeric column index
#' @param rowInds A span of row indices over which to operate. These should be the data rows, and this is
#'    just so that you can have other ignored rows be in there. This can be NULL, in which case all rows
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @param colInds A span of column indices over which to operate. These should be the data columns, and this is
#'    just so that you can have other ignored columns be in there. This can be NULL, in which case all columns
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @return returns the input data frame, but with the data cols normalized.
#' @export
row_norm_to_frame <- function(dd, normCol, rowInds=NULL, colInds=NULL){

    norm_stat <- unlist(dd[,normCol])

    if(is.null(colInds)){
        all_col_inds <- 1:ncol(dd)
        data_mat <- dd
    }else if(is.function(colInds)){
        all_col_inds <- colInds(dd)
        data_mat <- dd[,all_col_inds]
    }else{
        all_col_inds <- colInds
        data_mat <- dd[,all_col_inds]
    }

    if(is.null(rowInds)){
        all_row_inds <- 1:nrow(dd)
        data_mat_2 <- data_mat
        norm_stat_2 <- norm_stat
    }else if(is.function(rowInds)){
        all_row_inds <- rowInds(dd)
        data_mat_2 <- data_mat[all_row_inds,]
        norm_stat_2 <- norm_stat[all_row_inds]
    }else{
        all_row_inds <- rowInds
        data_mat_2 <- data_mat[all_row_inds,]
        norm_stat_2 <- norm_stat[all_row_inds]
    }

    normed_mat <- sweep(data_mat_2, 1, norm_stat_2, FUN="/")
    dd_out <- dd
    dd_out[all_row_inds, all_col_inds] <- normed_mat
    return(dd_out)
}


#' row_norm_to_median normalizes all the frame-rows by the median of a number of them..
#'
#' @param dd input data frame to normalize
#' @param normCols A vector of indices that pick columns. Medians are calculated from these in row-wise manner, and
#'      they are used to normalize each row. May be a function like 'span' 
#' @param rowInds A span of row indices over which to operate. These should be the data rows, and this is
#'    just so that you can have other ignored rows be in there. This can be NULL, in which case all rows
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @param colInds A span of column indices over which to operate. These should be the data columns, and this is
#'    just so that you can have other ignored columns be in there. This can be NULL, in which case all columns
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @return returns the input data frame, but with the data cols normalized.
#' @export
row_norm_to_median <- function(dd, normCols, rowInds=NULL, colInds=NULL){


    if(is.null(normCols)){
        norm_stat_pre <- dd
    }else if(is.function(normCols)){
        norm_stat_pre <- dd[,normCols(dd)]
    }else{
        norm_stat_pre <- dd[,normCols]
    }
    norm_stat <- as.vector(apply(norm_stat_pre,1,function(x){median(as.numeric(x), na.rm=T)}))

    if(is.null(colInds)){
        all_col_inds <- 1:ncol(dd)
        data_mat <- dd
    }else if(is.function(colInds)){
        all_col_inds <- colInds(dd)
        data_mat <- dd[,all_col_inds]
    }else{
        all_col_inds <- colInds
        data_mat <- dd[,all_col_inds]
    }


    if(is.null(rowInds)){
        all_row_inds <- 1:nrow(dd)
        data_mat_2 <- data_mat
        norm_stat_2 <- norm_stat
    }else if(is.function(rowInds)){
        all_row_inds <- rowInds(dd)
        data_mat_2 <- data_mat[all_row_inds,]
        norm_stat_2 <- norm_stat[all_row_inds]
    }else{
        all_row_inds <- rowInds
        data_mat_2 <- data_mat[all_row_inds,]
        norm_stat_2 <- norm_stat[all_row_inds]
    }


    normed_mat <- sweep(data_mat_2, 1, norm_stat_2, FUN="/")
    dd_out <- dd
    dd_out[all_row_inds,all_col_inds] <- normed_mat
    return(dd_out)
}


#' row_norm_to_sum normalizes all the frame-rows by the sum of a number of them..
#'
#' @param dd input data frame to normalize
#' @param normCols A vector of indices that pick columns. Sums (for normalization) are calculated from these in row-wise manner, and
#'    they are used to normalize. May be a function like 'span'.
#' @param rowInds A span of row indices over which to operate. These should be the data rows, and this is
#'    just so that you can have other ignored rows be in there. This can be NULL, in which case all rows
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @param colInds A span of column indices over which to operate. These should be the data columns, and this is
#'    just so that you can have other ignored columns be in there. This can be NULL, in which case all columns
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @return returns the input data frame, but with the data cols normalized.
#' @export
row_norm_to_sum <- function(dd, normCols, rowInds=NULL, colInds=NULL){

    if(is.null(normCols)){
        norm_stat_pre <- dd
    }else if(is.function(normCols)){
        norm_stat_pre <- dd[,normCols(dd)]
    }else{
        norm_stat_pre <- dd[,normCols]
    }
    norm_stat <- as.vector(apply(norm_stat_pre,1,function(x){sum(as.numeric(x), na.rm=T)}))

    if(is.null(colInds)){
        all_col_inds <- 1:ncol(dd)
        data_mat <- dd
    }else if(is.function(colInds)){
        all_col_inds <- colInds(dd)
        data_mat <- dd[,all_col_inds]
    }else{
        all_col_inds <- colInds
        data_mat <- dd[,all_col_inds]
    }

    if(is.null(rowInds)){
        all_row_inds <- 1:nrow(dd)
        data_mat_2 <- data_mat
        norm_stat_2 <- norm_stat
    }else if(is.function(rowInds)){
        all_row_inds <- rowInds(dd)
        data_mat_2 <- data_mat[all_row_inds,]
        norm_stat_2 <- norm_stat[all_row_inds]
    }else{
        all_row_inds <- rowInds
        data_mat_2 <- data_mat[all_row_inds,]
        norm_stat_2 <- norm_stat[all_row_inds]
    }

    normed_mat <- sweep(data_mat_2, 1, norm_stat_2, FUN="/")
    dd_out <- dd
    dd_out[all_row_inds,all_col_inds] <- normed_mat
    return(dd_out)
}

#' row_norm_null
#'
#' row_norm_null doesn't do any normalizing, but represents. Use this if you need a default normalizing arg
#' that does nothing.
#' @param dd input data frame
#' @param normCols dummy variable for column to normalize with. This whole function does nothing.
#' @param rowInds dummy variable
#' @param colInds dummy variable
#' @return returns the input data frame, exactly the way it was.
#' @export
row_norm_null <- function(dd, normCols, rowInds=NULL, colInds=NULL){
    return(dd)
}
