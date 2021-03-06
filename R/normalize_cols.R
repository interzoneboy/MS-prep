library(magrittr)

#' col_norm_to_vector normalizes all the frame-columns by a supplied vector, same length as rows.
#'
#' Pre-compute some other vector by which to normalize (like from a different by identically-dimensioned
#' matrix), and use it to normalize the columns of the input matrix.
#'
#' @param dd input data frame to normalize
#' @param normVector This is a vector that will properly normalize the columns of this data frame, BEFORE
#'        any non-processed columns are removed from it. (i.e. it has to have exactly the same dimension as
#'        the pre-processed data frame it's going to normalize.). nounaetouh
#' @param rowInds A span of rows over which to operate. These should be the data rows, and this is
#'    just so that you can have other ignored rows be in there. This can be NULL, in which case all rows
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @param colInds A span of columns over which to operate,
#'    just so that you can have other ignored columns be in there (time, mz, etc). This can be NULL, in which case all cols
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @return returns the input data frame, but with the specified data columns normalized by the input row vector.
#' @export
col_norm_to_vector <- function(dd, normVector, rowInds=NULL, colInds=NULL){

    norm_stat <- unlist(normVector)

    if(is.null(rowInds)){
        all_row_inds <- 1:nrow(dd)
        data_mat <- dd
    }else if(is.function(rowInds)){
        all_row_inds <- rowInds(dd)
        data_mat <- dd[all_row_inds,]
    }else{
        all_row_inds <- rowInds
        data_mat <- dd[all_row_inds,]
    }

    if(is.null(colInds)){
        all_col_inds <- 1:ncol(dd)
        data_mat_2 <- data_mat
        norm_stat_2 <- norm_stat
    }else if(is.function(colInds)){
        all_col_inds <- colInds(dd)
        data_mat_2 <- data_mat[,all_col_inds]
        norm_stat_2 <- norm_stat[all_col_inds]
    }else{
        all_col_inds <- colInds
        data_mat_2 <- data_mat[,all_col_inds]
        norm_stat_2 <- norm_stat[all_col_inds]
    }


    normed_mat <- sweep(data_mat_2, 2, norm_stat_2, FUN="/")
    dd_out <- dd
    dd_out[all_row_inds,all_col_inds] <- normed_mat
    return(dd_out)
}




#' col_norm_to_frame normalizes all the frame-columns by one of them.
#'
#' Set this to the first frame to normalize by the most intense frame, or set this to
#' the internal standard frame.
#'
#' @param dd input data frame to normalize
#' @param normRow numeric row index indicating the row of the frame we normalize to.
#' @param rowInds A span of rows over which to operate. These should be the data rows, and this is
#'    just so that you can have other ignored rows be in there. This can be NULL, in which case all rows
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @param colInds A span of columns over which to operate,
#'    just so that you can have other ignored columns be in there (time, mz, etc). This can be NULL, in which case all cols
#'      are normalized, or it can be a vector of integer indices, or it can be a function that gets called with dd
#'      and which returns a vector of ints (see function 'span' in this package)
#' @return returns the input data frame, but with the specified data columns each normalized by the corresponding
#'         entry in the normRow row.
#' @export
col_norm_to_frame <- function(dd, normRow, rowInds=NULL, colInds=NULL){

    norm_stat <- unlist(dd[normRow,])

    if(is.null(rowInds)){
        all_row_inds <- 1:nrow(dd)
        data_mat <- dd
    }else if(is.function(rowInds)){
        all_row_inds <- rowInds(dd)
        data_mat <- dd[all_row_inds,]
    }else{
        all_row_inds <- rowInds
        data_mat <- dd[all_row_inds,]
    }

    if(is.null(colInds)){
        all_col_inds <- 1:ncol(dd)
        data_mat_2 <- data_mat
        norm_stat_2 <- norm_stat
    }else if(is.function(colInds)){
        all_col_inds <- colInds(dd)
        data_mat_2 <- data_mat[,all_col_inds]
        norm_stat_2 <- norm_stat[all_col_inds]
    }else{
        all_col_inds <- colInds
        data_mat_2 <- data_mat[,all_col_inds]
        norm_stat_2 <- norm_stat[all_col_inds]
    }


    normed_mat <- sweep(data_mat_2, 2, norm_stat_2, FUN="/")
    dd_out <- dd
    dd_out[all_row_inds,all_col_inds] <- normed_mat
    return(dd_out)
}

#' col_norm_to_median normalizes all the frame-columns by the median of a number of them..
#'
#' @param dd input data frame to normalize
#' @param normRows A vector of indices that pick rows. Medians are calculated from these in column-wise manner, and
#'      they are used to normalize the corresponding column. May be a function like 'span' 
#' @param rowInds A span of row indices over which to operate. These should be the data rows, and this is
#'    just so that you can have other ignored rows be in there. May be a function like 'span'.
#' @param colInds A span of indices over which to operate. These should be the data columns, and this is
#'    just so that you can have other ignored columns be in there. May be a function like 'span'.
#' @return returns the input data frame, but with the specified data normalized.
#' @export
col_norm_to_median <- function(dd, normRows, rowInds=NULL, colInds=NULL){

    if(is.null(normRows)){
        norm_stat_pre <- dd
    }else if(is.function(normRows)){
        norm_stat_pre <- dd[normRows(dd),]
    }else{
        norm_stat_pre <- dd[normRows,]
    }
    norm_stat <- as.vector(apply(norm_stat_pre,2,function(x){median(as.numeric(x), na.rm=T)}))

    if(is.null(rowInds)){
        all_row_inds <- 1:nrow(dd)
        data_mat <- dd
    }else if(is.function(rowInds)){
        all_row_inds <- rowInds(dd)
        data_mat <- dd[all_row_inds,]
    }else{
        all_row_inds <- inds
        data_mat <- dd[all_row_inds,]
    }


    if(is.null(colInds)){
        all_col_inds <- 1:ncol(dd)
        data_mat_2 <- data_mat
        norm_stat_2 <- norm_stat
    }else if(is.function(colInds)){
        all_col_inds <- colInds(dd)
        data_mat_2 <- data_mat[,all_col_inds]
        norm_stat_2 <- norm_stat[all_col_inds]
    }else{
        all_col_inds <- colInds
        data_mat_2 <- data_mat[,all_col_inds]
        norm_stat_2 <- norm_stat[all_col_inds]
    }


    normed_mat <- sweep(data_mat_2, 2, norm_stat_2, FUN="/")
    dd_out <- dd
    dd_out[all_row_inds, all_col_inds] <- normed_mat
    return(dd_out)
}

#' col_norm_to_sum normalizes all the frame-columns by the sum of a number of them..
#'
#' @param dd input data frame to normalize
#' @param normRows A vector of indices that pick rows. Sums are calculated from these in column-wise manner, and
#'      they are used to normalize the columns. May be a function like 'span' 
#' @param rowInds A span of row indices over which to operate. These should be the data rows, and this is
#'    just so that you can have other ignored rows be in there. May be a function like 'span'.
#' @param colInds A span of indices over which to operate. These should be the data columns, and this is
#'    just so that you can have other ignored columns be in there. May be a function like 'span'.
#' @return returns the input data frame, but with the specified data normalized.
#' @export
col_norm_to_sum <- function(dd, normRows, rowInds=NULL, colInds=NULL){

    if(is.null(normRows)){
        norm_stat_pre <- dd
    }else if(is.function(normRows)){
        norm_stat_pre <- dd[normRows(dd),]
    }else{
        norm_stat_pre <- dd[normRows,]
    }
    norm_stat <- as.vector(apply(norm_stat_pre,2,function(x){sum(as.numeric(x), na.rm=T)}))

    if(is.null(rowInds)){
        all_row_inds <- 1:nrow(dd)
        data_mat <- dd
    }else if(is.function(rowInds)){
        all_row_inds <- rowInds(dd)
        data_mat <- dd[all_row_inds,]
    }else{
        all_row_inds <- rowInds
        data_mat <- dd[all_row_inds,]
    }

    if(is.null(colInds)){
        all_col_inds <- 1:ncol(dd)
        data_mat_2 <- data_mat
        norm_stat_2 <- norm_stat
    }else if(is.function(colInds)){
        all_col_inds <- colInds(dd)
        data_mat_2 <- data_mat[,all_col_inds]
        norm_stat_2 <- norm_stat[all_col_inds]
    }else{
        all_col_inds <- colInds
        data_mat_2 <- data_mat[,all_col_inds]
        norm_stat_2 <- norm_stat[all_col_inds]
    }

    normed_mat <- sweep(data_mat_2, 2, norm_stat_2, FUN="/")
    dd_out <- dd
    dd_out[all_row_inds, all_col_inds] <- normed_mat
    return(dd_out)
}


#' col_norm_null
#'
#' norm_null doesn't do any normalizing, but represents. Use this if you need a default normalizing arg
#' that does nothing.
#' @param dd input data frame
#' @param normRows dummy variable for column to normalize with. This whole function does nothing.
#' @param rowInds dummy variable
#' @param colInds dummy variable
#' @return returns the input data frame, exactly the way it was.
#' @export
col_norm_null <- function(dd, normRows, rowInds=NULL, colInds=NULL){
    return(dd)
}
