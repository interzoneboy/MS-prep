library(magrittr)

#' col_norm_to_frame normalizes all the frame-columns by one of them.
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
col_norm_to_frame <- function(dd, normCol, rowInds=NULL, colInds=NULL){

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

#' col_norm_to_median normalizes all the frame-columns by the median of a number of them..
#'
#' @param dd input data frame to normalize
#' @param normCols A vector of indices that pick columns. Medians are calculated from these in row-wise manner, and
#'      they are used to normalize. May be a function like 'span' 
#' @param inds A span of indices over which to operate. These should be the data columns, and this is
#'    just so that you can have other ignored columns be in there. May be a function like 'span'.
#' @return returns the input data frame, but with the data cols normalized.
#' @export
col_norm_to_median <- function(dd, normCols, rowInds=NULL, colInds=NULL){

    if(is.null(normCols)){
        norm_stat_pre <- dd
    }else if(is.function(normCols)){
        norm_stat_pre <- dd[,normCols(dd)]
    }else{
        norm_stat_pre <- dd[,normCols]
    }
    norm_stat <- as.vector(apply(norm_stat_pre,1,function(x){median(x, na.rm=T)}))

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


#' col_norm_to_sum normalizes all the frame-columns by the sum of a number of them..
#'
#' @param dd input data frame to normalize
#' @param normCols A vector of indices that pick columns. Sums (for normalization) are calculated from these in row-wise manner, and
#'    they are used to normalize. May be a function like 'span'.
#' @param inds A span of indices over which to operate. Only these columns will have their entries normalized. Useful for ignoring
#'    id columns etc. May be a function like 'span'.
#' @return returns the input data frame, but with the data cols normalized.
#' @export
col_norm_to_sum <- function(dd, normCols, rowInds=NULL, colInds=NULL){

    if(is.null(normCols)){
        norm_stat_pre <- dd
    }else if(is.function(normCols)){
        norm_stat_pre <- dd[,normCols(dd)]
    }else{
        norm_stat_pre <- dd[,normCols]
    }
    norm_stat <- as.vector(apply(norm_stat_pre,1,function(x){sum(x, na.rm=T)}))

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

#' col_norm_null
#'
#' col_norm_null doesn't do any normalizing, but represents. Use this if you need a default normalizing arg
#' that does nothing.
#' @param dd input data frame
#' @param normCols dummy variable for column to normalize with. This whole function does nothing.
#' @param rowInds dummy variable
#' @param colInds dummy variable
#' @return returns the input data frame, exactly the way it was.
#' @export
col_norm_null <- function(dd, normCols, rowInds=NULL, colInds=NULL){
    return(dd)
}
