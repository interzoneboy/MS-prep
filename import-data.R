library(reshape2)
library(plyr)
library(stringr)

#
#
#

p <- function(f){
    w <- function(...){
        w2 <- function(d){
            f(d, ...)
        }
        return(w2)
    }
    return(w)
}

import_data <- function(fname){

    d <- read_data_file(fname)

}


# Assuming d_in is a data frame with column 1 as sample id, other columns as variable values.
# names(d_in) must be ["sample_id", "varName1", "varName2", ..., "varNameN"]
normalize <- function(d_in, body=NULL){

}


# Assuming d_in is a data frame with column 1 as sample id, other columns as variable values 
# names(d_in) must be ["sample_id", "varName1", "varName2", ..., "varNameN"]
find_id_cols <- function(d_in, body=NULL){

}

# Assuming d_in is a data frame with column 1 as sample id, other columns as variable values 
# names(d_in) must be ["sample_id", "varName1", "varName2", ..., "varNameN"]
find_measure_cols <- function(d_in, body=NULL){


}

# Function reads in a csv file, and prepares it so that it has a proper header line
# (1st line), and a consistent body afterwards, with no trailing empty rows or columns.
read_data_file <- function(fname, body=NULL){

    if (is.null(body)){
        d <- read.csv(fname, header=T, stringsAsFactors=F)
    }else{
        d <- body(fname)
    }
    return(d)
}

#' Function removes columns from input data frame d, returning a data frame without them
#' @param d Input data frame needing columns removed
#' @param func
#' @param inds A vector of indexes to consider for removal. Useful for guaranteeing that id info won't be removed etc.
#' @return The input data frame \code{d} but missing the unwanted columns
remove_cols <- function(d, funcList, inds=NULL){

    #bad.cols <- grep(name_frag, names(d), ignore.case=T)
    if (is.null(inds)){
        all_inds <- 1:ncol(d)
    }else{
        all_inds <- inds
    }
    ret_inds <- intersect(all_inds, Reduce(function(x,y){
        newInds <- y(d)
        return(union(newInds, x))
    }, funcList, init=c()))
    if (length(ret_inds) > 0){
        d.out <- d[,-ret_inds]
    }else{
        d.out <- d
    }
    return(d.out)
    #return(ret_inds)
}

#' Function removes rows from input data frame d, returning a data frame without them
#' @param d Input data frame needing rows removed
#' @param func
#' @param inds A vector of indexes to consider for removal. Useful for guaranteeing that id info won't be removed etc.
#' @return The input data frame \code{d} but missing the unwanted columns
remove_rows <- function(d, funcList, inds=NULL){

    if (is.null(inds)){
        all_inds <- 1:nrow(d)
    }else{
        all_inds <- inds
    }
    ret_inds <- intersect(all_inds, Reduce(function(x,y){
                                           newInds <- y(d)
                                           return(union(newInds, x))
    }, funcList, init=c()))
    if (length(ret_inds) > 0){
        d.out <- d[-ret_inds,]
    }else{
        d.out <- d
    }
    return(d.out)

}

#' Function that somehow generates a new column called "new_col_name" from the old column, called
#' "col_name". splitFunc is a function that accepts a vector, and returns a new vector. This function
#' is intended to do things like split sample names from the default values that are multipart and
#' full of "_"s.
#' @param d Input data frame needing new_col_name from col_name
#' @param col_name The name of the column to use in generating the new one.
#' @param new_col_name What are we going to name the new column.
#' @param splitFunc Takes a single vector, returns a single vector, and contains the column creation logic.
generate_col <- function(d, col_name, new_col_name, splitFunc){
    print(paste("blaaaaah", dim(d)))
    print(names(d))
    d.out <- d
    d.out[[new_col_name]] <- splitFunc(d[[col_name]])
    return(d.out)
} 



#' Function that uniformly applies a change to a column. Intended to do things like 
#' converting character columns to numeric ones.
#' @param d Input data frame needing column alteration.
#' @param col_name The name of the column to alter.
#' @param alterFunc Takes a single vector, returns a single vector that is an altered copy.
alter_col <- function(d, col_name, alterFunc){
    d.out <- d
    d.out[[col_name]] <- alterFunc(d.out[[col_name]])
    return(d.out)
} 



#' Function transposes the dataframe. You tell it which columns contain row id information, and
#' which contain row measurement information, so that it can put together the transposed dataset correctly.
#' The "names" of the data frame become the row.names after the transpose. This is made into an additional column
#' called sample_id.
#' @param d Input data frame
#' @param id_cols numeric indices of columns containing row id information
#' @param measured_cols numeric indices of columns containing measurement information
#' @param name_cols unary function that takes the sub-matrix of (formerly row, after transpose column) information, and creates a vector
#'        to be used for sample names out of it.
#' @return transposed data frame, with the sample_ids in the first colmns (used to be original data frame names), and the frame/variable
#'         id information in the names(...) attribute.
transpose <- function(d, id_cols, measured_cols, id_col_name, name_cols=NULL ){
    d_trans <- data.frame(t(d), stringsAsFactors=F)
    d_trans <- data.frame(id=row.names(d_trans), d_trans, stringsAsFactors=F)
    d_nameThings <- d_trans[1:(length(id_cols)),]
    d_out <- d_trans[(length(id_cols)+1):nrow(d_trans),]
    if (!is.null(name_cols)){
        names(d_out) <- name_cols(d_nameThings)
    }else{
        names(d_out) <- c(id_col_name, paste0("frame_", str_trim(d_nameThings[1,2:length(d_nameThings)])))
    }
    row.names(d_out) <- NULL
    return(d_out)
}


transpose_filter_normalize <- function(d_in, id_cols, measure_cols,
                                       id_formatter=NULL,
                                       col_filters=NULL,
                                       row_filters=NULL,
                                       normalizers=NULL) {

    # Take only the column indexes of interest, that either id the row, or id the sample.
    #
    d_sieve_mz_info <- d_in[,id_cols]
    d_pre <- d_in[,c(id_cols, measure_cols)]
    d_trans <- data.frame(t(d_pre), stringsAsFactors=F)
    d_trans <- data.frame(id=row.names(d_trans), d_trans, stringsAsFactors=F)

    d <- d_trans[(length(id_cols)+1):nrow(d_trans),]
    if (!is.null(id_formatter)){
        d$id <- id_formatter(d$id)
    }

    data_cols <- data.frame(lapply(d[,2:ncol(d)], as.numeric))

    # Now filter stuff --------------------------------------
    #
    #   Filter the data_cols, using the list of col filtering functions provided as argument.
    if (!is.null(col_filters)){
        data_cols <- Reduce(function(x, f){return(f(x))}, col_filters, init=data_cols)
    }
    d <- data.frame(id=d$id, data_cols, stringsAsFactors=F)
    if (!is.null(row_filters)){
        d <- Reduce(function(x,f){f(x)}, row_filters, init=d)
    }

    # Now do normalizing stuff ------------------------------
    #
    #   normalizing...
    if (!is.null(normalizers)){
        d <- Reduce(function(x,f){f(x)}, normalizers, init=d)
    }

    # Process sample ID if necessary (for matching to aux sample info)
    #
    ret <- data.frame(id=d$id, data_cols_out, stringsAsFactors=F)
    return(ret)

}
