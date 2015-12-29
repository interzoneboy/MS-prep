library(reshape2)
library(plyr)
library(stringr)

#' print_dim
#'
#' print_dim simply prints the dim of input matrix d using "print"
#' @param d The input data frame for dim printing.
#' @param tagStr A string to provide some context
#' @return Prints to the screen... returns the data frame d for further pipelining.
#' @export
print_dim <- function(d, tagStr){
    print(paste0(tagStr," -- ", paste(dim(d), collapse=",")))
    return(d)

}


#' P is the pipelining function of this module. It decorates functions such that their only argument
#' becomes the data frame in the pipeline.
#' Intended for use with magrittr %>% pipelines.
#'
#' @export
p <- function(f){
    w <- function(...){
        w2 <- function(d){
            f(d, ...)
        }
        return(w2)
    }
    return(w)
}


#' Function reads in a csv file, and prepares it so that it has a proper header line
#' (1st line), and a consistent body afterwards, with no trailing empty rows or columns.
#' @param fname This is the filename to read in
#' @param body Need something other than the read.csv line described below? Pass it in here.
#' @return a data frame, with something sensical in names(...)
#' @export
read_data_file <- function(fname, body=NULL){

    if (is.null(body)){
        d <- read.csv(fname, header=T, stringsAsFactors=F)
    }else{
        d <- body(fname)
    }
    return(d)
}


#' Function that describes a range in a given data frame context. Intended for use within a
#' pipeline, where there's no explicit data frame to measure, like for the inds argument of
#' remove_cols and remove_rows. 
#'
#' @param type Either Is this a range of 'row' or 'col' 
#' @param fromInd Either a numeric, or 'start' or 'end'
#' @param toInd Either a numeric, or 'start' or 'end'
#' @return function(d), called with a data frame, that returns a numeric vector.
#' @export
span <- function(type, fromInd, toInd){
    inner <- function(d){
        a <- function(x, dimMeasure){
            if(is.numeric(x)){
                ff <- x
            }else if(x=="start"){
                ff <- 1
            }else if(x=="end"){
                ff <- dimMeasure(d)
            }
            return(ff)
        }
        if (type=="row"){
            ff <- a(fromInd, nrow)
            ee <- a(toInd, nrow)
            return(ff:ee)
        } else if (type=="col"){
            ff <- a(fromInd, ncol)
            ee <- a(toInd, ncol)
            return(ff:ee)
        } else {
            stop("Illegal Type")
        }
    }
    return(inner)
}
        

#' Function removes columns from input data frame d, returning a data frame without them
#' @param d Input data frame needing columns removed
#' @param funcList A list of functions that each yield column indices to be removed. Called with dataframe as input. Joined with union.
#' @param inds A vector of indexes to consider for removal. Useful for guaranteeing that id info won't be removed etc. The final list of columns to remove is intersected with this list. This can also be a function that is called with the data frame d, and returns a vector of indices (such as the 'span' function above).
#' @return The input data frame \code{d} but missing the unwanted columns
#' @export
remove_cols <- function(d, funcList, inds=NULL){

    #bad.cols <- grep(name_frag, names(d), ignore.case=T)
    if (is.null(inds)){
        all_inds <- 1:ncol(d)
    }else if(is.function(inds)){
        all_inds <- inds(d)
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
#' @param funcList a list of functions. Each takes a whole dataframe, and returns a vector of indices detailing which rows to remove.
#' @param inds A vector of indexes to consider for removal. Useful for guaranteeing that id info won't be removed etc. The final list of rows to remove is intersected with this list. This can also be a function that is called with the data frame d, and returns a vector of indices (such as the 'span' function above).
#' @return The input data frame \code{d} but missing the unwanted rows
#' @export
remove_rows <- function(d, funcList, inds=NULL){

    if (is.null(inds)){
        all_inds <- 1:nrow(d)
    }else if(is.function(inds)){
        all_inds <- inds(d)
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

#' keep_rows
#'
#' Function keeps only the specified rows from input data frame d, returning a new
#' data frame containing only these rows.
#'
#' @param d Input data frame
#' @param funcList a list of functions. Each takes a whole dataframe, and returns a vector of indices detailing which rows to keep.
#' @return The input data frame \code{d} but containing only the rows we want to keep
#' @export
keep_rows <- function(d, funcList){
 
    ret_inds <- Reduce(function(x,y){
                         newInds <- y(d)
                         return(union(newInds, x))
    }, funcList, init=c())
    if (length(ret_inds) > 0){
        d.out <- d[ret_inds,]
    }else{
        stop("Not keeping any rows at all?")
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
#' @return A copy of the input dataframe d, with the new column appended
#' @export
generate_col <- function(d, col_name, new_col_name, splitFunc){
    d.out <- d
    d.out[[new_col_name]] <- splitFunc(d[[col_name]])
    return(d.out)
} 



#' Function that uniformly applies a change to a column. Intended to do things like 
#' converting character columns to numeric ones.
#' @param d Input data frame needing column alteration.
#' @param col_name The name of the column to alter.
#' @param alterFunc Takes a single vector, returns a single vector that is an altered copy.
#' @return A copy of the input data frame d with the given column altered.
#' @export
alter_col <- function(d, col_name, alterFunc){
    d.out <- d
    d.out[[col_name]] <- alterFunc(d.out[[col_name]])
    return(d.out)
} 

#' Function that uniformly applies the same change to many columns. Intended to do things like 
#' converting character columns to numeric ones.
#' @param d Input data frame needing column alteration.
#' @param col_finder Take the whole data frame (d here) and return a vector of numeric indices of columns to alter.
#' @param alterFunc Takes a single vector, returns a single vector that is an altered copy.
#' @return A copy of the input data frame d with the given columns altered.
#' @export
alter_cols <- function(d, col_finder, alterFunc){
    d.out <- d
    col_inds <- col_finder(d.out)
    for (ci in col_inds){
        d.out[,ci] <- alterFunc(d.out[,ci])
    }
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
#' @export
transpose <- function(d, id_cols, id_col_name, name_cols=NULL ){
    d_trans <- data.frame(t(d), stringsAsFactors=F)
    d_trans <- data.frame(id=row.names(d_trans), d_trans, stringsAsFactors=F)
    d_nameThings <- d_trans[1:(length(id_cols)),]
    d_out <- d_trans[(length(id_cols)+1):nrow(d_trans),]
    if (!is.null(name_cols)){
        names(d_out) <- name_cols(d_nameThings)
    }else{
        names(d_out) <- c(id_col_name, paste0("frame_", stringr::str_trim(d_nameThings[1,2:length(d_nameThings)])))
    }
    row.names(d_out) <- NULL
    return(d_out)
}




