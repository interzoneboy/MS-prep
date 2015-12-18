library(reshape2)
library(plyr)
library(funprog)

#
#
#

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
