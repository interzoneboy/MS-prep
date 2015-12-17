library(reshape2)
library(plyr)

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

import_data <- function(d_in, id_cols, measure_cols,
                                col_filters=NULL,
                                row_filters=NULL) {
    #names_and_mz <- d[,id_cols]
    #data <- d[,measure_cols]

    # Take only the column indexes of interest, that either id the row, or id the sample.
    #
    d_pre <- d_in[,c(id_cols, measure_cols)]
    d_trans <- data.frame(t(d_pre), stringsAsFactors=F)
    d_trans <- data.frame(id=row.names(d_trans), d_trans, stringsAsFactors=F)

    d <- d_trans[(length(id_cols)+1):nrow(d_trans),]

    data_cols <- data.frame(lapply(d[,2:ncol(d)], as.numeric))

    # Now filter stuff --------------------------------------
    #
    #   filtering...



    # Now do normalizing stuff ------------------------------
    #
    #   normalizing...


    # Process sample ID if necessary (for matching to aux sample info)
    #

    ret <- data.frame(id=d$id, data_cols_out, stringsAsFactors=F)
    return(ret)

}
