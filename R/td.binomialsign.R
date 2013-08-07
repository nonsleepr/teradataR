td.binomialsign <- function(tdf, column, thresh = 0.05) {
    if (!is.td.data.frame(tdf)) 
        stop("'tdf' is not a td data frame")
    if (missing(column) || !nchar(column)) 
        stop("No 'column' specified.")
    if (thresh <= 0 || thresh >= 1) 
        stop("'thresh' value must be greater than 0 and less than 1.")
    query <- .td.genbinomialsign(tdf, column)
    df <- tdQuery(query)
    return(df)
}
 
