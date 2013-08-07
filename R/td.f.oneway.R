td.f.oneway <- function(tdf, col1, col2, thresh = 0.05) {
    if (!is.td.data.frame(tdf)) 
        stop("'tdf' is not a td data frame")
    if (missing(col1) || !nchar(col1)) 
        stop("'col1' not specified.")
    if (missing(col2) || !length(col2)) 
        stop("'col2' not specified.")
    if (thresh <= 0 || thresh >= 1) 
        stop("'thresh' value must be greater than 0 and less than 1.")
    query <- .td.genfway(tdf, col1, col2, thresh)
    df <- tdQuery(query)
    return(df)
}
 
