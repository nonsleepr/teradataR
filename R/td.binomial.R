td.binomial <- function(tdf, first, second, thresh = 0.05) {
    if (!is.td.data.frame(tdf)) 
        stop("'tdf' is not a td data frame")
    if (missing(first) || missing(second) || !nchar(first) || !nchar(second)) 
        stop("'first' and 'second are required parameters.")
    if (length(first) > 1L || length(second) > 1L) 
        warning("Multiple dimension values not allowed for 'first' or 'second'.  Using only first element.")
    if (thresh <= 0 || thresh >= 1) 
        stop("'thresh' value must be greater than 0 and less than 1.")
    query <- .td.genbinomial(tdf, first[1], second[1])
    df <- tdQuery(query)
    return(df)
}
 
