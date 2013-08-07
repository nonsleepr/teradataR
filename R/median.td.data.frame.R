median.td.data.frame <- function(x) {
    if (!is.td.data.frame(x)) 
        stop("'x' is not a td data frame")
    if (length(x) != 1) 
        stop("median can only be run against a td data frame of one column.")
    
    query <- paste(.td.genmedian(x, names(x)))
    df <- try(tdQuery(query))
    return(as.numeric(df))
}
 
