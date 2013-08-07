td.rank <- function(tdf, col) {
    if (!is.td.data.frame(tdf)) 
        stop("'tdf' is not a td data frame")
    if (!length(names(tdf))) 
        stop("td.data.frame contains no columns")
    if (missing(col) && length(tdf) == 1L) 
        col <- names(tdf)[1]
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    query <- .td.genrank(tdf, col)
    df <- try(tdQuery(query))
    if (is.null(class(df))) 
        stop("Error retrieving rank values from table.")
    return(df)
}
 
