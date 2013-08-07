min.td.data.frame <- function(tdf, ...) {
    if (!is.td.data.frame(tdf)) 
        stop("'tdf' is not a td data frame")
    if (!length(names(tdf))) 
        stop("td.data.frame contains no columns")
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    exprs <- attr(tdf, "expressions")
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste("WHERE", attr(tdf, "whereClause"))
    
    cols <- character(0)
    for (i in 1:length(names(tdf))) {
        if (as.character(i) %in% names(exprs)) 
            col <- gettextf("MIN(%s) AS xmin", .td.gencolumnexpr(tdf[i])) else col <- .td.genstats(names(tdf)[i], "min")
        
        cols[i] <- paste(col, as.character(i), sep = "")
    }
    
    query <- gettextf("SELECT %s FROM %s %s", paste(cols, collapse = ","), obj, wc)
    df <- try(tdQuery(query))
    if (is.null(attr(df, "class"))) 
        stop("Only defined on a td data frame with all numeric variables")
    return(min(df))
}
 
