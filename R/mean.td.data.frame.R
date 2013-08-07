mean.td.data.frame <- function(x, ...) {
    if (!is.td.data.frame(x)) 
        stop("'x' is not a td data frame")
    if (!length(names(x))) 
        stop("td.data.frame contains no columns")
    obj <- .td.object(attr(x, "tableName"), attr(x, "database"))
    meanVal <- double(0)
    exprs <- attr(x, "expressions")
    
    wc <- ""
    if (!is.null(attr(x, "whereClause"))) 
        wc <- paste("WHERE", attr(x, "whereClause"))
    
    for (i in 1:length(names(x))) {
        if (as.character(i) %in% names(exprs)) 
            col <- gettextf("AVG(%s) AS xmean", .td.gencolumnexpr(x[i])) else col <- .td.genstats(names(x)[i], "mean")
        
        query <- gettextf("SELECT %s FROM %s %s", col, obj, wc)
        df <- try(tdQuery(query))
        if (is.null(attr(df, "class"))) {
            warning("argument is not numeric or logical: returning NA")
            meanVal[length(meanVal) + 1] <- NA
        } else meanVal[length(meanVal) + 1] <- as.numeric(df)
        names(meanVal)[length(meanVal)] <- names(x)[i]
    }
    return(meanVal)
}
 
