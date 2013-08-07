td.sample <- function(tdf, sizes = missing, otable = "", odatabase = "") {
    if (!is.td.data.frame(tdf)) 
        stop("'tdf' is not a td data frame")
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    exprs <- attr(tdf, "expressions")
    
    if (missing(sizes)) {
        query <- .td.tdf2sql(tdf)
    } else {
        query <- .td.tdf2sql(tdf, sizes)
    }
    
    if (nchar(otable)) {
        oObj <- .td.object(otable, odatabase)
        if (.td.objectExists(oObj)) 
            stop(gettextf("Table %s already exists.", oObj))
        query <- gettextf("CREATE TABLE %s AS (%s) WITH DATA", oObj, query)
    }
    df <- try(tdQueryUpdate(query))
    if (is.data.frame(df)) 
        return(df)
    if (length(df) == 1L && df == "No Data") 
        return(td.data.frame(otable, odatabase))
    if (length(df) == 2L) 
        stop(df[1])
    stop("Unknown error in td.sample")
}
 
