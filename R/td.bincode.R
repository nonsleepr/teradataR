td.bincode <- function(tdf, col, oTable, oDatabase = "", bins = 10) {
    if (!is.td.data.frame(tdf)) 
        stop("'tdf' is not a td data frame")
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    oObj <- .td.object(oTable, oDatabase)
    nm <- names(tdf)
    nm <- nm[-(match(col, nm))]
    if (!(col %in% names(tdf))) 
        stop(gettextf("Column %s not found in td.data.frame", col))
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" WHERE ", attr(tdf, "whereClause"))
    nms <- paste(rep(gettextf("\"%s\"", nm)), collapse = ",")
    colexpr <- .td.gencolumnexpr(tdf[col])
    binText <- .td.genbin(colexpr, bins = bins, as = col)
    nms <- paste(nms, ",", binText, collapse = ",")
    query <- gettextf("CREATE TABLE %s AS (SELECT %s FROM %s, (%s) A %s) WITH DATA", oObj, nms, obj, gettextf("SELECT %s FROM %s %s", 
        paste(.td.genstats(colexpr, c("min", "max")), collapse = ","), obj, wc), wc)
    if (.td.objectExists(oObj)) 
        stop(gettextf("Table %s already exists.", oObj))
    df <- try(tdQueryUpdate(query))
    if (length(df) == 1L && df == "No Data") 
        return(td.data.frame(oTable, oDatabase)) else stop(gettextf("Error: %s", paste(df, collapse = "")))
    stop(gettextf("Error: %s", paste(df, collapse = "")))
}
 
