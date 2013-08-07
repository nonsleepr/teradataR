td.zscore <- function(tdf, col, oTable, oDatabase = "") {
    if (!is.td.data.frame(tdf)) 
        stop("'tdf' is not a td data frame")
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    oObj <- .td.object(oTable, oDatabase)
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste("WHERE", attr(tdf, "whereClause"))
    nm <- names(tdf)
    if (!col %in% nm) 
        stop("'col' to transform is not a column in the td data frame.")
    nm <- nm[-(grep(col, nm))]
    nms <- paste(rep(gettextf("\"%s\"", nm)), collapse = ",")
    zscoreText <- .td.genzscore(col, as = col)
    nms <- paste(nms, ",", zscoreText, collapse = ",")
    query <- gettextf("CREATE TABLE %s AS (SELECT %s FROM %s, (%s) A %s) WITH DATA", oObj, nms, obj, gettextf("SELECT %s FROM %s %s", 
        paste(.td.genstats(col, c("mean", "std")), collapse = ","), obj, wc), wc)
    if (.td.objectExists(oObj)) 
        stop(gettextf("Table %s already exists.", oObj))
    df <- try(tdQueryUpdate(query))
    if (length(df) == 1L && df == "No Data") 
        return(td.data.frame(oTable, oDatabase)) else stop(gettextf("Error: %s", paste(df, collapse = "")))
    stop(gettextf("Error: %s", paste(df, collapse = "")))
}
 
