td.data.frame <- function(table, database = "") {
    if (missing(database) || is.null(database) || nchar(database) == 0) 
        obj <- gettextf("\"%s\"", table) else obj <- gettextf("\"%s\".\"%s\"", database, table)
    query <- gettextf("SELECT * FROM %s SAMPLE 0", obj)
    res <- try(tdQuery(query))
    if (is.null(attr(res, "class"))) {
        res <- data.frame(stringsAsFactors = FALSE)
        attr(res, "totalRows") <- 0
        warning("Teradata table not found.  Result is empty data frame.")
    } else {
        query <- sprintf("SELECT CAST(COUNT(*) AS FLOAT) FROM %s", obj)
        res2 <- try(tdQuery(query))
        attr(res, "totalRows") <- as.numeric(res2)
    }
    attr(res, "class") <- c("td.data.frame")
    attr(res, "tableName") <- table
    if (!is.null(database) && !missing(database) && nchar(database) > 0) 
        attr(res, "database") <- database else {
        res2 <- try(tdQuery("SELECT DATABASE"))
        if (!is.null(attr(res2, "class"))) 
            attr(res, "database") <- as.character(res2[[1]])
    }
    return(res)
}
 
