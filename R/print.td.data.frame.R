print.td.data.frame <- function(x) {
    obj <- .td.object(attr(x, "tableName"), attr(x, "database"))
    rows <- attr(x, "sampleRows")
    cols <- length(names(x))
    if (is.null(rows)) 
        rows <- attr(x, "totalRows")
    cat(gettextf("Teradata table %s\n\n", obj))
    cat("Columns\n")
    print(names(x))
    if (!is.null(attr(x, "expressions"))) {
        cat("\n")
        cat("Transformed Column Expressions")
        exprs <- attr(x, "expressions")
        names(exprs) <- names(x)[as.integer(names(exprs))]
        cat("\n")
        print(paste(unclass(exprs), "AS", names(exprs)))
    }
    if (!is.null(attr(x, "whereClause"))) {
        cat("\n")
        cat("Condition\n")
        wc <- attr(x, "whereClause")
        print(wc)
    }
    cat(gettextf("\n%d rows\n", rows))
    invisible(x)
}
 
