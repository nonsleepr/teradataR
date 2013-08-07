td.cov <- function(tdf) {
    if (!is.td.data.frame(tdf)) 
        stop("td.cov requires a td.data.frame")
    if (!length(names(tdf))) 
        stop("td.data.frame contains no columns")
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" WHERE ", attr(tdf, "whereClause"))
    v <- names(tdf)
    vlen <- length(v)
    a <- c()
    for (i in 1:vlen) for (j in i:vlen) a <- c(a, paste("COVAR_POP(", .td.gencolumnexpr(tdf[v[i]]), ",", .td.gencolumnexpr(tdf[v[j]]), 
        ")", sep = ""))
    query <- sprintf("SELECT %s FROM %s %s", paste(a, collapse = ","), obj, wc)
    df <- try(tdQuery(query))
    if (is.null(attr(df, "class"))) 
        stop("'x' must be numeric")
    covMat <- matrix(1:(vlen * vlen), vlen, vlen, dimnames = list(v, v))
    idx <- 1
    for (i in 1:vlen) for (j in i:vlen) {
        covMat[j, i] <- covMat[i, j] <- df[[idx]]
        idx <- idx + 1
    }
    return(covMat)
}
 
