subset.td.data.frame <-
function (x, subset, select, drop = FALSE, ...)
{
    if (!missing(subset))
    {
        e <- substitute(subset)
        wc <- deparse(e)
#        r <- eval(e, x, parent.frame())
#        if (!is.logical(r))
#            stop("'subset' must evaluate to logical")
#        r <- r & !is.na(r)
    }
    if (missing(select))
        vars <- TRUE
    else {
        nl <- as.list(seq_along(x))
        names(nl) <- names(x)
        vars <- eval(substitute(select), nl, parent.frame())
    }
    
    if(exists("wc"))
    {
      y <- x[vars]
      wc <- gsub("==", "=", wc)
      wc <- gsub("&&", " AND ", wc)
      wc <- gsub("&", " AND ", wc)
      wc <- gsub("||", " OR ", wc, fixed=TRUE)
      wc <- gsub("|", " OR ", wc, fixed=TRUE)
      wc <- gsub("^", "**", wc, fixed=TRUE)
      wc <- gsub("%%", " MOD ", wc)
      wc <- gsub("\"", "'", wc, fixed=TRUE)
      wc <- gsub("%in%", " IN ", wc)
      wc <- gsub("c(", "(", wc, fixed=TRUE)

      if(!is.null(attr(y, "whereClause")))
        wc <- paste(attr(y, "whereClause"), "AND", wc)

      attr(y, "whereClause") <- wc

      obj <- .td.object(attr(x,"tableName"),attr(x,"database"))
      query <- sprintf("SELECT CAST(COUNT(*) AS FLOAT) FROM %s WHERE %s", obj, wc)
      res <- try(tdQuery(query))
      if(!is.data.frame(res))
      {
        warning("Invalid subset expression.")
        return(x[vars])
      }
      attr(y, "totalRows") <- as.numeric(res)
      return(y)
    }
    
    return(x[vars])
}
