sum.td.data.frame <-
function(tdf, ...)
{
	if(!is.td.data.frame(tdf))
		stop("'tdf' is not a td data frame")
	if(!length(names(tdf)))
    stop("td.data.frame contains no columns")
	obj <- .td.object(attr(tdf,"tableName"),attr(tdf,"database"))
	sumVal <- 0
	exprs <- attr(tdf, "expressions")

  wc <- ""
  if(!is.null(attr(tdf, "whereClause")))
    wc <- paste(" WHERE ", attr(tdf, "whereClause"))
	
	for(i in 1:length(names(tdf)))
	{
	  if(as.character(i) %in% names(exprs))
		  query <- gettextf("SELECT SUM(%s) FROM %s %s", .td.gencolumnexpr(tdf[i]), obj, wc)
		else
		  query <- gettextf("SELECT SUM(\"%s\") FROM %s %s", names(tdf)[i], obj, wc)
		df <- try(tdQuery(query))
    if(is.null(attr(df,"class")))
      stop("Only defined on a td data frame with all numeric variables")
    sumVal <- sumVal + as.numeric(df)
	}
	return(sumVal)
}

