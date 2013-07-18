td.freq <-
function(tdf, col, plot=TRUE,...)
{
	if(!is.td.data.frame(tdf))
		stop("'tdf' is not a td data frame")
  if(missing(col))
  {
    nm <- names(tdf)[1]
    col <- .td.gencolumnexpr(tdf[names(tdf)[1]])
  }
  else
  {
    nm <- col
    col <- .td.gencolumnexpr(tdf[col])
  }
  obj <- .td.object(attr(tdf,"tableName"),attr(tdf,"database"))
  query <- .td.genfreq(tdf, col)
	df <- try(tdQuery(query))
  if(is.null(attr(df,"class")))
    stop("Error retrieving frequency values from table.")
  if(plot==TRUE)
  {
    ttl <- gettextf("Frequency of %s", nm)
    xTbl <- as.table(df[[4]], df[[3]])
    names(xTbl) <- df[[3]]
    barplot(xTbl, col="dark red",xlab=col,ylab="Counts",main=ttl)
    invisible(df)
  }
  else
    return(df)
}

