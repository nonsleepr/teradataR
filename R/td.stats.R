td.stats <-
function(tdf, col, which="all", type="population", group.by=NULL)
{
	if(!is.td.data.frame(tdf))
		stop("'tdf' is not a td data frame")
	if(!missing(group.by) && !is.td.data.frame(group.by))
		stop("'group.by' is not a td data frame")
	if(!length(names(tdf)))
    stop("td.data.frame contains no columns")

  wc <- ""
  if(!is.null(attr(tdf, "whereClause")))
    wc <- paste("WHERE", attr(tdf, "whereClause"))

  if(missing(col))
  {
    nm <- names(tdf)[1]
    col <- .td.gencolumnexpr(tdf[names(tdf)[1]])
  }
  else
    nm <- col
    
  obj <- .td.object(attr(tdf,"tableName"),attr(tdf,"database"))
	stats <- .td.genstats(col, which, type)
	if(is.null(group.by))
	 query <- gettextf("SELECT '%s' as col,%s FROM \"%s\" %s", nm, paste(stats, collapse=","), attr(tdf,"tableName"), wc)
  else
  {
   gb <- paste("\"", names(group.by), "\"", sep="",collapse=",")
	 query <- gettextf("SELECT %s,'%s' as col,%s FROM \"%s\" %s GROUP BY %s ORDER BY %s", gb, nm, 
                     paste(stats, collapse=","), attr(tdf,"tableName"), wc, gb, gb)
  }
	df <- tdQuery(query)
	if(is.data.frame(df))
	  return(df)
	return(df[1])
}

