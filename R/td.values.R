td.values <-
function(tdf, col, group.by=NULL)
{
	if(!is.td.data.frame(tdf))
		stop("'tdf' is not a td data frame")
	if(!length(names(tdf)))
    stop("td.data.frame contains no columns")
  if(!missing(group.by) && !is.td.data.frame(group.by))
    stop("'group.by' is not a td data frame")

  wc <- ""
  if(!is.null(attr(tdf, "whereClause")))
    wc <- paste("WHERE", attr(tdf, "whereClause"))

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
	values <- .td.genvalues(col)
	
	if(!is.null(attr(tdf[nm], "expressions")))
	  fnc <- .td.objectIsNumeric2
  else
    fnc <- .td.objectIsNumeric
	
	if(fnc(attr(tdf,"database"), attr(tdf,"tableName"), col))
  	vals <- paste(values[1:5], collapse=",")
	else
    vals <- paste(values[c(1,5,6)], collapse=",")
  
  if(length(group.by))  
  {
    gb <- paste("\"", names(group.by), "\"", sep="",collapse=",")
  	query <- gettextf("SELECT %s,'%s' as col,%s FROM %s %s GROUP BY %s ORDER BY %s", gb, nm, vals, obj, wc, gb, gb)
 	}
 	else
  	query <- gettextf("SELECT '%s' as col,%s FROM %s %s", nm, vals, obj, wc)
	return(tdQuery(query))
}

