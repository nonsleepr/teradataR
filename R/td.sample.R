td.sample <-
function(tdf,sizes=10,otable="",odatabase="")
{
	if(!is.td.data.frame(tdf))
		stop("'tdf' is not a td data frame")
  obj <- .td.object(attr(tdf,"tableName"),attr(tdf,"database"))
  exprs <- attr(tdf, "expressions")
  
  query <- .td.tdf2sql(tdf, sizes)
  
	if(nchar(otable))
	{
	  if(obj == oObj)
	    stop("Source and destination table cannot be the same.")
	  oObj <- .td.object(otable, odatabase)
    if(.td.objectExists(oObj))
      stop(gettextf("Table %s already exists.", oObj))
	  query <- gettextf("CREATE TABLE %s AS (%s) WITH DATA",
	                    oObj, query)
	}
	df <- try(tdQuery(query))
	if(is.data.frame(df))
	  return(df)
  if(length(df) == 1L && df == "No Data")
	  return(td.data.frame(otable,odatabase))
  if(length(df) == 2L)
    stop(df[1])
  stop("Unknown error in td.sample")
}

