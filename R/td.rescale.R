td.rescale <-
function(tdf, col, oTable, oDatabase="", lower=0.0, upper=1.0)
{
	if(!is.td.data.frame(tdf))
		stop("'tdf' is not a td data frame")
  obj <- .td.object(attr(tdf,"tableName"),attr(tdf,"database"))
  wc <- ""
  if(!is.null(attr(tdf, "whereClause")))
    wc <- paste(" WHERE ", attr(tdf, "whereClause"))
  oObj <- .td.object(oTable,oDatabase)
 	nm <- names(tdf)
 	if(!col %in% nm)
 	  stop("'col' to transform is not a column in the td data frame.")
 	nm <- nm[-(grep(col,nm))]
	nms <- paste(rep(gettextf("\"%s\"",nm)),collapse=",")
	rescaleText <- .td.genrescale(col, as=col, lower, upper)
	nms <- paste(nms, ",", rescaleText, collapse=",")
	query <- gettextf("CREATE TABLE %s AS (SELECT %s FROM %s, (%s) A %s) WITH DATA",
                    oObj, nms, obj,
                    gettextf("SELECT %s FROM %s %s", paste(.td.genstats(col,c("min","max")),collapse=","), obj, wc), wc)
  if(.td.objectExists(oObj))
    stop(gettextf("Table %s already exists.", oObj))
	df <- try(tdQuery(query))
  if(length(df) == 1L && df == "No Data")
    return(td.data.frame(oTable, oDatabase))
  else
    stop(gettextf("Error: %s", paste(df, collapse="")))
  stop(gettextf("Error: %s", paste(df, collapse="")))
}

