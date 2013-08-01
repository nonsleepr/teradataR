td.nullreplace <-
function(tdf, col, oTable, oDatabase="", rType="literal", rVal=0.0)
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
	nrepText <- .td.gennrep(col, as=col, rType, rVal)
	nms <- paste(nms, ",", nrepText, collapse=",")
  if(rType == "literal")
	  query <- gettextf("CREATE TABLE %s AS (SELECT %s FROM %s %s) WITH DATA",
                    oObj, nms, obj, wc)
  else if(rType == "mean")
	  query <- gettextf("CREATE TABLE %s AS (SELECT %s FROM %s, (%s) A %s) WITH DATA",
                    oObj, nms, obj,
                    gettextf("SELECT %s FROM %s %s", paste(.td.genstats(col,c("mean")),collapse=","), obj, wc), wc)
  else if(rType == "mode")
	  query <- gettextf("CREATE TABLE %s AS (SELECT %s FROM %s, (%s) A %s) WITH DATA",
                    oObj, nms, obj, .td.genmode(tdf, col), wc)
  else if(rType == "median")
	  query <- gettextf("CREATE TABLE %s AS (SELECT %s FROM %s, (%s) A %s) WITH DATA",
                    oObj, nms, obj, .td.genmedian(tdf, col), wc)
  else
	  query <- gettextf("CREATE TABLE %s AS (SELECT %s FROM %s %s) WITH DATA",
                    oObj, nms, obj, wc)
  if(.td.objectExists(oObj))
    stop(gettextf("Table %s already exists.", oObj))
	df <- try(tdQueryUpdate(query))
  if(length(df) == 1L && df == "No Data")
    return(td.data.frame(oTable, oDatabase))
  else
    stop(gettextf("Error: %s", paste(df, collapse="")))
  stop(gettextf("Error: %s", paste(df, collapse="")))
}

