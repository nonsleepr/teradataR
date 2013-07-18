td.merge <-
function(tdf1, tdf2, oTable="newMergeTable", oDatabase="", mergeType="union")
{
  oObj <- .td.object(oTable,oDatabase)
  
  mtypes <-  c("union","minus","intersect")
  mlist <- paste(mtypes, collapse=",")
  if(!mergeType %in% mtypes)
    stop(gettextf("Unknown 'mergeType'.  Must be one of %s", mlist))
  mergeText <- .td.genmerge(tdf1, tdf2, mergeType)
  if(.td.objectExists(oObj))
    stop(gettextf("Table %s already exists.", oObj))
	query <- gettextf("CREATE TABLE %s AS (%s) WITH DATA",
	                  oObj, mergeText)
	df <- try(tdQuery(query))
  if(length(df) == 1L && df == "No Data")
    return(td.data.frame(oTable, oDatabase))
  else
    stop(gettextf("Error: %s", paste(df, collapse="")))
}

