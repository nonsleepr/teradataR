td.join <-
function(tdf1, tdf2, oTable="newJoinTable", oDatabase="",
                   index1="", index2="", joinType="inner")
{
  oObj <- .td.object(oTable,oDatabase)
  if(missing(index1))
    index1 <- .td.getPrimaryIndicies(tdf1)
  if(missing(index2))
    index2 <- .td.getPrimaryIndicies(tdf2)
  if(length(index1) != length(index2))
    stop("Index values to join on do not match.")
  jtypes <- c("inner", "left outer", "right outer", "full outer")
  tlist <- paste(jtypes, collapse=",")
  if(!joinType %in% jtypes)
    stop(gettextf("Unknown 'joinType'.  Must be one of %s", tlist))
  joinText <- .td.genjoin(tdf1, tdf2, index1, index2, joinType)
  if(.td.objectExists(oObj))
    stop(gettextf("Table %s already exists.", oObj))
	query <- gettextf("CREATE TABLE %s AS (%s) WITH DATA",
	                  oObj, joinText)
	df <- try(tdQueryUpdate(query))
  if(length(df) == 1L && df == "No Data")
    return(td.data.frame(oTable, oDatabase))
  else
    stop(gettextf("Error: %s", paste(df, collapse="")))
}

