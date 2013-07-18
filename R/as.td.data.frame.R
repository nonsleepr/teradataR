as.td.data.frame <-
function(x, ...)
{
  if(inherits(x,"td.data.frame"))
  {
    args <- list(...)
    if(is.null(args[["tableName"]]))
      tbl <- deparse(substitute(x))
    else
      tbl <- args[["tableName"]]

    if(!is.null(args[["database"]]))
      oDatabase <- args[["database"]]
    else
      oDatabase <- NULL

    oObj <- .td.object(tbl,oDatabase)
    selectText <- .td.tdf2sql(x)
    
	  query <- gettextf("CREATE TABLE %s AS (%s) WITH DATA",
	                  oObj, selectText)
	  df <- try(tdQueryUpdate(query))
    if(length(df) == 1L && df == "No Data")
      return(td.data.frame(tbl, oDatabase))
    else
      stop(gettextf("Error: %s", paste(df, collapse="")))
  }
  if(inherits(x,"data.frame"))
  {
    tbl <- deparse(substitute(x))
    tdSave(x, tbl)
    return(td.data.frame(tbl))
  }
  stop("Unable to convert object to td.data.frame")
}

