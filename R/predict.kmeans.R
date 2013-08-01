predict.kmeans <- function(object, newdata=NULL, oTable="", oDatabase="", ...)
{
  if(is.data.frame(newdata))
  {
    if(is.null(data))
      return(object$cluster)
    cntrs <- object$centers
    cr <- nrow(cntrs)
    cnames <- colnames(cntrs)
    ndata <- newdata[, cnames, drop=FALSE]
    cids <- rep(NA,nrow(ndata))
    ndata <- rbind(cntrs, ndata)
    dists <- numeric(cr)
    for(i in (cr+1):nrow(ndata))
    {
       for(j in 1:(cr))
         dists[j] <- dist(ndata[c(j,i),])
       newc <- which.min(dists)
       if(length(newc)>0)
         cids[i-cr] <- newc
    }
    return(as.numeric(cids))
  }
  
  if(is.td.data.frame(newdata))
  {
    parms <- pairlist(...)
    if(!nchar(oTable))
      stop("When using a td data frame you must supply an 'oTable' parameter")
    oObj <- .td.object(oTable, oDatabase)
    kmeansText <- .td.genkmeans(newdata, object)
    query <- gettextf("CREATE TABLE %s AS (%s) WITH DATA",
	                    oObj, kmeansText)
    if(.td.objectExists(oObj))
      stop(gettextf("Table %s already exists.", oObj))
	df <- try(tdQueryUpdate(query))
    if(length(df) == 1L && df == "No Data")
      return(td.data.frame(oTable, oDatabase))
    else
      stop(gettextf("Error: %s", paste(df, collapse="")))
  }
}

.td.genkmeans <- function(tdf, km)
{
  wc <- ""
  if(!is.null(attr(tdf, "whereClause")))
    wc <- paste("WHERE", attr(tdf, "whereClause"))

  obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
  clusterId <- .td.genclusterid(as.data.frame(km$centers), "clusterID")
  query <- gettextf("SELECT %s,%s FROM %s %s", .td.gencolumnexpr(tdf), clusterId, obj, wc)
  return(query)
}
