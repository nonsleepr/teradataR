td.cor <-
function(tdf)
{
  if(!is.td.data.frame(tdf))
	  stop("td.cor requires a td.data.frame")
	if(!length(names(tdf)))
    stop("td.data.frame contains no columns")
  obj <- .td.object(attr(tdf,"tableName"),attr(tdf,"database"))
  wc <- ""
  if(!is.null(attr(tdf, "whereClause")))
    wc <- paste(" WHERE ", attr(tdf, "whereClause"))
  v <- names(tdf)
  vlen <- length(v)
  if(vlen < 2)
  {
    corMat <-  matrix(1, 1, 1, dimnames=list(v,v))
    corMat[] <- 1.
    return(corMat)
  }
  a <- c()
  for(i in 1:(vlen-1))
    for(j in (i+1):vlen)
#      a <- c(a,paste("CORR(",v[i],",",v[j],")",sep=""))
      a <- c(a,paste("CORR(",.td.gencolumnexpr(tdf[v[i]]),",",.td.gencolumnexpr(tdf[v[j]]),")",sep=""))
  query <- gettextf("SELECT %s FROM %s %s", paste(a,collapse=","), obj, wc)
  df <- try(tdQuery(query))
  if(is.null(attr(df,"class")))
	  stop("'x' must be numeric")
  corMat <-  matrix(1:(vlen*vlen), vlen, vlen, dimnames=list(v,v))
  corMat[] <- 1.
  idx <- 1
  for(i in 1:(vlen-1))
    for(j in (i+1):vlen)
    {
      corMat[j,i]<- corMat[i,j] <- df[[idx]]
      idx <- idx + 1
    }
  return(corMat)
}

