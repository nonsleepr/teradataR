td.quantiles <-
function(tdf, col, qType=c("ter","quar","dec"))
{
	if(!is.td.data.frame(tdf))
		stop("'tdf' is not a td data frame")
	if(!length(names(tdf)))
    stop("td.data.frame contains no columns")
  if(!all(qType %in% c("ter","quar","dec")))
    stop("'qType' is not a valid type.")
  if(missing(col))
    col <- .td.gencolumnexpr(tdf[names(tdf)[1]])
  else
    col <- .td.gencolumnexpr(tdf[col])
    
  query <- .td.genquantiles(tdf, col, qType)
	df <- try(tdQuery(query))
  if(is.null(class(df)))
    stop("Error retrieving quantile values from table.")
  return(df)
}

