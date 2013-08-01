td.mode <-
function(tdf, col)
{
	if(!is.td.data.frame(tdf))
		stop("'tdf' is not a td data frame")
	if(!length(names(tdf)))
    stop("td.data.frame contains no columns")
  if(missing(col) && length(tdf) == 1L)
    col <- names(tdf)[1]
  query <- .td.genmode(tdf, col)
	df <- try(tdQuery(query))
  if(is.null(class(df)))
    stop("Error retrieving mode values from table.")
  return(df)
}

