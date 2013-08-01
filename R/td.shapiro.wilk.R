td.shapiro.wilk <-
function(tdf, col, thresh=0.05)
{
	if(!is.td.data.frame(tdf))
		stop("'tdf' is not a td data frame")
  if(missing(col) || !nchar(col))
    stop("'col' not specified.")
  if(thresh <= 0 || thresh >=1)
    stop("'thresh' value must be greater than 0 and less than 1.")  
  query <- .td.genshapirowilk(tdf, col, thresh)
  df <- tdQuery(query)
  return(df)
}

