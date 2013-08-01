td.solve <-
function(tdf, rhscols)
{
	if(!is.td.data.frame(tdf))
		stop("'tdf' is not a td data frame")
	if(!length(names(tdf)))
    stop("td.data.frame contains no columns")
  lhs <- which(!names(tdf) %in% rhscols)
  rhs <- which(names(tdf) %in% rhscols)
	df <- td.cor(tdf)
	m1 <- df[lhs,lhs]
	m2 <- df[lhs,rhs]
	return(solve(m1,m2))
}

