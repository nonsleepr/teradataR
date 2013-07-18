td.factanal <-
function(tdf, ...)
{
	if(!is.td.data.frame(tdf))
		stop("'tdf' is not a td data frame")
	df <- td.cov(tdf)
	return(factanal(covmat=df,...))
}

