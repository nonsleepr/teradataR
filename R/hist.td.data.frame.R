hist.td.data.frame <-
function(x, breaks=10, ...) {
  oldmf <- par("mfrow")
  oldoma <- par("oma")
  oldmar <- par("mar")
  on.exit(par(mfrow = oldmf, oma = oldoma, mar = oldmar))
  la <- length(x)
  mf <- if(la <= 1)
    c(1, 1)
    else if (la <= 2)
    c(2, 1)
    else if (la <= 4)
    c(2, 2)
    else if (la <= 6)
    c(2, 3)
    else if (la <= 9)
    c(3, 3)
    else if (la <= 12)
    c(3, 4)
    else
    c(4, 4)
  if(la > 16)
  {
    la <- 16
    warning("Only showing first 16 variables")
  }
  par(mfrow = mf)
  
  for(i in 1:la)
  {
  	if(!is.null(attr(x[i], "expressions")))
  	  fnc <- .td.objectIsNumeric2
    else
      fnc <- .td.objectIsNumeric
      
    if(fnc(attr(x,"database"), attr(x,"tableName"),
       .td.gencolumnexpr(x[names(x)[i]])))
       td.hist(x, names(x)[i], breaks)
    else
    {
      df <- td.freq(x, names(x)[i], plot=FALSE)
      rvec <- df[[4]]
      names(rvec) <- df[[3]]
      dotchart(rvec, xlab = paste("Frequencies for", names(x)[i]))
    }
  }
  
}

