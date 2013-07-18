REGR_INTERCEPT <- function(x, y)
{
  if(inherits(x, "td.data.frame"))
  {
    if(length(x) > 1)
      message("REGR_INTERCEPT warning:  td.data.frame 'x' has length > 1 using first element")

    xval <- .td.gencolumnexpr(x[1])
  }
  else if(inherits(x,"td.expression"))
    xval <- x
  else if(inherits(x,"character"))
    xval <- paste("'", x, "'", sep="")
  else 
    stop("Invalid data type for 'x' in REGR_INTERCEPT function")
  
  if(inherits(y, "td.data.frame"))
  {
    if(length(y) > 1)
      message("REGR_INTERCEPT warning:  td.data.frame 'y' has length > 1 using first element")

    yval <- .td.gencolumnexpr(y[1])
  }
  else if(inherits(y,"td.expression"))
    yval <- y
  else if(inherits(y,"character"))
    yval <- paste("'", y, "'", sep="")
  else 
    stop("Invalid data type for 'y' in REGR_INTERCEPT function")


  val <- paste("REGR_INTERCEPT(", xval, ",", yval, ")", sep="")

  class(val) <- "td.expression"
  return(val)
}

#CEIL <- function(x)
#{
#  if(inherits(x, "td.data.frame"))
#  {
#    if(length(x) == 1)
#      if(!is.null(attr(x, "expressions")))
#      {
#        val <- paste("CEIL(", attr(x, "expressions")[[names(x)]], ")", sep="")
#        class(val) <- "td.expression"
#        return(val)
#      }
#      else
#      {
#        val <- paste("CEIL(\"", names(x), "\")", sep="")
#        class(val) <- "td.expression"
#        return(val)
#      }
#    else
#    {
#      message("CEIL warning:  td.data.frame 'x' has length > 1 using first element")
#      val <- paste("CEIL(\"", names(x)[1], "\")", sep="")
#      class(val) <- "td.expression"
#      return(val)
#    }
#      
#  }
#  
#  if(inherits(x, "character") || inherits(x,"td.expression"))
#  {
#    val <- paste("CEIL(", x, ")", sep="")
#    class(val) <- "td.expression"
#    return(val)
#  }
#}
#