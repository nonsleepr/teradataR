ASCII <- function(x)
{
  if(inherits(x, "td.data.frame"))
  {
    if(length(x) > 1)
      message("ASCII warning:  td.data.frame 'x' has length > 1 using first element")

    val <- paste("ASCII(", .td.gencolumnexpr(x[1]), ")", sep="")   
  }
  else if(inherits(x,"td.expression"))
    val <- paste("ASCII(", x, ")", sep="")
  else if(inherits(x,"character"))
    val <- paste("ASCII('", x, "')", sep="")
  else 
    stop("Invalid data type for 'x' in ASCII function")

  class(val) <- "td.expression"
  return(val)
}
