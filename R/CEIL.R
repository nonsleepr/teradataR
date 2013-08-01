CEIL <- function(x)
{
  if(inherits(x, "td.data.frame"))
  {
    if(length(x) > 1)
      message("CEIL warning:  td.data.frame 'x' has length > 1 using first element")

    val <- paste("CEIL(", .td.gencolumnexpr(x[1]), ")", sep="")   
  }
  else if(inherits(x,"td.expression") || inherits(x, "numeric"))
    val <- paste("CEIL(", x, ")", sep="")
  else if(inherits(x,"character"))
    val <- paste("CEIL('", x, "')", sep="")
  else 
    stop("Invalid data type for 'x' in CEIL function")

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