INTCAP <- function(x)
{
  if(inherits(x, "td.data.frame"))
  {
    if(length(x) > 1)
      message("INTCAP warning:  td.data.frame 'x' has length > 1 using first element")

    val <- paste("INTCAP(", .td.gencolumnexpr(x[1]), ")", sep="")   
  }
  else if(inherits(x,"td.expression") || inherits(x, "numeric"))
    val <- paste("INTCAP(", x, ")", sep="")
  else if(inherits(x,"character"))
    val <- paste("INTCAP('", x, "')", sep="")
  else 
    stop("Invalid data type for 'x' in INTCAP function")

  class(val) <- "td.expression"
  return(val)
}


#INTCAP <- function(x)
#{
#  asTdExpr <- function(x) {class(x) <- "td.expression"; return(x)}
#
#  if(inherits(x, "td.data.frame"))
#  {
#    if(length(x) == 1)
#      if(!is.null(attr(x, "expressions")))
#        return(asTdExpr(paste("INTCAP(", attr(x, "expressions")[[names(x)]], ")", sep="")))
#      else
#        return(asTdExpr(paste("INTCAP(\"", names(x), "\")", sep="")))
#    else
#    {
#      message("INTCAP warning:  td.data.frame 'x' has length > 1 using first element")
#      return(asTdExpr(paste("INTCAP(\"", names(x)[1], "\")", sep="")))
#    }
#      
#  }
#  
#  if(inherits(x, "character") || inherits(x,"td.expression"))
#  {
#    return(asTdExpr(paste("INTCAP(", x, ")", sep="")))
#  }
#}
#