FLOOR <- function(x)
{
  if(inherits(x, "td.data.frame"))
  {
    if(length(x) > 1)
      message("FLOOR warning:  td.data.frame 'x' has length > 1 using first element")

    val <- paste("FLOOR(", .td.gencolumnexpr(x[1]), ")", sep="")   
  }
  else if(inherits(x,"td.expression") || inherits(x,"numeric"))
    val <- paste("FLOOR(", x, ")", sep="")
  else if(inherits(x,"character"))
    val <- paste("FLOOR('", x, "')", sep="")
  else 
    stop("Invalid data type for 'x' in FLOOR function")

  class(val) <- "td.expression"
  return(val)
}

#FLOOR <- function(x)
#{
#  asTdExpr <- function(x) {class(x) <- "td.expression"; return(x)}
#
#  if(inherits(x, "td.data.frame"))
#  {
#    if(length(x) == 1)
#      if(!is.null(attr(x, "expressions")))
#        return(asTdExpr(paste("FLOOR(", attr(x, "expressions")[[1]], ")", sep="")))
#      else
#        return(asTdExpr(paste("FLOOR(\"", names(x), "\")", sep="")))
#    else
#    {
#      message("FLOOR warning:  td.data.frame 'x' has length > 1 using first element")
#      if(any(names(attr(x,"expressions")) == "1"))
#        return(asTdExpr(paste("FLOOR(", attr(x, "expressions")[[1]], ")", sep="")))
#      else
#        return(asTdExpr(paste("FLOOR(\"", names(x)[1], "\")", sep="")))
#    }
#
#  }
#
#  if(inherits(x, "character") || inherits(x, "td.expression"))
#  {
#    return(asTdExpr(paste("FLOOR(", x, ")", sep="")))
#  }
#  
#  stop("Invalid data type supplied to FLOOR")
#}
#