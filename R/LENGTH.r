LENGTH <- function(x)
{
  if(inherits(x, "td.data.frame"))
  {
    if(length(x) > 1)
      message("LENGTH warning:  td.data.frame 'x' has length > 1 using first element")

    val <- paste("LENGTH(", .td.gencolumnexpr(x[1]), ")", sep="")   
  }
  else if(inherits(x,"td.expression") || inherits(x, "numeric"))
    val <- paste("LENGTH(", x, ")", sep="")
  else if(inherits(x,"character"))
    val <- paste("LENGTH('", x, "')", sep="")
  else 
    stop("Invalid data type for 'x' in LENGTH function")

  class(val) <- "td.expression"
  return(val)
}

#LENGTH <- function(x)
#{
#  asTdExpr <- function(x) {class(x) <- "td.expression"; return(x)}
#
#  if(inherits(x, "td.data.frame"))
#  {
#    if(length(x) == 1)
#      if(!is.null(attr(x, "expressions")))
#        return(asTdExpr(paste("LENGTH(", attr(x, "expressions")[[names(x)]], ")", sep="")))
#      else
#        return(asTdExpr(paste("LENGTH(\"", names(x), "\")", sep="")))
#    else
#    {
#      message("LENGTH warning:  td.data.frame 'x' has length > 1 using first element")
#      return(asTdExpr(paste("LENGTH(\"", names(x)[1], "\")", sep="")))
#    }
#
#  }
#
#  if(inherits(x, "character") || inherits(x,"td.expression"))
#  {
#    return(asTdExpr(paste("LENGTH(", x, ")", sep="")))
#  }
#}
#