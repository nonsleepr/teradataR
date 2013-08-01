DECODE <- function(x, ...)
{
  parms <- list(...)
  for(i in 1:length(parms))
  {
    if(is.character(parms[[i]]))
      parms[[i]] <- paste("'", parms[[i]], "'", sep="")
  }
  plist <- paste(parms, collapse=",")

  if(inherits(x, "td.data.frame"))
  {
    if(length(x) > 1)
      message("DECODE warning:  td.data.frame 'x' has length > 1 using first element")

    val <- paste("DECODE(", .td.gencolumnexpr(x[1]), ",", plist, ")", sep="")   
  }
  else if(inherits(x,"td.expression") || inherits(x, "numeric"))
    val <- paste("DECODE(", x, ",", plist, ")", sep="")
  else if(inherits(x,"character"))
    val <- paste("DECODE('", x, "'", ",", plist, ")", sep="")
  else 
    stop("Invalid data type for 'x' in DECODE function")

  class(val) <- "td.expression"
  return(val)
}

#DECODE <- function(x, ...)
#{
#  parms <- list(...)
#  for(i in 1:length(parms))
#  {
#    if(is.character(parms[[i]]))
#      parms[[i]] <- paste("'", parms[[i]], "'", sep="")
#  }
#  plist <- paste(parms, collapse=",")
#  
#  if(inherits(x, "td.data.frame"))
#  {
#    if(length(x) == 1)
#    {
#      if(!is.null(attr(x, "expressions")) &&
#         names(x) %in% names(attr(x,"expressions")))
#      {
#        val <- paste("DECODE(", attr(x, "expressions")[[names(x)]],
#                     ",", plist, ")", sep="")
#        class(val) <- "td.expression"
#        return(val)
#      }
#      else
#      {
#        val <- paste("DECODE(\"", names(x), "\",", plist, ")", sep="")
#        class(val) <- "td.expression"
#        return(val)
#      }
#    }
#    else
#    {
#      message("DECODE warning:  td.data.frame 'x' has length > 1 using first element")
#      val <- paste("DECODE(\"", names(x)[1], ",", plist, "\")", sep="")
#      class(val) <- "td.expression"
#      return(val)
#    }
#
#  }
#
#  if(inherits(x, "character") || inherits(x,"td.expression"))
#  {
#    val <- paste("DECODE(", x, ",", plist, ")", sep="")
#    class(val) <- "td.expression"
#    return(val)
#  }
#}
#