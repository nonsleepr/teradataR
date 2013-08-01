TRUNC <- function(x, places=1)
{
  asTdExpr <- function(x) {class(x) <- "td.expression"; return(x)}

  pfmt <- "TRUNC(%s,%d)"
  if(inherits(x, "td.data.frame"))
  {
    if(length(x) == 1)
    {
      if(!is.null(attr(x, "expressions")))
        val <- attr(x, "expressions")[[names(x)]]
      else
        val <- names(x)

    }
    else
    {
      message("TRUNC warning:  td.data.frame 'x' has length > 1 using first element")
      val <- names(x)[1]
    }

    return(asTdExpr(gettextf(pfmt, val, places)))

  }

  if(inherits(x, "character") || inherits(x,"td.expression"))
  {
    return(asTdExpr(paste("TRUNC(", x, ",", places, ")", sep="")))
  }
}
