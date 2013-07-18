OREPLACE <- function(x, search_string, replace_string=" ")
{
  asTdExpr <- function(x) {class(x) <- "td.expression"; return(x)}

  rfmt <- "OREPLACE(%s,%s,%s)"
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
      message("OREPLACE warning:  td.data.frame 'x' has length > 1 using first element")
      val <- names(x)[1]
    }

    return(asTdExpr(gettextf(rfmt, val, search_string, replace_string)))

  }

  if(inherits(x, "character") || inherits(x,"td.expression"))
  {
    return(asTdExpr(paste("OREPLACE(", x, ",", search_string, ",", replace_string, ")", sep="")))
  }
}
