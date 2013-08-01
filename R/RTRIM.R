RTRIM <- function(x, rstring=" ")
{
  asTdExpr <- function(x) {class(x) <- "td.expression"; return(x)}

  rfmt <- "RTRIM(%s,%s)"
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
      message("RTRIM warning:  td.data.frame 'x' has length > 1 using first element")
      val <- names(x)[1]
    }

    return(asTdExpr(gettextf(rfmt, val, rstring)))

  }

  if(inherits(x, "character") || inherits(x,"td.expression"))
  {
    return(asTdExpr(paste("RTRIM(", x, ",", fill_string, ")", sep="")))
  }
}
