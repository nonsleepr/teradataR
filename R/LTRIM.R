LTRIM <- function(x, rstring=" ")
{
  asTdExpr <- function(x) {class(x) <- "td.expression"; return(x)}

  lfmt <- "LTRIM(%s,%s)"
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
      message("LTRIM warning:  td.data.frame 'x' has length > 1 using first element")
      val <- names(x)[1]
    }
    
    return(asTdExpr(gettextf(lfmt, val, rstring)))

  }

  if(inherits(x, "character") || inherits(x,"td.expression"))
  {
    return(asTdExpr(paste("LTRIM(", x, ",", fill_string, ")", sep="")))
  }
}
