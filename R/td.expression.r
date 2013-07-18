Ops.td.expression <-
function(e1, e2)
{
  aparen <- bparen <- FALSE
  if(!missing(e1))
  {
    da <- deparse(substitute(e1))
    if(substr(da,1,1) == "(" && substr(da, nchar(da), nchar(da)) == ")")
      aparen <- TRUE
  }
  if(!missing(e2))
  {
    db <- deparse(substitute(e2))
    if(substr(db,1,1) == "(" && substr(db, nchar(db), nchar(db)) == ")")
      bparen <- TRUE
  }
  op = .Generic
  if(op == "%%")
    op = "mod"
  if(is.td.data.frame(e1))
    aval <- paste("\"", names(e1), "\"", sep="")
  if(is.numeric(e1) || is.character(e1) || is.td.expression(e1))
    aval <- e1
  if(missing(e2))
  {
    if(aparen)
      aval <- paste("(", aval, ")", sep="")
    val <- paste(aval, op)
    class(val) <- "td.expression"
    return(val)
  }
  if(is.td.data.frame(e2))
    bval <- paste("\"", names(e2), "\"", sep="")
  if(is.numeric(e2) || is.character(e2) || is.td.expression(e2))
    bval <- e2

  if(aparen)
    aval <- paste("(", aval, ")", sep="")
  if(bparen)
    bval <- paste("(", bval, ")", sep="")

  val <- paste(aval, op, bval)
  class(val) <- "td.expression"
  return(val)
}

#  