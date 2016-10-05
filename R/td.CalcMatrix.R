td.CalcMatrix <- function(selectPhrase=string, ons=string, phase=NULL, calctype=NULL, output=NULL, null_handling=NULL, optional_operators=NULL, as=NULL) {

  ons <- unlist(ons)
  ons <- paste(ons, sep="", collapse="\n")
  using <- .td.usingClause(phase=phase, calctype=calctype, output=output, null_handling=null_handling)
  queryText <- paste(selectPhrase, "(\n", ons, using, ") ", optional_operators, " as ", as, ";")
    
  print(queryText)
  return(queryText)
}
