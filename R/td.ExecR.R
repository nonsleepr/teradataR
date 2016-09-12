td.ExecR <- function(selectPhrase=string, ons=list(), returns=NULL, contract=NULL, operator=string, optional_operators=NULL) {
  ons<- unlist(ons)
  ons <- paste(ons, sep="", collapse="\n")
  using <- .td.usingClause(returns=returns, contract=contract, operator=operator)
  queryText <- paste(selectPhrase, "(\n", ons, using, ") ", optional_operators, ") as db;")
  print(queryText)

  return(queryText)

}