on <- function(target=NULL, from=NULL, subQuery=NULL, partition=NULL, hash=NULL, order=NULL, local_order=NULL, null_order=NULL, dimension=NULL, as=NULL) {
    if(!is.null(subQuery)) {
      baseText <- paste("on %s%s%s%s", subQuery, "%s")
    }
    else {
      baseText <- "on %s%s%s%s%s"
    }
    if (grepl("select", target)) {
       if(!is.null(from)) {
        returnString <- gettextf(baseText, "(", target," from ", from,  ")")
       }
       else {
         returnString <- gettextf(baseText, "(", target, ")", "", "")
       }
    }
    else {
      returnString <- gettextf(baseText, target, "", "", "", "","")
    }
    
    if(!is.null(as)) {
      returnString <- paste(returnString, .td.makeAs(as), sep="\n")
    }
    if (!is.null(partition)) {
      returnString <- paste( returnString, .td.makePartition(partition), sep = "\n")
    }
    if (!is.null(hash)) {
      returnString <- paste(returnString, .td.makeHash(hash), sep="\n")
    }
    if (!is.null(order)) {
      returnString <- paste(returnString, .td.makeOrder(order), sep="\n")
    }

    if (!is.null(local_order)) {
      returnString <- paste(returnString, .td.makeLocalOrder(null_order, local_order), sep="\n")
    }
    if (!is.null(dimension)) {
      returnString <- paste(returnString, .td.makeDimension(), sep="\n")
    }
    returnString <- gsub(";", "", returnString)   
    return(returnString)
}
