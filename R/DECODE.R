DECODE <- function(x, default=NULL, ...) {
    simplePaste <- function(i) {
      if(inherits(i, "numeric")) {
        res <- as.numeric(paste(i))
      }
      else {
        res <- paste("'",i,"'", sep="")
      }
      return(res)
    }
    params <- list(...)
    res <- lapply(params, simplePaste)
    res <- paste(res, collapse=",")
    if (inherits(x, "td.data.frame")) {
        if (length(x) > 1) {
            message("DECODE warning:  td.data.frame 'x' has length > 1 using first element")
        }
        val <- paste("DECODE(", .td.gencolumnexpr(x[1]), ",", res, ",'", default, "')", sep = "")
    } 
   else if (inherits(x, "td.expression") || inherits(x, "numeric")) {
        val <- paste("DECODE(", x, ",", res, ",'", default, "')", sep = "") 
   }
   else if (inherits(x, "character")) {
        val <- paste("DECODE('", x, "'", ",", res, ",'", default, "')", sep = "") 
   }    
   else stop("Invalid data type for 'x' in DECODE function")
   
   class(val) <- "td.expression"
   return(val)
}