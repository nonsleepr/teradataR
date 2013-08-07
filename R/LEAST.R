LEAST <- function(...) {
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    
    parms <- list(...)
    if (length(parms) > 10 || length(parms) == 0) 
        stop("Error: LEAST must take from 1-10 parameters")
    
    pfmt <- "LEAST(%s)"
    inputs <- character(0)
    
    for (i in 1:length(parms)) {
        x <- parms[[i]]
        if (inherits(x, "td.data.frame")) {
            if (length(x) > 1) 
                message("LEAST warning:  td.data.frame 'x' has length > 1 using first element")
            
            inputs[i] <- .td.gencolumnexpr(x[1])
        } else if (inherits(x, "character")) 
            inputs[i] <- paste("'", x, "'", sep = "") else if (inherits(x, "td.expression") || inherits(x, "numeric")) 
            inputs[i] <- x else stop(gettextf("Invalid data type for parameter %d in LEAST function", i))
    }
    
    return(asTdExpr(gettextf(pfmt, paste(inputs, collapse = ","))))
} 
