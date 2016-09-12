POWER <- function(x, exponent = 1) {
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    
    pfmt <- "POWER(%s, %s)"
    if (inherits(x, "td.data.frame")) {
        if (length(x) == 1 || length(exponent0) == 1) {
            if (!is.null(attr(x, "expressions"))) {
                val1 <- attr(x, "expressions")[[names(x)]]
            }
            else {
                val1 <- names(x)
            }
            if (!is.null(attr(exponent, "expressions"))) {
                val2 <- attr(exponent, "expressions")[[names(x)]]
            }
            else {
              val2 <- names(exponent)
            }
        }
        else {
            message("POWER warning:  td.data.frame 'x' or 'exponent' has length > 1 using first element")
            val1 <- names(x)[1]
            val2 <- names(exponent)[1]
        }
        
        return(asTdExpr(gettextf(pfmt, val1, val2)))
        
    }
    
    if (inherits(x, "character") || inherits(x, "td.expression")) {
        return(asTdExpr(paste("POWER(", x, exponent, ")", sep = "")))
    }
} 
