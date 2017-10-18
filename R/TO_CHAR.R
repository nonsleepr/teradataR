TO_CHAR <- function(x, format) {
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    
    tfmt <- "TO_CHAR(%s, %s)"
    if (inherits(x, "td.data.frame") || inherits(format, "td.data.frame")) {
        if (length(x) == 1 || length(format) == 1) {
            if (!is.null(attr(x, "expressions"))) {
                val1 <- attr(x, "expressions")[[names(x)]] 
            }
            else {
                val1 <- names(x)
            }
            if (!is.null(attr(format, "expressions"))) {
                val2 <- attr(format, "expressions")[[names(format)]]
            }
            else {
              val2 <- names(format)
            }
        } 
        else {
            message("TO_CHAR warning:  td.data.frame 'x' or 'format' has length > 1 using first element")
            val1 <- names(x)[1]
            val2 <- names(format)[1]
        }
        
        return(asTdExpr(gettextf(tfmt, val1, val2)))
        
    }
    
    if (inherits(x, "character") || inherits(x, "td.expression")) {
        return(asTdExpr(paste("TO_CHAR(", x, format, ")", sep = "")))
    }
} 
