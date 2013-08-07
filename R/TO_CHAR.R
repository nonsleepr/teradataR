TO_CHAR <- function(x, format = " ") {
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    
    tfmt <- "TO_CHAR(%s,%s)"
    if (inherits(x, "td.data.frame")) {
        if (length(x) == 1) {
            if (!is.null(attr(x, "expressions"))) 
                val <- attr(x, "expressions")[[names(x)]] else val <- names(x)
            
        } else {
            message("TO_CHAR warning:  td.data.frame 'x' has length > 1 using first element")
            val <- names(x)[1]
        }
        
        return(asTdExpr(gettextf(tfmt, val, format)))
        
    }
    
    if (inherits(x, "character") || inherits(x, "td.expression")) {
        return(asTdExpr(paste("TO_CHAR(", x, ",", format, ")", sep = "")))
    }
} 
