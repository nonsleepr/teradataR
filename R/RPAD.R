RPAD <- function(x, ilength, fill_string = " ") {
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    
    rfmt <- "RPAD(%s,%d,%s)"
    if (inherits(x, "td.data.frame")) {
        if (length(x) == 1) {
            if (!is.null(attr(x, "expressions"))) 
                val <- attr(x, "expressions")[[names(x)]] else val <- names(x)
            
        } else {
            message("RPAD warning:  td.data.frame 'x' has length > 1 using first element")
            val <- names(x)[1]
        }
        
        return(asTdExpr(gettextf(rfmt, val, ilength, fill_string)))
        
    }
    
    if (inherits(x, "character") || inherits(x, "td.expression")) {
        return(asTdExpr(paste("RPAD(", x, ",", ilength, ",", fill_string, ")", sep = "")))
    }
} 
