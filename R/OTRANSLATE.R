OTRANSLATE <- function(x, from_string, to_string = " ") {
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    
    ofmt <- "OTRANSLATE(%s,%s,%s)"
    if (inherits(x, "td.data.frame")) {
        if (length(x) == 1) {
            if (!is.null(attr(x, "expressions"))) 
                val <- attr(x, "expressions")[[names(x)]] else val <- names(x)
            
        } else {
            message("OTRANSLATE warning:  td.data.frame 'x' has length > 1 using first element")
            val <- names(x)[1]
        }
        
        return(asTdExpr(gettextf(ofmt, val, from_string, to_string)))
        
    }
    
    if (inherits(x, "character") || inherits(x, "td.expression")) {
        return(asTdExpr(paste("OTRANSLATE(", x, ",", from_string, ",", to_string, ")", sep = "")))
    }
} 
