NGRAM <- function(x, second_string, gram_length) {
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    
    ofmt <- "NGRAM(%s,%s,%d)"
    if (inherits(x, "td.data.frame")) {
        if (length(x) == 1) {
            if (!is.null(attr(x, "expressions"))) 
                val <- attr(x, "expressions")[[names(x)]] else val <- names(x)
            
        } else {
            message("NGRAM warning:  td.data.frame 'x' has length > 1 using first element")
            val <- names(x)[1]
        }
        
        return(asTdExpr(gettextf(ofmt, val, second_string, gram_length)))
        
    }
    
    if (inherits(x, "character") || inherits(x, "td.expression")) {
        return(asTdExpr(paste("NGRAM(", x, ",", second_string, ",", gram_length, ")", sep = "")))
    }
} 
