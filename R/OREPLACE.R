OREPLACE <- function(x, search_char, replace_char) {
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    
    rfmt <- "OREPLACE(%s, %s, %s)"
    if (inherits(x, "td.data.frame")) {
        if (length(x) == 1 || length(y) == 1) {
            if (!is.null(attr(x, "expressions")) || !is.null(attr(search_char, "expressions")) || !is.null(attr(replace_char, "expressions"))) { 
                val1 <- attr(x, "expressions")[[names(x)]]
                val2 <- attr(search_char, "expressions")[[names(search_char)]]
                val3 <- attr(replace_char, "expressions")[[names(replace_char)]]
            }
            else {
              val1 <- names(x)
              val2 <- names(search_char)
              val3 <- names(replace_char)
            }
        } 
        else {
            message("OREPLACE warning:  td.data.frame 'x' or 'search_string' or 'replace_string' has length > 1 using first element")
            val1 <- names(x)[1]
            val2 <- names(search_char)[1]
            val3 <- names(replace_char)[1]
        }
        
        return(asTdExpr(gettextf(rfmt, val1, val2, val3)))
        
    }
    
    if (inherits(x, "character") || inherits(x, "td.expression")) {
        return(asTdExpr(paste("OREPLACE(", val1, ", ", val2, ", ", val3, ")", sep = "")))
    }
} 
