LPAD <- function(x, ilength, fill_string = " ") {
    #helper function to make sure input value is of correct type
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    
    lfmt <- "LPAD(\"%s\",%d,\'%s\')"
    #handles conditions in which x is a td data frame
    if (inherits(x, "td.data.frame")) {
        if (length(x) == 1) {
            if (!is.null(attr(x, "expressions"))) 
                val <- attr(x, "expressions")[[names(x)]] else val <- names(x)
            
        } else {
            message("LPAD warning:  td.data.frame 'x' has length > 1 using first element")
            val <- names(x)[1]
        }
        
        return(asTdExpr(gettextf(lfmt, val, ilength, fill_string)))
        
    }
    #handles conditions in which x is a character or td expression
    if (inherits(x, "character") || inherits(x, "td.expression")) {
        return(asTdExpr(paste("LPAD(\"", x, "\",", ilength, ",", fill_string, ")", sep = " ")))
    }
} 
