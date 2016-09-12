LTRIM <- function(x) {
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    
    lfmt <- "LTRIM(%s)"

        if (length(x) == 1) {
            if (!is.null(attr(x, "expressions"))) 
                val <- attr(x, "expressions")[[names(x)]] else val <- names(x)
            
        } else {
            message("LTRIM warning:  td.data.frame 'x' has length > 1 using first element")
            val <- names(x)[1]
        }
            return(asTdExpr(gettextf(lfmt, val)))
}
