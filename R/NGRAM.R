NGRAM <- function(x, y, gram_length) {
    #helper function acts as a setter for class td.expression
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    #set up base text
    ofmt <- "NGRAM(%s,%s,%d)"
    #determine datatype of parameters
    if (inherits(x, "td.data.frame") || inherits(y, "td.data.frame")) {
        if (length(x) == 1 && length(y) == 1) {
            if (!is.null(attr(x, "expressions")) && (!is.null(attr(y, "expressions")))) {
                val1 <- attr(x, "expressions")[[names(x)]] 
                val2 <- attr(y, "expressions")[[names(y)]]
            }
            else {
                val1 <- names(x)
                val2 <- names(y)
            }
        }
        else {
            message("NGRAM warning:  td.data.frame 'x' or 'y' has length > 1 using first element")
            val1 <- names(x)[1]
            val2 <- names(y)[1]
        }
        return(asTdExpr(gettextf(ofmt, val1, val2, gram_length)))
        
    }
    #check for other datatypes
    if (inherits(x, "character") || inherits(x, "td.expression") || inherits(y, "character") || inherits(y, "td.expression")) {
        return(asTdExpr(paste("NGRAM(", x, ",",y, ",", gram_length, ")", sep = "")))
    }
} 
