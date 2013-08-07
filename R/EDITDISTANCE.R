EDITDISTANCE <- function(x, y) {
    if (inherits(x, "td.data.frame")) {
        if (length(x) > 1) 
            message("EDITDISTANCE warning:  td.data.frame 'x' has length > 1 using first element")
        
        xval <- .td.gencolumnexpr(x[1])
    } else if (inherits(x, "td.expression")) 
        xval <- x else if (inherits(x, "character")) 
        xval <- paste("'", x, "'", sep = "") else stop("Invalid data type for 'x' in EDITDISTANCE function")
    
    if (inherits(y, "td.data.frame")) {
        if (length(y) > 1) 
            message("EDITDISTANCE warning:  td.data.frame 'y' has length > 1 using first element")
        
        yval <- .td.gencolumnexpr(y[1])
    } else if (inherits(y, "td.expression")) 
        yval <- y else if (inherits(y, "character")) 
        yval <- paste("'", y, "'", sep = "") else stop("Invalid data type for 'y' in EDITDISTANCE function")
    
    
    val <- paste("EDITDISTANCE(", xval, ",", yval, ")", sep = "")
    
    class(val) <- "td.expression"
    return(val)
}

# EDITDISTANCE <- function(x, second_string=' ') { asTdExpr <- function(x) {class(x) <- 'td.expression'; return(x)}
# efmt <- 'EDITDISTANCE(%s,%s)' if(inherits(x, 'td.data.frame')) { if(length(x) == 1) { if(!is.null(attr(x,
# 'expressions'))) val <- attr(x, 'expressions')[[names(x)]] else val <- names(x) } else { message('EDITDISTANCE
# warning: td.data.frame 'x' has length > 1 using first element') val <- names(x)[1] } return(asTdExpr(gettextf(efmt,
# val, second_string))) } if(inherits(x, 'character') || inherits(x,'td.expression')) {
# return(asTdExpr(paste('EDITDISTANCE(', x, ',', second_string, ')', sep=''))) } } 
