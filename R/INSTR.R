INSTR <- function(x, y) {
    # check type of x 
    if (inherits(x, "td.data.frame")) {
        if (length(x) > 1) 
            message("INSTR warning:  td.data.frame 'x' has length > 1 using first element")
        
        xval <- .td.gencolumnexpr(x[1])
    } else if (inherits(x, "td.expression")) 
        xval <- x else if (inherits(x, "character")) 
        xval <- paste("'", x, "'", sep = "") else stop("Invalid data type for 'x' in INSTR function")
    
    # check type of y
    if (inherits(y, "td.data.frame")) {
        if (length(y) > 1) 
            message("INSTR warning:  td.data.frame 'y' has length > 1 using first element")
        
        yval <- .td.gencolumnexpr(y[1])
    } else if (inherits(y, "td.expression")) 
        yval <- y else if (inherits(y, "character")) 
        yval <- paste("'", y, "'", sep = "") else stop("Invalid data type for 'y' in INSTR function")
    
    
    val <- paste("INSTR(", xval, ",", yval, ")", sep = "")
    
    class(val) <- "td.expression"
    return(val)
}