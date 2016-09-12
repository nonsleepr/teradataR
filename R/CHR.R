CHR <- function(x) {
    if (inherits(x, "td.data.frame")) {
        if (length(x) > 1) 
            message("CHR warning:  td.data.frame 'x' has length > 1 using first element")
        
        val <- paste("CHR(", .td.gencolumnexpr(x[1]), ")", sep = "")
    } else if (inherits(x, "td.expression") || inherits(x, "numeric")) 
        val <- paste("CHR(", x, ")", sep = "") else if (inherits(x, "character")) 
        val <- paste("CHR('", x, "')", sep = "") else stop("Invalid data type for 'x' in CHR function")
    
    class(val) <- "td.expression"
    return(val)
}
