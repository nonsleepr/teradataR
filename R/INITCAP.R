INITCAP <- function(x) {
  #handles condition in which x is a td data frame  
  if (inherits(x, "td.data.frame")) {
        if (length(x) > 1) 
            message("INITCAP warning:  td.data.frame 'x' has length > 1 using first element")
        
        #sets up query expression
        val <- paste("INITCAP(", .td.gencolumnexpr(x[1]), ")", sep = "")
   #handles condition in which x is a td expression or numeric 
   }
   else if (inherits(x, "td.expression") || inherits(x, "numeric") || inherits(x, "character")) 
        val <- paste("INITCAP(", x, ")", sep = "") 
    else stop("Invalid data type for 'x' in INITCAP function")
    
    class(val) <- "td.expression"
    return(val)
}