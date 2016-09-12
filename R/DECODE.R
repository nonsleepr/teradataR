DECODE <- function(x, default=NULL, ...) {
    simplePaste <- function(i) {
      if(inherits(i, "numeric")) {
        res <- as.numeric(paste(i))
      }
      else {
        res <- paste("'",i,"'", collapse=none)
        print(nchar(res, type="chars"))
      }
      return(res)
    }
    params <- list(...)
    #print(parms[[2]])
    #plist <- unlist(parms)
    #return("DECODE(c1, 1, \'A\', 2,\'B\')" )
    res <- lapply(params, simplePaste)
    res <- paste(res, collapse=",")
    print(res)
    if (inherits(x, "td.data.frame")) {
        if (length(x) > 1) {
            message("DECODE warning:  td.data.frame 'x' has length > 1 using first element")
        }
        
       
        #print(res)
        val <- paste("DECODE(", .td.gencolumnexpr(x[1]), ",", res, ")", sep = "")
    } 
   else if (inherits(x, "td.expression") || inherits(x, "numeric")) {
        val <- paste("DECODE(", x, ",", res, ")", sep = "") 
   }
   else if (inherits(x, "character")) {
        val <- paste("DECODE('", x, "'", ",", lapply(params, simplePaste), ")", sep = "") 
   }    
   else stop("Invalid data type for 'x' in DECODE function")
   
   class(val) <- "td.expression"
   return(val)
}

# DECODE <- function(x, ...) { parms <- list(...)  for(i in 1:length(parms)) { if(is.character(parms[[i]])) parms[[i]]
# <- paste(''', parms[[i]], ''', sep='') } plist <- paste(parms, collapse=',') if(inherits(x, 'td.data.frame')) {
# if(length(x) == 1) { if(!is.null(attr(x, 'expressions')) && names(x) %in% names(attr(x,'expressions'))) { val <-
# paste('DECODE(', attr(x, 'expressions')[[names(x)]], ',', plist, ')', sep='') class(val) <- 'td.expression'
# return(val) } else { val <- paste('DECODE(\'', names(x), '\',', plist, ')', sep='') class(val) <- 'td.expression'
# return(val) } } else { message('DECODE warning: td.data.frame 'x' has length > 1 using first element') val <-
# paste('DECODE(\'', names(x)[1], ',', plist, '\')', sep='') class(val) <- 'td.expression' return(val) } }
# if(inherits(x, 'character') || inherits(x,'td.expression')) { val <- paste('DECODE(', x, ',', plist, ')', sep='')
# class(val) <- 'td.expression' return(val) } } 
