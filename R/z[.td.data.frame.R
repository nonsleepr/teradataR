`[.td.data.frame` <- function(x, i, j) {
    Nargs <- nargs()
    if (missing(i) && (missing(j) || Nargs == 2L)) 
        return(x)
    if (Nargs == 2L || missing(i)) {
        cols <- if (missing(i)) 
            j else i
        if (!is.numeric(cols) && !is.character(cols) && !is.logical(cols)) 
            stop(gettextf("Invalid subscript type '%s'", typeof(cols)))
        if (is.character(cols)) 
            cols <- match(make.unique(cols), names(x))
        if (is.logical(cols)) 
            cols <- match(names(x)[rep(cols, length.out = length(names(x)))], names(x))
        if (any(is.na(cols)) || !all(cols <= length(x))) 
            stop("Undefined columns selected")
        if (any(cols < 0) && any(cols > 0)) 
            stop("only 0's may be mixed with negative subscripts")
        if (any(cols < 0)) 
            cols <- (1:length(x))[cols]
        a <- attributes(x)
        a$names <- NULL
        y <- NextMethod("[")
        if (any(duplicated(names(y)))) {
            y <- y[which(!duplicated(names(y)))]
            cols <- unique(cols)
        }
        a$names <- names(y)
        attributes(y) <- a
        # y <- x rc <- length(x):1 rc <- (length(x):(length(unique(cols))+1)) for(col in rc) { if(!(col %in% cols)) y[col] <-
        # NULL } names(y) <- names(x)[cols]
        attr(y, "expressions") <- NULL
        # exprs <- attr(x, 'expressions')[which(names(y) %in% names(attr(x,'expressions')))]
        exprs <- attr(x, "expressions")[as.character(cols[which(cols %in% names(attr(x, "expressions")))])]
        if (!is.null(exprs) && !any(is.na(exprs)) && length(exprs) > 0L) {
            names(exprs) <- which(cols %in% names(exprs))
            attr(y, "expressions") <- c(exprs)
        }
        return(y)
    }
    
    if (missing(j)) {
        if (is.character(i)) 
            stop("Undefined columns selected")
        if (is.numeric(i)) {
            warning("td data frame unchanged.  Use td.sample if you wish to reduce row size for tables.")
            return(x)
        }
        stop(gettextf("Invalid subscript type '%s'", typeof(i)))
    }
    
    warning("td data frame unchanged.  Use td.sample if you wish to reduce row size for tables.")
    return(x)
}
 
