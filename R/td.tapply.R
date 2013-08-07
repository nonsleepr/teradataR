td.tapply <- function(X, INDEX, FUN = NULL, asdf = FALSE, ...) {
    if (!inherits(X, "td.data.frame")) 
        stop("'X' is not a td data frame")
    if (!inherits(INDEX, "td.data.frame")) 
        stop("'INDEX' is not a td data frame")
    
    if (!is.null(FUN)) {
        fnc <- character(0)
        
        if (identical(sum, FUN) || (is.character(FUN) && FUN == "sum")) 
            fnc <- "sum"
        if (identical(min, FUN) || (is.character(FUN) && FUN == "min")) 
            fnc <- "min"
        if (identical(max, FUN) || (is.character(FUN) && FUN == "max")) 
            fnc <- "max"
        if (identical(mean, FUN) || (is.character(FUN) && FUN == "mean")) 
            fnc <- "mean"
        
        if (identical(td.stats, FUN) || (is.character(FUN) && FUN == "td.stats")) {
            if (asdf) 
                return(td.stats(X, group.by = INDEX))
            
            return(.td.dataframe2array(td.stats(X, group.by = INDEX), INDEX))
        }
        
        if (identical(td.values, FUN) || (is.character(FUN) && FUN == "td.values")) {
            if (asdf) 
                return(td.values(X, group.by = INDEX))
            
            return(.td.dataframe2array(td.values(X, group.by = INDEX), INDEX))
        }
        
        if (length(fnc)) {
            fmt <- "SELECT %s,%s FROM %s GROUP BY %s ORDER BY %s"
            if (length(INDEX) > 2) 
                srt <- paste("\"", names(INDEX)[2:length(INDEX)], "\"", sep = "", collapse = ",") else srt <- paste("\"", names(INDEX), "\"", sep = "", collapse = ",")
            nms <- paste("\"", names(INDEX), "\"", sep = "", collapse = ",")
            obj <- .td.object(attr(X, "tableName"), attr(X, "database"))
            query <- gettextf(fmt, nms, .td.genstats(names(X)[1], fnc), obj, nms, srt)
            res <- tdQuery(query)
            if (asdf) 
                return(res)
            return(.td.dataframe2array(res, INDEX))
            # vals <- list() dms <- integer(0) for(i in 1:(length(res)-1)) { vals[[i]] <- as.vector(sort(unique(res[[i]]))) dms[i]
            # <- length(vals[[i]]) } names(vals) <- names(INDEX) arr <- array(dim = dms, dimnames=vals) for(i in 1:nrow(res)) { mat
            # <- matrix(as.character(res[i,1:length(dms)]), nrow=1) arr[mat] <- res[i,length(res)] }
            
            # return(arr)
        }
    }
} 
