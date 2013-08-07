summary.td.data.frame <- function(tdf, maxsum = 7, digits = max(3, getOption("digits") - 3), ...) {
    if (!is.td.data.frame(tdf)) 
        stop("'tdf' is not a td data frame")
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    lst <- list()
    
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" WHERE ", attr(tdf, "whereClause"))
    
    for (i in 1:length(names(tdf))) {
        cols <- paste(.td.genstats(names(tdf)[i], c("min", "max", "mean", "cnt")), collapse = ",")
        cnull <- .td.genvalues(names(tdf)[i])
        cols <- paste(cols, ",", cnull[1])
        query <- gettextf("SELECT %s FROM %s %s", paste(cols, collapse = ","), obj, wc)
        mmmn <- try(tdQuery(query))
        if (is.data.frame(mmmn)) {
            # query <- .td.genmedian(tdf,names(tdf)[i]) med <- tdQuery(query)
            query <- .td.genquartiles(tdf, names(tdf)[i], mmmn[[4]])
            quar <- tdQuery(query)
            vals <- c(mmmn[[1]], quar[[1]], quar[[3]], mmmn[[3]], quar[[2]], mmmn[[2]], if (mmmn[[5]] > 0) mmmn[[5]] else double(0))
            names(vals) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", if (mmmn[[5]] > 0) "NA's" else character(0))
        } else {
            vals <- c()
            names(vals) <- c()
            amt <- maxsum
            query <- gettextf("SELECT \"%s\", COUNT(*) as cnt FROM %s %s GROUP BY 1 ORDER BY 2 DESC", names(tdf)[i], obj, 
                wc)
            charvals <- tdQuery(query)
            v <- charvals[[1]]
            cs <- charvals[[2]]
            
            if (any(is.na(v))) {
                vals <- c(sum(cs[is.na(v)]))
                names(vals) <- "NA's"
                amt <- amt - 1
                nullIndex = which(is.na(v))
                v <- v[-nullIndex]
                cs <- cs[-nullIndex]
                # v <- v[-(length(v))] cs <- cs[-(length(cs))]
            }
            if (length(v) > amt) {
                amt <- amt - 1
                vals <- c(sum(cs[(amt + 1):length(v)]), vals)
                names(vals)[1] <- "(OTHER)"
            }
            if (amt > length(v)) 
                amt = length(v)
            prevnm <- names(vals)
            vals <- c(cs[1:amt], vals)
            names(vals) <- c(as.character(v[1:amt]), prevnm)
        }
        lst[[length(lst) + 1]] <- vals
    }
    names(lst) <- names(tdf)
    nv <- length(tdf)
    nm <- names(tdf)
    lw <- numeric(nv)
    nr <- max(unlist(lapply(lst, NROW)))
    for (i in 1L:nv) {
        sms <- lst[[i]]
        lbs <- format(names(sms))
        sms <- paste(lbs, ":", format(sms, digits = digits), "  ", sep = "")
        lw[i] <- nchar(lbs[1L], type = "w")
        length(sms) <- nr
        lst[[i]] <- sms
    }
    z <- unlist(lst, use.names = TRUE)
    dim(z) <- c(nr, nv)
    blanks <- paste(character(max(lw) + 2L), collapse = " ")
    pad <- floor(lw - nchar(nm, type = "w")/2)
    nm <- paste(substring(blanks, 1, pad), nm, sep = "")
    dimnames(z) <- list(rep.int("", nr), nm)
    attr(z, "class") <- c("table")
    z
}
 
