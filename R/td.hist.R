td.hist <- function(tdf, col, breaks = 10, plot = TRUE, ...) {
    if (!is.td.data.frame(tdf)) 
        stop("'tdf' is not a td data frame")
    if (missing(col)) {
        nm <- names(tdf)[1]
        col <- .td.gencolumnexpr(tdf[names(tdf)[1]])
    } else {
        nm <- col
        col <- .td.gencolumnexpr(tdf[col])
    }
    
    query <- .td.genhist(tdf, col, breaks)
    df <- try(tdQuery(query))
    if (is.null(attr(df, "class"))) 
        stop("'col' must be a numeric column")
    minBin <- df$xbin[1]
    maxBin <- df$xbin[nrow(df)]
    wid <- df$xend[1] - df$xbeg[1]
    brks <- seq(df$xbeg[1], by = wid, length.out = maxBin + 1)
    cnts <- rep(0, maxBin - 1)
    for (i in 1:nrow(df)) cnts[df$xbin[i]] <- df$xcnt[i]
    dens <- cnts/sum(cnts)
    myHist <- list(brks, cnts, dens, dens)
    names(myHist) <- c("breaks", "counts", "intensities", "density")
    attr(myHist, "class") <- "histogram"
    if (plot) 
        try(plot(myHist, main = paste("Histogram of", col), col = "orange", ...)) else return(myHist)
}
 
