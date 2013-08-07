td.overlap <- function(tdf1, tdf2, fields1, fields2) {
    if (missing(fields1)) 
        fields1 <- .td.getPrimaryIndicies(tdf1)
    if (missing(fields2)) 
        fields2 <- .td.getPrimaryIndicies(tdf2)
    query <- .td.genoverlap(tdf1, tdf2, fields1, fields2)
    df <- try(tdQuery(query))
    if (class(df) == "data.frame") 
        return(df) else stop(gettextf("Error: %s", paste(df[1], collapse = "")))
}
 
