dim.td.data.frame <- function(x) {
    rows <- attr(x, "totalRows")
    c(rows, length(x))
}
 
