tdSave <- function(x, tablename = "") {
    if (inherits(x, "td.data.frame")) 
        return(x)
    if (inherits(x, "data.frame")) {
        if (nchar(tablename) > 0) 
            tbl <- tablename else tbl <- deparse(substitute(x))
        if (class(tdConnection) == "RODBC") {
            sqlSave(tdConnection, x, tablename = tbl)
            return(td.data.frame(tbl))
        }
        
        if (class(tdConnection) == "JDBCConnection") {
            dbWriteTable(tdConnection, name = tbl, value = x)
            return(td.data.frame(tbl))
        }
    }
}
 
