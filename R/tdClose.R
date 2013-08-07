tdClose <- function(x = tdConnection) {
    if (class(tdConnection) == "RODBC") 
        close(x)
    if (class(tdConnection) == "JDBCConnection") 
        dbDisconnect(x)
}
 
