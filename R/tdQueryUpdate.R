tdQueryUpdate <-
function(q, ...)
{
  if(class(tdConnection) == "RODBC")
    return(sqlQuery(tdConnection, q, ...))
  if(class(tdConnection) == "JDBCConnection")
  {
    dbSendUpdate(tdConnection, q, ...)
    return("No Data")
  }
}
