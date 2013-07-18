tdQuery <-
function(q, ...)
{
  if(class(tdConnection) == "RODBC")
    return(sqlQuery(tdConnection, q, ...))
  if(class(tdConnection) == "JDBCConnection")
    return(dbGetQuery(tdConnection, q, ...))
}

