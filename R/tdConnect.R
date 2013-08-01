tdConnect <-
function (dsn, uid = "", pwd = "", database = "", dType = "odbc")
{
    if(dType == "odbc")
    {
      require(RODBC)
      st <- paste("DSN=", dsn, sep = "")
      if (nchar(uid))
          st <- paste(st, ";UID=", uid, sep = "")
      if (nchar(pwd))
          st <- paste(st, ";PWD=", pwd, sep = "")
      if (nchar(database))
          st <- paste(st, ";Database=", database, sep = "")
      tdConnection <- odbcDriverConnect(st)
      assign("tdConnection", tdConnection, envir=.GlobalEnv)
      invisible(tdConnection)
    }

    if(dType == "jdbc")
    {
      require(RJDBC)
      drv <- JDBC("com.teradata.jdbc.TeraDriver")
      st <- paste("jdbc:teradata://", dsn, sep = "")
      if (nchar(database))
          st <- paste(st, "/database=", database, sep = "")
      tdConnection <- dbConnect(drv, st, user=uid, password=pwd)
      assign("tdConnection", tdConnection, envir=.GlobalEnv)
      invisible(tdConnection)
    }
}

