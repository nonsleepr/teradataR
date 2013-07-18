tdMetadataDB <-
function (database)
{
    if(!is.null(database) && nchar(database))
    {
      assign("tdMetadata", database, envir=.GlobalEnv)
      invisible(tdMetadata)
    }
    else
    {
      if(exists("tdMetadata", envir=.GlobalEnv))
        remove(tdMetadata, envir=.GlobalEnv)
      invisible(NA)
    }
}

