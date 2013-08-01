as.data.frame.td.data.frame <-
function(x, size = 100)
{
    if(!length(names(x)))
      stop("td.data.frame contains no columns")
    query <- paste(.td.tdf2sql(x), "SAMPLE", size)
    return(tdQuery(query))
}

