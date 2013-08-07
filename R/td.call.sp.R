td.call.sp <- function() {
    maxDisplay <- 10
    currentIndex <- 1
    df <- tdQuery(paste("SELECT CAST(databasename AS VARCHAR(30)),CAST(tablename AS VARCHAR(30)) FROM DBC.TABLESX WHERE TableKind = 'P' ", 
        "ORDER BY LastAlterTimeStamp DESC"))
    if (nrow(df) < maxDisplay) 
        maxDisplay <- nrow(df)
    done <- FALSE
    while (!done) {
        cat("\n")
        for (i in currentIndex:(currentIndex + maxDisplay - 1)) cat(gettextf("%d) %s.%s\n", i, df[i, 1], df[i, 2]))
        cat(gettextf("\nShowing %d - %d of %d\n", currentIndex, currentIndex + maxDisplay - 1, nrow(df)))
        cat(gettext("Enter # to run stored procedure, (n)ext, (p)revious, or (q)uit: "))
        ans <- readLines(, 1)
        
        if (ans == "q") 
            break
        if (ans == "n") {
            currentIndex <- currentIndex + maxDisplay
            if (currentIndex > nrow(df)) 
                currentIndex <- 1
        }
        if (ans == "p") {
            currentIndex <- currentIndex - maxDisplay
            if (currentIndex < 1) 
                currentIndex <- 1
        }
        
        old.warn <- getOption("warn")
        options(warn = -1)
        idx <- as.numeric(ans)
        options(warn = old.warn)
        if (!is.na(idx) && (idx >= currentIndex && idx <= (currentIndex + maxDisplay - 1))) {
            obj <- .td.object(df[idx, 2], df[idx, 1])
            qry <- gettextf(paste("SELECT CAST(ColumnName AS VARCHAR(30)), CAST(ColumnType AS VARCHAR(10)),CommentString FROM DBC.COLUMNSX ", 
                "WHERE DatabaseName = '%s' and TableName = '%s' ORDER BY ColumnId", sep = ""), df[idx, 1], df[idx, 2])
            
            df2 <- tdQuery(qry)
            parms <- character(0)
            ptypes <- character(0)
            for (i in 1:nrow(df2)) {
                if (is.na(df2[i, 3]) || !nchar(df2[i, 3])) 
                  cat(gettextf("Enter value for %s: ", df2[i, 1])) else cat(gettextf("Enter value for %s(%s): ", df2[i, 1], df2[i, 3]))
                ans <- readLines(, 1)
                parms[length(parms) + 1] <- ans
                ptypes[length(ptypes) + 1] <- as.character(df2[i, 2])
            }
            td.numeric <- c("I", "F", "D", "I1", "I2")
            
            for (i in 1:length(parms)) if (!ptypes[i] %in% td.numeric) 
                parms[i] <- gettextf("'%s'", parms[i])
            plist <- paste(parms, collapse = ",")
            dfsp <- try(tdQuery(gettextf("CALL %s(%s)", obj, plist)))
            invisible(dfsp)
        }
    }
}
 
