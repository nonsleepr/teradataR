OTRANSLATE <- function(x, search_char, replace_char) {
    asTdExpr <- function(x) {
        class(x) <- "td.expression"
        return(x)
    }
    
    ofmt <- "OTRANSLATE(%s,%s,%s)"
    if (inherits(x, "td.data.frame") && inherits(search_char, "td.data.frame") && inherits(replace_char, "td.data.frame")) {
        if (length(x) == 1 && length(search_char) == 1 && length(replace_char) == 1) {
            if (!is.null(attr(x, "expressions")) || !is.null(attr(search_char, "expressions")) || !is.null(attr(replace_char, "expressions"))) {
                val1 <- attr(x, "expressions")[[names(x)]] 
                val2 <- attr(search_char, "expressions")[[names(search_char)]]
                val3 <- attr(replace_char, "expressions")[[names(replace_char)]]
            }
            else {
              val1 <- names(x)
              val2 <- names(search_char)
              val3 <- names(replace_char)
          } 
        }
    }
    else {
      message("OTRANSLATE warning:  td.data.frame 'x' or 'search_char' or 'replace_char' has length > 1 using first element")
      val1 <- names(x)[1]
      val2 <- names(search_char)[1]
      val3 <- names(replace_char)[1]
    }

        return(asTdExpr(gettextf(ofmt, val1, val2, val3)))
    
    if (inherits(x, "character") || inherits(x, "td.expression") || inherits(search_char, "character") ||
        inherits(search_char,"td.expression") || inherits(replace_char, "character") || inherits(replace_char, "td.expression")) {
        return(asTdExpr(paste("OTRANSLATE(", x, ",", search_char, ",", replace_char, ")", sep = "")))
    }
} 
