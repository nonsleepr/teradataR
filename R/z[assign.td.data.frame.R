`[<-.td.data.frame` <- function(x, i, j, value) {
    if (!all(names(sys.call()) %in% c("", "value"))) 
        warning("named arguments are discouraged")
    nA <- nargs()
    if (nA == 4L) {
        has.i <- !missing(i)
        has.j <- !missing(j)
    } else if (nA == 3L) {
        if (is.atomic(value)) 
            names(value) <- NULL
        if (missing(i) && missing(j)) {
            i <- j <- NULL
            has.i <- has.j <- FALSE
            if (is.null(value)) 
                return(x[logical()])
        } else {
            if (is.logical(i) && is.matrix(i) && all(dim(i) == dim(x))) {
                stop("Logical matrix not alowed for td.data.frame assignment")
                return(x)
            }
            if (is.matrix(i)) 
                stop("only logical matrix subscripts are allowed in replacement")
            j <- i
            i <- NULL
            has.i <- FALSE
            has.j <- TRUE
        }
    } else {
        stop("need 0, 1, or 2 subscripts")
    }
    if (has.j && length(j) == 0L) 
        return(x)
    cl <- oldClass(x)
    class(x) <- NULL
    new.cols <- NULL
    nvars <- length(x)
    nrows <- .row_names_info(x, 2L)
    if (has.i) {
        rows <- NULL
        if (any(is.na(i))) 
            stop("missing values are not allowed in subscripted assignments of data frames")
        if (char.i <- is.character(i)) {
            rows <- attr(x, "row.names")
            ii <- match(i, rows)
            nextra <- sum(new.rows <- is.na(ii))
            if (nextra > 0L) {
                ii[new.rows] <- seq.int(from = nrows + 1L, length.out = nextra)
                new.rows <- i[new.rows]
            }
            i <- ii
        }
        if (all(i >= 0L) && (nn <- max(i)) > nrows) {
            if (is.null(rows)) 
                rows <- attr(x, "row.names")
            if (!char.i) {
                nrr <- (nrows + 1L):nn
                if (inherits(value, "data.frame") && (dim(value)[1L]) >= length(nrr)) {
                  new.rows <- attr(value, "row.names")[seq_along(nrr)]
                  repl <- duplicated(new.rows) | match(new.rows, rows, 0L)
                  if (any(repl)) 
                    new.rows[repl] <- nrr[repl]
                } else new.rows <- nrr
            }
            x <- xpdrows.data.frame(x, rows, new.rows)
            rows <- attr(x, "row.names")
            nrows <- length(rows)
        }
        iseq <- seq_len(nrows)[i]
        if (any(is.na(iseq))) 
            stop("non-existent rows not allowed")
    } else iseq <- NULL
    if (has.j) {
        if (any(is.na(j))) 
            stop("missing values are not allowed in subscripted assignments of data frames")
        if (is.character(j)) {
            if ("" %in% j) 
                stop("column name \"\" cannot match any column")
            jj <- match(j, names(x))
            nnew <- sum(is.na(jj))
            if (nnew > 0L) {
                n <- is.na(jj)
                jj[n] <- nvars + seq_len(nnew)
                new.cols <- j[n]
            }
            jseq <- jj
        } else if (is.logical(j) || min(j) < 0L) 
            jseq <- seq_along(x)[j] else {
            jseq <- j
            if (max(jseq) > nvars) {
                new.cols <- paste("V", seq.int(from = nvars + 1L, to = max(jseq)), sep = "")
                if (length(new.cols) != sum(jseq > nvars)) 
                  stop("new columns would leave holes after existing columns")
                if (is.list(value) && !is.null(vnm <- names(value))) {
                  p <- length(jseq)
                  if (length(vnm) < p) 
                    vnm <- rep(vnm, length.out = p)
                  new.cols <- vnm[jseq > nvars]
                }
            }
        }
    } else jseq <- seq_along(x)
    if (anyDuplicated(jseq)) 
        stop("duplicate subscripts for columns")
    n <- length(iseq)
    if (n == 0L) 
        n <- nrows
    p <- length(jseq)
    m <- length(value)
    # if (is.td.expression(value)) { nm <- names(x) rows <- .row_names_info(x, 0L) a <- attributes(x) a['names'] <- NULL x
    # <- c(x, vector('list', length(new.cols))) attributes(x) <- a names(x) <- c(nm, new.cols) attr(x, 'row.names') <- rows
    # expr <- value names(expr) <- new.cols if(is.null(attr(x, 'expressions'))) attr(x, 'expressions') <- expr else attr(x,
    # 'expressions') <- c(attr(x, 'expressions'), expr) class(x) <- cl return(x) }
    if (!is.list(value)) {
        if (p == 1L) {
            # N <- NROW(value) if (N > n) stop(gettextf('replacement has %d rows, data has %d', N, n), domain = NA) if (N < n && N
            # > 0L) if (n%%N == 0L && length(dim(value)) <= 1L) value <- rep(value, length.out = n) else stop(gettextf('replacement
            # has %d rows, data has %d', N, n), domain = NA)
            names(value) <- NULL
            value <- list(value)
        } else {
            # if (m < n * p && (m == 0L || (n * p)%%m)) stop(gettextf('replacement has %d items, need %d', m, n * p), domain = NA)
            value <- matrix(value, max(n, 1), p)
            value <- split(value, col(value))
        }
        dimv <- c(n, p)
    } else {
        value <- unclass(value)
        lens <- vapply(value, NROW, 1L)
        for (k in seq_along(lens)) {
            N <- lens[k]
            if (n != N && length(dim(value[[k]])) == 2L) 
                stop(gettextf("replacement element %d is a matrix/data frame of %d rows, need %d", k, N, n), domain = NA)
            if (N > 0L && N < n && n%%N) 
                stop(gettextf("replacement element %d has %d rows, need %d", k, N, n), domain = NA)
            if (N > 0L && N < n) 
                value[[k]] <- rep(value[[k]], length.out = n)
            if (N > n) {
                warning(gettextf("replacement element %d has %d rows to replace %d rows", k, N, n), domain = NA)
                value[[k]] <- value[[k]][seq_len(n)]
            }
        }
        dimv <- c(n, length(value))
    }
    nrowv <- dimv[1L]
    # if (nrowv < n && nrowv > 0L) { if (n%%nrowv == 0L) value <- value[rep(seq_len(nrowv), length.out = n), , drop =
    # FALSE] else stop(gettextf('%d rows in value to replace %d rows', nrowv, n), domain = NA) } else if (nrowv > n)
    # warning(gettextf('replacement data has %d rows to replace %d rows', nrowv, n), domain = NA)
    ncolv <- dimv[2L]
    jvseq <- seq_len(p)
    if (ncolv < p) 
        jvseq <- rep(seq_len(ncolv), length.out = p) else if (ncolv > p) {
        warning(gettextf("provided %d variables to replace %d variables", ncolv, p), domain = NA)
        new.cols <- new.cols[seq_len(p)]
    }
    if (length(new.cols)) {
        nm <- names(x)
        rows <- .row_names_info(x, 0L)
        a <- attributes(x)
        a["names"] <- NULL
        x <- c(x, vector("list", length(new.cols)))
        attributes(x) <- a
        names(x) <- c(nm, new.cols)
        attr(x, "row.names") <- rows
    }
    if (has.i) 
        for (jjj in seq_len(p)) {
            jj <- jseq[jjj]
            vjj <- value[[jvseq[[jjj]]]]
            if (jj <= nvars) {
                if (length(dim(x[[jj]])) != 2L) 
                  x[[jj]][iseq] <- vjj else x[[jj]][iseq, ] <- vjj
            } else {
                x[[jj]] <- vjj[FALSE]
                if (length(dim(vjj)) == 2L) {
                  length(x[[j]]) <- nrows * ncol(vjj)
                  dim(x[[j]]) <- c(nrows, ncol(vjj))
                  x[[jj]][iseq, ] <- vjj
                } else {
                  length(x[[j]]) <- nrows
                  x[[jj]][iseq] <- vjj
                }
            }
        } else if (p > 0L) 
        for (jjj in p:1L) {
            o <- order(jseq)
            jseq <- jseq[o]
            jvseq <- jvseq[o]
            jj <- jseq[jjj]
            # v <- value[[jvseq[[jjj]]]]
            expr <- unclass(value[[jvseq[[jjj]]]])
            names(expr) <- jj
            if (is.null(attr(x, "expressions"))) 
                attr(x, "expressions") <- c(expr) else {
                if (jj %in% names(attr(x, "expressions"))) 
                  attr(x, "expressions")[which(jj == names(attr(x, "expressions")))] <- expr[[1]] else attr(x, "expressions") <- c(expr, attr(x, "expressions"))
            }
            
            # if (nrows > 0L && !length(v)) length(v) <- nrows x[[jj]] <- v if (!is.null(v) && is.atomic(x[[jj]])) names(x[[jj]])
            # <- NULL
        }
    if (length(new.cols) > 0L) {
        new.cols <- names(x)
        if (anyDuplicated(new.cols)) 
            names(x) <- make.unique(new.cols)
    }
    class(x) <- cl
    x
} 
