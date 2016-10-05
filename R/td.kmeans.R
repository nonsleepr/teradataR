td.kmeans <- function(x, centers, iter.max = 10, nstart = 1) {
    if (!is.td.data.frame(x)) 
        stop("'x' is not a td data frame")
    if (length(centers) != 1L) 
        stop("Only single numeric value allowed for 'centers'")
    if (!length(names(x))) 
        stop("td.data.frame contains no columns")
    obj <- .td.object(attr(x, "tableName"), attr(x, "database"))
    wc <- ""
    if (!is.null(attr(x, "whereClause"))) 
        wc <- paste(" WHERE ", attr(x, "whereClause"))
    ac <- ""
    if (!is.null(attr(x, "whereClause"))) 
        ac <- paste(" AND ", attr(x, "whereClause"))
    
    nms <- paste(gettextf("\"%s\"", names(x)), collapse = ",")
    maxD <- 0
    for (i in 1:nstart) {
        testClusters <- tdQuery(gettextf("SELECT DISTINCT %s FROM %s %s SAMPLE %d", nms, obj, wc, centers))
        curD <- 0
        for (j in 1:centers - 1) {
            for (k in (j + 1):centers) curD <- curD + sum(dist(testClusters[c(j, k), ]))
        }
        if (curD > maxD) {
            maxD <- curD
            initClusters <- testClusters
        }
    }
    if (centers == 1L || nrow(initClusters) < centers) 
        iter <- iter.max + 1 else iter <- 1
    
    while (iter <= iter.max) {
        caseText <- .td.genclusterid(initClusters)
        sText <- paste(gettextf("SUM(CAST(\"%s\" AS FLOAT))/COUNT(*) AS \"%s\"", names(x), names(x)), collapse = ",")
        query <- gettextf("SELECT %s,%s FROM %s %s GROUP BY 1 ORDER BY 1", caseText, sText, obj, wc)
        newClusters <- try(tdQuery(query))
        if (!is.data.frame(newClusters)) 
            stop("Error processing request...try lower number of centers.")
        initClusters <- newClusters[-1]
        newCaseText <- .td.genclusterid(initClusters, "newClusterID")
        query <- gettextf("SELECT COUNT(*) FROM (SELECT %s, %s FROM %s WHERE clusterID <> newClusterID %s) A", caseText, 
            newCaseText, obj, ac)
        switched <- try(tdQuery(query))
        if (!is.data.frame(switched)) 
            break
        if (!switched[1]) 
            iter <- iter.max
        
        iter <- iter + 1
    }
    
    caseText <- .td.genclusterid(initClusters)
    query <- gettextf("SELECT %s, COUNT(*) FROM %s %s GROUP BY 1 ORDER BY 1", caseText, obj, wc)
    sz <- try(tdQuery(query))
    csums <- paste(gettextf("SUM(C%d) AS C%d", 1:centers, 1:centers), collapse = ",")
    subq <- gettextf("SELECT %s,%s FROM %s %s GROUP BY 1", caseText, paste(.td.genclusterss(initClusters), collapse = ","), 
        obj, wc)
    query <- gettextf("SELECT %s FROM (%s) A", csums, subq)
    withinss <- try(tdQuery(query))
    oList <- vector("list", 4)
    names(oList) <- c("cluster", "centers", "withinss", "size")
    attr(oList, "class") <- "kmeans"
    oList$centers <- as.matrix(initClusters)
    oList$size <- sz[[2]]
    if (is.data.frame(withinss)) 
        oList$withinss <- as.vector(withinss[1, ])
    
    return(oList)
}

.td.genclusterid <- function(centroids, AS = "clusterID") {
    ccnt <- nrow(centroids)
    nms <- names(centroids)
    cText <- c()
    caseText <- c("CASE")
    for (i in 1:ccnt) cText[i] <- paste("sqrt(", gettextf("(%f-\"%s\")**2", centroids[i, ], nms), ")", sep = "", collapse = "+")
    
    if (ccnt > 1) {
        for (i in 1:(ccnt - 1)) {
            caseText <- c(caseText, " WHEN ")
            for (j in (i + 1):ccnt) {
                if (j != (i + 1)) 
                  caseText <- c(caseText, " AND ")
                caseText <- c(caseText, gettextf("(%s <= %s)", cText[i], cText[j]))
            }
            caseText <- c(caseText, " THEN ", i, " ")
        }
    } else caseText <- c(caseText, gettextf(" WHEN (1=1) THEN 1 ", cText[1]))
    caseText <- c(caseText, " ELSE ", ccnt, " END AS ", AS)
    caseText <- paste(caseText, collapse = "")
    
    return(caseText)
}

.td.genclusterss <- function(centroids) {
    ccnt <- nrow(centroids)
    nms <- names(centroids)
    cText <- c()
    for (i in 1:ccnt) {
        cText[i] <- paste("(", gettextf("(%f-\"%s\")**2", centroids[i, ], nms), ")", sep = "", collapse = "+")
        cText[i] <- paste("SUM(CASE WHEN clusterID = ", i, " THEN ", cText[i], " ELSE 0 END) AS C", i, sep = "")
    }
    return(cText)
} 
