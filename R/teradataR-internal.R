.td.dropTable <- function(obj) {
    query <- gettextf("DROP TABLE %s", obj)
    df <- try(tdQueryUpdate(query))
}

.td.genbin <- function(col, bins = 10, as = "XBIN", xmin = "XMIN", xmax = "XMAX") {
    x <- gettextf(paste("CASE WHEN XMAX = XMIN THEN 1 ", "WHEN XMAX = %s THEN %d ", "ELSE CAST((%s - XMIN)/(CAST(XMAX - XMIN AS FLOAT)", 
        "/%d) + 1 AS INTEGER) END AS ", as, sep = ""), col, bins, col, bins)
    if (xmin != "XMIN" || xmax != "XMAX") {
        x <- gsub("XMIN", xmin, x)
        x <- gsub("XMAX", xmax, x)
    }
    return(x)
}

.td.genbinomial <- function(tdf, col1, col2, thresh = 0.05, binprob = 0.5) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    nrmobj <- .td.locateObject("TWMS_Normal")
    binobj <- .td.locateObject("TWMS_Binomial")
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" WHERE ", attr(tdf, "whereClause"))
    query <- paste("SELECT \"_twm_N\" AS \"N\",\"_twm_NPos\" AS \"NPos\" ", ",\"_twm_NNeg\" AS \"NNeg\" ", ",CASE \tWHEN \"N\" >100 THEN 2*(COALESCE(\"_twm_ND\".\"P\", 0.000001)) ", 
        "WHEN \"_twm_NNeg\" < \"_twm_NPos\" THEN ", "(CASE \tWHEN 2*(COALESCE(1 - \"_twm_BD\".\"CumBinomDist\", 0.000001)) > 1 THEN 1 ", 
        "ELSE 2*(COALESCE(1 - \"_twm_BD\".\"CumBinomDist\", 0.000001)) END) ", "ELSE ", "(CASE \tWHEN 2*(COALESCE(\"_twm_BD\".\"CumBinomDist\", 0.000001)) > 1 THEN 1 ", 
        "ELSE 2*(COALESCE(\"_twm_BD\".\"CumBinomDist\", 0.000001)) END) END AS \"BP\" ", ",CASE \tWHEN \"_twm_N\" IS NULL THEN NULL ", 
        "WHEN \"_twm_NPos\" > \"_twm_NNeg\" AND \"BP\" <= \"_twm_Alpha\" THEN 'p' ", "WHEN \"BP\" > \"_twm_Alpha\" THEN 'a' ", 
        "WHEN \"_twm_NPos\" <= \"_twm_NNeg\" AND \"BP\" <= \"_twm_Alpha\" THEN 'n' ", "ELSE NULL END AS \"BinomialCallP_`thresh`\" ", 
        "FROM ", "( ", "SELECT ", "`thresh` AS \"_twm_Alpha\" ", ",SUM (CASE WHEN (\"`col2`\" - \"`col1`\") > 0  THEN  1  ELSE  0  END) AS \"_twm_NPos\" ", 
        ",SUM (CASE WHEN (\"`col2`\" - \"`col1`\") <= 0  THEN  1  ELSE  0  END) AS \"_twm_NNeg\" ", ",CASE WHEN (\"_twm_NPos\" + \"_twm_NNeg\") > 0 THEN (\"_twm_NPos\" + \"_twm_NNeg\") ELSE NULL END AS \"_twm_N\" ", 
        ",`binprob` AS \"_twm_BProb\" ", ",(\"_twm_NPos\" - \"_twm_N\"*\"_twm_BProb\")/SQRT(\"_twm_N\"*\"_twm_BProb\"*(1 - \"_twm_BProb\")) AS \"_twm_ZPos\" ", 
        "FROM `obj` `where` ", ") AS \"T1\" ", "LEFT OUTER JOIN `binobj` \"_twm_BD\" ", "ON \"_twm_BD\".\"N_Trials\" = \"T1\".\"_twm_N\" ", 
        "AND \"_twm_BD\".\"K\" = (CASE \tWHEN \"_twm_NNeg\" < \"_twm_NPos\" THEN \"T1\".\"_twm_NPos\"- 1 ", "ELSE \"T1\".\"_twm_NPos\" END) ", 
        "AND \"_twm_BD\".\"Prob\" = \"T1\".\"_twm_BProb\" ", "LEFT OUTER JOIN `nrmobj` \"_twm_ND\" ", "ON \"_twm_ND\".\"Z\"*1000(DECIMAL(10,0)) = -ABS(\"_twm_ZPos\")*1000(DECIMAL(10,0)) ", 
        sep = "")
    query <- gsub("`obj`", obj, query)
    query <- gsub("`nrmobj`", nrmobj, query)
    query <- gsub("`binobj`", binobj, query)
    query <- gsub("`col1`", col1, query)
    query <- gsub("`col2`", col2, query)
    query <- gsub("`thresh`", thresh, query)
    query <- gsub("`binprob`", binprob, query)
    query <- gsub("`where`", wc, query)
    return(query)
}

.td.genbinomialsign <- function(tdf, col, thresh = 0.05) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    nrmobj <- .td.locateObject("TWMS_Normal")
    binobj <- .td.locateObject("TWMS_Binomial")
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" WHERE ", attr(tdf, "whereClause"))
    query <- paste("SELECT \"_twm_N\" AS \"N\",\"_twm_NPos\" AS \"NPos\" ", ",\"_twm_NNeg\" AS \"NNeg\" ", ",CASE \tWHEN \"N\" >100 THEN 2*(COALESCE(\"_twm_ND\".\"P\", 0.000001)) ", 
        "WHEN \"_twm_NNeg\" < \"_twm_NPos\" THEN ", "(CASE \tWHEN 2*(COALESCE(1 - \"_twm_BD\".\"CumBinomDist\", 0.000001)) > 1 THEN 1 ", 
        "ELSE 2*(COALESCE(1 - \"_twm_BD\".\"CumBinomDist\", 0.000001)) END) ", "ELSE ", "(CASE \tWHEN 2*(COALESCE(\"_twm_BD\".\"CumBinomDist\", 0.000001)) > 1 THEN 1 ", 
        "ELSE 2*(COALESCE(\"_twm_BD\".\"CumBinomDist\", 0.000001)) END) END AS \"BP\" ", ",CASE \tWHEN \"_twm_N\" IS NULL THEN NULL ", 
        "WHEN \"_twm_NPos\" > \"_twm_NNeg\" AND \"BP\" <= \"_twm_Alpha\" THEN 'p' ", "WHEN \"BP\" > \"_twm_Alpha\" THEN 'a' ", 
        "WHEN \"_twm_NPos\" <= \"_twm_NNeg\" AND \"BP\" <= \"_twm_Alpha\" THEN 'n' ", "ELSE NULL END AS \"BinomialCallP_`thresh`\" ", 
        "FROM ", "( ", "SELECT ", "`thresh` AS \"_twm_Alpha\" ", ",SUM (CASE WHEN (\"`col`\") > 0  THEN  1  ELSE  0  END) AS \"_twm_NPos\" ", 
        ",SUM (CASE WHEN (\"`col`\") <= 0  THEN  1  ELSE  0  END) AS \"_twm_NNeg\" ", ",CASE WHEN (\"_twm_NPos\" + \"_twm_NNeg\") > 0 THEN (\"_twm_NPos\" + \"_twm_NNeg\") ELSE NULL END AS \"_twm_N\" ", 
        ",0.5 AS \"_twm_BProb\" ", ",(\"_twm_NPos\" - \"_twm_N\"*\"_twm_BProb\")/SQRT(\"_twm_N\"*\"_twm_BProb\"*(1 - \"_twm_BProb\")) AS \"_twm_ZPos\" ", 
        "FROM `obj` `where` ", ") AS \"T1\" ", "LEFT OUTER JOIN `binobj` \"_twm_BD\" ", "ON \"_twm_BD\".\"N_Trials\" = \"T1\".\"_twm_N\" ", 
        "AND \"_twm_BD\".\"K\" = (CASE \tWHEN \"_twm_NNeg\" < \"_twm_NPos\" THEN \"T1\".\"_twm_NPos\"- 1 ", "ELSE \"T1\".\"_twm_NPos\" END) ", 
        "AND \"_twm_BD\".\"Prob\" = \"T1\".\"_twm_BProb\" ", "LEFT OUTER JOIN `nrmobj` \"_twm_ND\" ", "ON \"_twm_ND\".\"Z\"*1000(DECIMAL(10,0)) = -ABS(\"_twm_ZPos\")*1000(DECIMAL(10,0)) ", 
        sep = "")
    query <- gsub("`obj`", obj, query)
    query <- gsub("`nrmobj`", nrmobj, query)
    query <- gsub("`binobj`", binobj, query)
    query <- gsub("`col`", col, query)
    query <- gsub("`thresh`", thresh, query)
    query <- gsub("`where`", wc, query)
    return(query)
}

.td.gendagopearson <- function(tdf, col, thresh = 0.05) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    chisqobj <- .td.locateObject("TWMS_ChiSquare")
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" WHERE ", attr(tdf, "whereClause"))
    subq <- paste("(SELECT \"`col`\" AS \"_twm_Valu\" ", ",SUM(1) OVER (  ORDER BY \"`col`\" ROWS UNBOUNDED PRECEDING) AS \"_twm_Rnk\" ", 
        "FROM `obj` `where`) ", sep = "")
    query <- paste("SELECT \"_twm_Ksquared\" AS \"T\" ", ",\"_twm_Zkurtosis\" AS \"Zkurtosis\" ", ",\"_twm_Zskew\" AS \"Zskew\" ", 
        ",(CASE \tWHEN \"T\" > coalesce(\"chi\".\"t9\", 99) THEN 0.0001 ", "WHEN \"T\" > coalesce(\"chi\".\"t8\", 99) THEN \"P\".\"T8\"+(\"P\".\"T9\"-\"P\".\"T8\")*(\"T\"-\"chi\".\"t8\")/(\"chi\".\"t9\"-\"chi\".\"t8\") ", 
        "WHEN \"T\" > coalesce(\"chi\".\"t7\", 99) THEN \"P\".\"T7\"+(\"P\".\"T8\"-\"P\".\"T7\")*(\"T\"-\"chi\".\"t7\")/(\"chi\".\"t8\"-\"chi\".\"t7\") ", 
        "WHEN \"T\" > coalesce(\"chi\".\"t6\", 99) THEN \"P\".\"T6\"+(\"P\".\"T7\"-\"P\".\"T6\")*(\"T\"-\"chi\".\"t6\")/(\"chi\".\"t7\"-\"chi\".\"t6\") ", 
        "WHEN \"T\" > coalesce(\"chi\".\"t5\", 99) THEN \"P\".\"T5\"+(\"P\".\"T6\"-\"P\".\"T5\")*(\"T\"-\"chi\".\"t5\")/(\"chi\".\"t6\"-\"chi\".\"t5\") ", 
        "WHEN \"T\" > coalesce(\"chi\".\"t4\", 99) THEN \"P\".\"T4\"+(\"P\".\"T5\"-\"P\".\"T4\")*(\"T\"-\"chi\".\"t4\")/(\"chi\".\"t5\"-\"chi\".\"t4\") ", 
        "WHEN \"T\" > coalesce(\"chi\".\"t3\", 99) THEN \"P\".\"T3\"+(\"P\".\"T4\"-\"P\".\"T3\")*(\"T\"-\"chi\".\"t3\")/(\"chi\".\"t4\"-\"chi\".\"t3\") ", 
        "WHEN \"T\" > coalesce(\"chi\".\"t2\", 99) THEN \"P\".\"T2\"+(\"P\".\"T3\"-\"P\".\"T2\")*(\"T\"-\"chi\".\"t2\")/(\"chi\".\"t3\"-\"chi\".\"t2\") ", 
        "WHEN \"T\" > coalesce(\"chi\".\"t1\", 99) THEN \"P\".\"T1\"+(\"P\".\"T2\"-\"P\".\"T1\")*(\"T\"-\"chi\".\"t1\")/(\"chi\".\"t2\"-\"chi\".\"t1\") ", 
        "ELSE 0.25 END) AS \"ChiPValue\" ", ",(CASE \tWHEN \"T\" > coalesce(\"chi\".\"t9\", 99) THEN '<0.0001' ", "WHEN \"T\" > coalesce(\"chi\".\"t1\", 99) THEN NULL ", 
        "ELSE '>0.25' END) AS \"ChiPText\" ", ",CASE \tWHEN \"ChiPValue\" IS NULL THEN  NULL ", "WHEN \"ChiPValue\" <= `thresh` THEN 'p' ", 
        "ELSE 'a' END AS \"ChiCallP_`thresh`\" ", "FROM ", "( ", "SELECT ", "MAX(\"_twm_C\") AS \"_twm_N\" ", ",AVG((\"_twm_Valu\"-\"_twm_M\")*(\"_twm_Valu\"-\"_twm_M\")) AS \"_twm_M2\" ", 
        ",AVG((\"_twm_Valu\"-\"_twm_M\")*(\"_twm_Valu\"-\"_twm_M\")*(\"_twm_Valu\"-\"_twm_M\")) AS \"_twm_M3\" ", ",AVG((\"_twm_Valu\"-\"_twm_M\")*(\"_twm_Valu\"-\"_twm_M\")*(\"_twm_Valu\"-\"_twm_M\")*(\"_twm_Valu\"-\"_twm_M\")) AS \"_twm_M4\" ", 
        ",\"_twm_M3\"/EXP(LN(\"_twm_M2\")*3/2E0)*SQRT(((\"_twm_N\"+1)*(\"_twm_N\"+3))/(6E0*(\"_twm_N\"-2))) AS \"_twm_Y\" ", 
        ",SQRT(2*( (3*(\"_twm_N\"*\"_twm_N\"+27*\"_twm_N\"-70)*(\"_twm_N\"+1)*(\"_twm_N\"+3))/((\"_twm_N\"-2E0)*(\"_twm_N\"+5)*(\"_twm_N\"+7)*(\"_twm_N\"+9))  -1)) -1 AS \"_twm_Wsq\" ", 
        ",SQRT(2/NULLIFZERO(\"_twm_Wsq\"-1)) AS \"_twm_Apha\" ", ",LN(\"_twm_Y\"/\"_twm_Apha\"+ SQRT((\"_twm_Y\"/\"_twm_Apha\")*(\"_twm_Y\"/\"_twm_Apha\") +1))/SQRT(LN(SQRT(\"_twm_Wsq\"))) AS \"_twm_Zskew\" ", 
        ",\"_twm_M4\"/(\"_twm_M2\"*\"_twm_M2\") AS \"_twm_b2\" ", ",3*(\"_twm_N\"-1)/(\"_twm_N\"+1E0) AS \"_twm_EB2\" ", 
        ",24*\"_twm_N\"*(\"_twm_N\"-2)*(\"_twm_N\"-3)/((\"_twm_N\"+1E0)*(\"_twm_N\"+1)*(\"_twm_N\"+3)*(\"_twm_N\"+5)) AS \"_twm_VB2\" ", 
        ",(\"_twm_b2\"-\"_twm_EB2\")/SQRT(\"_twm_VB2\") AS \"_twm_X\" ", ",6*(\"_twm_N\"*\"_twm_N\"-5*\"_twm_N\"+2)*SQRT(6*(\"_twm_N\"+3)*(\"_twm_N\"+5)/(\"_twm_N\"*(\"_twm_N\"-2E0)*(\"_twm_N\"-3)))/((\"_twm_N\"+7E0)*(\"_twm_N\"+9)) AS \"_twm_thirdsmo\" ", 
        ",(SQRT(1+(4/\"_twm_thirdsmo\"*\"_twm_thirdsmo\"))+2/\"_twm_thirdsmo\")*8/\"_twm_thirdsmo\"+6 AS \"_twm_A\" ", 
        ",((1-2/(9E0*\"_twm_A\")) - EXP(LN(ABS((1-2/\"_twm_A\")/(1+\"_twm_X\"*SQRT(2/(\"_twm_A\"-4)))))/3))/SQRT(2/(9E0*\"_twm_A\")) AS \"_twm_Zkurtosis\" ", 
        ",\"_twm_Zskew\"*\"_twm_Zskew\"+\"_twm_Zkurtosis\"*\"_twm_Zkurtosis\" AS \"_twm_Ksquared\" ", "FROM ", "( ", "SELECT ", 
        "COUNT(*) AS \"_twm_C\" ", ",AVG(\"_twm_Valu\") AS \"_twm_M\" ", "FROM `subq` SQ1 ", ") AS \"T4\" ", ", `subq` \"T3\" ", 
        ") AS \"T2\" ", "left outer join `chisqobj` \"chi\" ON \"chi\".\"k\"= 2 ", ",(SEL \"T9\",\"T8\",\"T7\",\"T6\",\"T5\",\"T4\",\"T3\",\"T2\",\"T1\" FROM `chisqobj` \"chi\" where \"chi\".\"k\"=0)  AS \"P\"", 
        sep = "")
    query <- gsub("`subq`", subq, query)
    query <- gsub("`obj`", obj, query)
    query <- gsub("`chisqobj`", chisqobj, query)
    query <- gsub("`col`", col, query)
    query <- gsub("`thresh`", thresh, query)
    query <- gsub("`where`", wc, query)
    return(query)
}

.td.genfreq <- function(tdf, col) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" WHERE ", attr(tdf, "whereClause"))
    query <- paste("SELECT '`tbl`' AS xtbl,'`col`' AS xcol,`col` AS xval ", ",CAST(COUNT(*) AS FLOAT) AS xcnt ", ",100.000 * xcnt / xall AS xpct ", 
        "FROM `obj` A,(SELECT CAST(COUNT(*) AS FLOAT) AS xall ", "FROM `obj` `where`) B `where` GROUP BY xtbl, xcol, xval, xall ORDER BY xval", 
        sep = "")
    query <- gsub("`obj`", obj, query)
    query <- gsub("`col`", col, query)
    query <- gsub("`tbl`", attr(tdf, "tableName"), query)
    query <- gsub("`where`", wc, query)
    return(query)
}

.td.genfway <- function(tdf, col1, col2, thresh = 0.05) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    fobj <- .td.locateObject("TWMS_F")
    subq <- paste("(SELECT \"`col1`\" AS \"_twm_Valu\",\"_twm_Sad\" AS \"_twm_Sad\" ", ",SUM(1) OVER (  ORDER BY \"`col1`\" ROWS UNBOUNDED PRECEDING) AS \"_twm_Rnk\" ", 
        "FROM `obj` \"T1\",(SELECT \"`col2`\" ", ",SUM(1) OVER (ORDER BY \"`col2`\" ROWS UNBOUNDED PRECEDING) AS \"_twm_sad\" ", 
        "FROM (SELECT \"`col2`\" FROM `obj` `where` GROUP BY \"`col2`\") AS \"T3\") AS \"T2\" ", "where \"T1\".\"`col2`\" = \"T2\".\"`col2`\")", 
        sep = "")
    query <- paste("SELECT \"_twm_a\" AS \"DF\",\"_twm_b\" AS \"DFErr\",\"T3\".\"Fdist\" AS \"F\" ", ",CASE \tWHEN \"F\" > \"T0.001\" THEN 0.001 ", 
        "WHEN \"F\" > \"T0.005\" THEN 0.005+(0.001-0.005)*(\"F\"-\"T0.005\")/(\"T0.001\"-\"T0.005\") ", "WHEN \"F\" > \"T0.01\" THEN 0.01+(0.005-0.01)*(\"F\"-\"T0.01\")/(\"T0.005\"-\"T0.01\") ", 
        "WHEN \"F\" > \"T0.025\" THEN 0.025+(0.01-0.025)*(\"F\"-\"T0.025\")/(\"T0.01\"-\"T0.025\") ", "WHEN \"F\" > \"T0.05\" THEN 0.05+(0.025-0.05)*(\"F\"-\"T0.05\")/(\"T0.025\"-\"T0.05\") ", 
        "WHEN \"F\" > \"T0.1\" THEN 0.1+(0.05-0.1)*(\"F\"-\"T0.1\")/(\"T0.05\"-\"T0.1\") ", "WHEN \"F\" > \"T0.25\" THEN 0.25+(0.1-0.25)*(\"F\"-\"T0.25\")/(\"T0.1\"-\"T0.25\") ", 
        "WHEN \"F\" IS NULL THEN  NULL ", "ELSE 0.25 END AS \"FPValue\" ", ",CASE \tWHEN \"F\" > \"T0.001\" THEN '<0.001' ", 
        "WHEN \"F\" > \"T0.25\" THEN NULL ", "WHEN \"F\" IS NULL THEN  NULL ", "ELSE '>0.25' END AS \"FPText\" ", ",CASE \tWHEN \"FPValue\" IS NULL THEN  NULL ", 
        "WHEN \"FPValue\" <= `thresh` THEN 'p' ", "ELSE 'a' END AS \"FCallP_`thresh`\" ", "FROM (SELECT ", "SUM(\"T4\".\"_twm_Vi\")  AS \"_twm_Vw\" ", 
        ",MIN(\"T4b\".\"_twm_Va\")  AS \"_twm_V\" ", ",(\"_twm_V\" - \"_twm_Vw\")  AS \"_twm_Vb\" ", ",COUNT(\"T4\".\"_twm_N\") - 1 AS \"_twm_a\" ", 
        ",MIN(\"T4b\".\"_twm_NN\") - 1 -\"_twm_a\" AS \"_twm_b\" ", ",CASE WHEN \"_twm_a\" < 121 THEN \"_twm_a\" ELSE 121 END AS \"_twm_aa\" ", 
        ",CASE WHEN \"_twm_b\" < 121 THEN \"_twm_b\" ELSE 121 END AS \"_twm_bb\" ", ",CASE WHEN \"_twm_Vw\" >0  THEN (\"_twm_Vb\"/NULLIFZERO(\"_twm_a\")) / NULLIFZERO(\"_twm_Vw\"/\"_twm_b\") ELSE NULL END AS \"Fdist\" ", 
        "FROM (SELECT ", "\"_twm_sad\" ", ",COUNT(*) AS \"_twm_N\" ", ",AVG(\"_twm_valu\") AS \"_twm_M\" ", ",\"_twm_N\"*VAR_POP(\"_twm_valu\") AS \"_twm_Vi\" ", 
        "FROM `subq` SQ1 ", "GROUP BY \"_twm_sad\") AS \"T4\" , ", "(SELECT COUNT(*) AS \"_twm_NN\" ", ",\"_twm_NN\"*VAR_POP(\"_twm_valu\") AS \"_twm_Va\" ", 
        "FROM `subq` SQ1) AS \"T4b\") AS \"T3\" ", "left outer join `fobj` \"FT\" ", "ON (\"DF_1\" = \"T3\".\"_twm_aa\") AND (\"DF_2\" = \"T3\".\"_twm_bb\")", 
        sep = "")
    query <- gsub("`subq`", subq, query)
    query <- gsub("`obj`", obj, query)
    query <- gsub("`fobj`", fobj, query)
    query <- gsub("`col1`", col1, query)
    query <- gsub("`col2`", col2, query)
    query <- gsub("`thresh`", thresh, query)
}

.td.genhist <- function(tdf, col, breaks = 10) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" WHERE ", attr(tdf, "whereClause"))
    
    query <- paste("SELECT ", .td.genbin(col, breaks, "xbin", "xmin", "xmax"), ",xmin + (xbin - 1) * ((xmax - xmin) / `brks`.0E0) AS xbeg ", 
        ",xmin + xbin * ((xmax - xmin) / `brks`.0E0) AS xend ", ",CAST(COUNT(*) AS FLOAT) AS xcnt ", "FROM `obj`,(SELECT MIN(`col`) AS xmin ", 
        ",MAX(`col`) AS xmax, CAST(COUNT(*) AS FLOAT) AS xall ", "FROM `obj` `where`) A `where` GROUP BY 1,2,3 ORDER BY 1,2,3", 
        sep = "")
    query <- gsub("`obj`", obj, query)
    query <- gsub("`col`", col, query)
    query <- gsub("`brks`", breaks, query)
    query <- gsub("`where`", wc, query)
    return(query)
}

.td.genjoin <- function(tdf1, tdf2, index1, index2, joinType = "inner") {
    jtypes <- c("inner", "left outer", "right outer", "full outer")
    jt1Obj <- .td.object(attr(tdf1, "tableName"), attr(tdf1, "database"))
    wc1 <- ""
    if (!is.null(attr(tdf1, "whereClause"))) 
        wc1 <- paste(" WHERE ", attr(tdf1, "whereClause"))
    jt2Obj <- .td.object(attr(tdf2, "tableName"), attr(tdf2, "database"))
    wc2 <- ""
    if (!is.null(attr(tdf2, "whereClause"))) {
        wc2 <- paste(" WHERE ", attr(tdf2, "whereClause"))
        if (nchar(wc1)) 
            wc2 <- gsub("WHERE", "AND", wc2)
    }
    nms1 <- names(tdf1)
    nms2 <- names(tdf2)
    for (i in 1:length(nms2)) {
        if (nms2[i] %in% nms1) 
            nms2[i] <- NA
    }
    nms2 <- nms2[!is.na(nms2)]
    cols1 <- paste(rep(gettextf("JT1.\"%s\"", nms1)), collapse = ",")
    cols2 <- paste(rep(gettextf("JT2.\"%s\"", nms2)), collapse = ",")
    lst <- gettextf("%s JOIN %s AS JT2", toupper(joinType), jt2Obj)
    for (i in 1:length(index1)) {
        if (i == 1) 
            lst[i + 1] <- gettextf("ON JT1.\"%s\" = JT2.\"%s\"", index1[i], index2[i]) else lst[i + 1] <- gettextf("AND JT1.\"%s\" = JT2.\"%s\"", index1[i], index2[i])
    }
    if (length(nms2)) 
        query <- gettextf("SELECT %s,%s FROM %s AS JT1 %s %s %s", paste(cols1, collapse = ","), paste(cols2, collapse = ","), 
            jt1Obj, paste(lst, collapse = " "), wc1, wc2) else query <- gettextf("SELECT %s FROM %s AS JT1 %s %s %s", paste(cols1, collapse = ","), jt1Obj, paste(lst, collapse = " "), 
        wc1, wc2)
    return(query)
}

.td.genks <- function(tdf, col, thresh = 0.05) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    ksobj <- .td.locateObject("TWMS_KolmogorovSmirnov")
    nrmobj <- .td.locateObject("TWMS_Normal")
    query <- paste("SELECT \"_twm_T\" AS \"Klm\", \"T1\".\"_twm_N\" AS \"M\" ", ",CASE \tWHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"klm\".\"t5\", 99) THEN 0.01 ", 
        "WHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"klm\".\"t4\", 99) THEN 1-(\"P\".\"T4\"+(\"P\".\"T5\"-\"P\".\"T4\")*(\"klm\"-\"_twm_F\"*\"klm\".\"t4\")/(\"_twm_F\"*\"klm\".\"t5\"-\"_twm_F\"*\"klm\".\"t4\")) ", 
        "WHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"klm\".\"t3\", 99) THEN 1-(\"P\".\"T3\"+(\"P\".\"T4\"-\"P\".\"T3\")*(\"klm\"-\"_twm_F\"*\"klm\".\"t3\")/(\"_twm_F\"*\"klm\".\"t4\"-\"_twm_F\"*\"klm\".\"t3\")) ", 
        "WHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"klm\".\"t2\", 99) THEN 1-(\"P\".\"T2\"+(\"P\".\"T3\"-\"P\".\"T2\")*(\"klm\"-\"_twm_F\"*\"klm\".\"t2\")/(\"_twm_F\"*\"klm\".\"t3\"-\"_twm_F\"*\"klm\".\"t2\")) ", 
        "WHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"klm\".\"t1\", 99) THEN 1-(\"P\".\"T1\"+(\"P\".\"T2\"-\"P\".\"T1\")*(\"klm\"-\"_twm_F\"*\"klm\".\"t1\")/(\"_twm_F\"*\"klm\".\"t2\"-\"_twm_F\"*\"klm\".\"t1\")) ", 
        "ELSE 0.20 END AS \"KlmPValue\" ", ",(CASE \tWHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"klm\".\"t5\", 99) THEN '<0.01' ", 
        "WHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"klm\".\"t1\", 99) THEN NULL ", "ELSE '>0.20' END) AS \"KlmPText\" ", ",CASE \tWHEN \"KlmPValue\" IS NULL THEN  NULL ", 
        "WHEN \"KlmPValue\" <= `thresh` THEN 'p' ", "ELSE 'a' END AS \"KlmCallP_`thresh`\" ", "FROM (SELECT MAX(\"_twm_N1\") AS \"_twm_N\" ", 
        ",CASE \tWHEN \"_twm_N\" <= 40 THEN \"_twm_N\" ", "ELSE 41 END AS \"_twm_K\" ", ",CASE \tWHEN \"_twm_N\" <= 40 THEN 1 ", 
        "ELSE 1/(SQRT(\"_twm_N\"+ SQRT(\"_twm_N\"/1E1))) END AS \"_twm_F\" ", ",MAX(\"_twm_diff\") AS \"_twm_T\" ", "FROM (SELECT \"_twm_N1\",\"_twm_zpos\",\"_twm_p1\" ", 
        ",COALESCE(\"nd\".\"pp\", 1E-6) AS \"_twm_nP\" ", ",ABS(\"_twm_p1\"-\"_twm_nP\") AS \"_twm_diff\" ", "FROM (SELECT \"_twm_V\",MAX(\"_twm_N\") AS \"_twm_N1\" ", 
        ",(\"_twm_v\"-MAX(\"_twm_M\"))/NULLIFZERO(MAX(\"_twm_SD\")) AS \"_twm_Zpos\" ", ",COUNT(*)/MAX(NULLIFZERO(CAST(\"_twm_N\" AS FLOAT))) AS \"_twm_p1\" ", 
        "FROM `obj` AS \"TA\" , ", "(SELECT ", "\"TA2\".\"_twm_Vmin\" + \"TA1\".\"_twm_I\"*\"TA2\".\"_twm_range\"/1E2 AS \"_twm_V\" ", 
        "FROM (SELECT DAY_OF_CALENDAR - 1 AS \"_twm_I\" FROM \"SYS_CALENDAR\".\"CALENDAR\" ", "WHERE \"_twm_I\"<101) AS \"TA1\" , ", 
        "(SELECT MIN(\"`col`\") AS \"_twm_Vmin\",(MAX(\"`col`\")-MIN(\"`col`\")) AS \"_twm_range\" ", "FROM `obj`) AS \"TA2\") AS \"TB\" , ", 
        "(SELECT COUNT(*) AS \"_twm_N\",AVG(CAST(\"`col`\" AS FLOAT)) AS \"_twm_M\" ", ",STDDEV_SAMP(CAST(\"`col`\" AS FLOAT)) AS \"_twm_SD\" ", 
        "FROM `obj` ", ") AS \"TC\" WHERE \"TA\".\"`col`\"<=\"_twm_V\" GROUP BY \"_twm_V\" ", ") AS \"T3\" ", "LEFT OUTER JOIN (SELECT \"z\" AS \"zz\", \"p\" AS \"pp\" FROM `nrmobj` ", 
        "UNION SELECT -\"z\" AS \"zz\", 1-\"p\" AS \"pp\" FROM `nrmobj`) \"nd\" ", "ON \"nd\".\"zz\"*1000(DECIMAL(10,0)) = \"_twm_ZPos\"*1000(DECIMAL(10,0)) ", 
        ") AS \"T2\") AS \"T1\" ", "LEFT OUTER JOIN `ksobj` \"klm\" ", "ON (\"klm\".\"n\" = \"T1\".\"_twm_K\"), ", "(SELECT \"T5\",\"T4\",\"T3\",\"T2\",\"T1\" ", 
        "FROM `ksobj` AS \"KT\" ", "WHERE \"KT\".\"N\" = 0 ", ") AS \"P\" ", sep = "")
    query <- gsub("`obj`", obj, query)
    query <- gsub("`ksobj`", ksobj, query)
    query <- gsub("`nrmobj`", nrmobj, query)
    query <- gsub("`col`", col, query)
    query <- gsub("`thresh`", thresh, query)
    return(query)
}

.td.genlilliefors <- function(tdf, col, thresh = 0.06) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    ksobj <- .td.locateObject("TWMS_KolmogorovSmirnov")
    nrmobj <- .td.locateObject("TWMS_Normal")
    nrmshrtobj <- .td.locateObject("TWMS_Normalshort")
    lfobj <- .td.locateObject("TWMS_Lilliefors")
    query <- paste("SELECT \"_twm_T\" AS \"Lilliefors\",\"T1\".\"_twm_J\" AS \"M\" ", ",CASE \tWHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"L\".\"t5\", 99) THEN 0.01 ", 
        "WHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"L\".\"t4\", 99) THEN 1-(\"P\".\"T4\"+(\"P\".\"T5\"-\"P\".\"T4\")*(\"Lilliefors\"-\"_twm_F\"*\"L\".\"t4\")/(\"_twm_F\"*\"L\".\"t5\"-\"_twm_F\"*\"L\".\"t4\")) ", 
        "WHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"L\".\"t3\", 99) THEN 1-(\"P\".\"T3\"+(\"P\".\"T4\"-\"P\".\"T3\")*(\"Lilliefors\"-\"_twm_F\"*\"L\".\"t3\")/(\"_twm_F\"*\"L\".\"t4\"-\"_twm_F\"*\"L\".\"t3\")) ", 
        "WHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"L\".\"t2\", 99) THEN 1-(\"P\".\"T2\"+(\"P\".\"T3\"-\"P\".\"T2\")*(\"Lilliefors\"-\"_twm_F\"*\"L\".\"t2\")/(\"_twm_F\"*\"L\".\"t3\"-\"_twm_F\"*\"L\".\"t2\")) ", 
        "WHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"L\".\"t1\", 99) THEN 1-(\"P\".\"T1\"+(\"P\".\"T2\"-\"P\".\"T1\")*(\"Lilliefors\"-\"_twm_F\"*\"L\".\"t1\")/(\"_twm_F\"*\"L\".\"t2\"-\"_twm_F\"*\"L\".\"t1\")) ", 
        "ELSE 0.20 END AS \"LillieforsPValue\" ", ",(CASE \tWHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"L\".\"t5\", 99) THEN '<0.01' ", 
        "WHEN \"_twm_T\" > COALESCE(\"_twm_F\"*\"L\".\"t1\", 99) THEN NULL ", "ELSE '>0.20' END) AS \"LillieforsPText\" ", 
        ",CASE \tWHEN \"LillieforsPValue\" IS NULL THEN  NULL ", "WHEN \"LillieforsPValue\" <= `thresh` THEN 'p' ", "ELSE 'a' END AS \"LillieforsCallP_`thresh`\" ", 
        "FROM ", "( ", "SELECT ", "MAX(\"_twm_N\") AS \"_twm_J\" ", ",CASE \tWHEN \"_twm_J\" <= 31 THEN \"_twm_J\" ", "ELSE 31 END AS \"_twm_K\" ", 
        ",CASE \tWHEN \"_twm_J\" <= 31 THEN 1 ", "ELSE 1/(SQRT(\"_twm_J\") -0.01 + 0.83/SQRT(\"_twm_J\")) END AS \"_twm_F\" ", 
        ",MAX(ABS(\"_twm_p1\"-(COALESCE(\"nd\".\"pp\", 1E-6)))) AS \"_twm_T\" ", "FROM ", "( ", "SELECT ", "MAX(\"_twm_N1\") AS \"_twm_N\" ", 
        ",\"_twm_zi\" ", ",COUNT(*)/MAX(CAST(\"_twm_N1\" AS FLOAT)) AS \"_twm_p1\" ", "FROM ", "( ", "SELECT ", "\"z\" AS \"_twm_zi\" FROM `nrmshrtobj` UNION SELECT -\"z\" AS \"_twm_zi\" FROM `nrmshrtobj` ", 
        ") AS \"T3a\", ", "( ", "SELECT ", "\"_twm_N\" AS \"_twm_N1\" ", ",(\"`col`\"-\"_twm_M\")/\"_twm_SD\" AS \"_twm_Zvalu\" ", 
        "FROM `obj` AS \"TA\" , ", "( ", "SELECT ", "COUNT(*) AS \"_twm_N\" ", ",AVG(CAST(\"`col`\" AS FLOAT)) AS \"_twm_M\" ", 
        ",STDDEV_SAMP(CAST(\"`col`\" AS FLOAT)) AS \"_twm_SD\" ", "FROM `obj` ", ") AS \"TC\" ", ") AS \"T3b\" ", "WHERE \"_twm_Zvalu\" <= \"_twm_zi\" ", 
        "GROUP BY \"_twm_zi\" ", ") AS \"T2\" ", "LEFT OUTER JOIN (SELECT \"z\" AS \"zz\", \"p\" AS \"pp\" FROM `nrmobj` ", 
        "UNION SELECT -\"z\" AS \"zz\", 1-\"p\" AS \"pp\" FROM `nrmobj`) \"nd\" ", "ON \"nd\".\"zz\"*1000(DECIMAL(10,0)) = \"_twm_Zi\"*1000(DECIMAL(10,0)) ", 
        ") AS \"T1\" ", "LEFT OUTER JOIN `lfobj` \"L\" ", "ON (\"L\".\"n\" = \"T1\".\"_twm_K\"), ", "( ", "SELECT ", "\"T5\" ", 
        ",\"T4\" ", ",\"T3\" ", ",\"T2\" ", ",\"T1\" ", "FROM `lfobj` AS \"Lx\" ", "WHERE \"Lx\".\"N\" = 0 ", ") AS \"P\"", 
        sep = "")
    query <- gsub("`obj`", obj, query)
    query <- gsub("`ksobj`", ksobj, query)
    query <- gsub("`nrmobj`", nrmobj, query)
    query <- gsub("`nrmshrtobj`", nrmshrtobj, query)
    query <- gsub("`lfobj`", lfobj, query)
    query <- gsub("`col`", col, query)
    query <- gsub("`thresh`", thresh, query)
    return(query)
}

.td.genmedian <- function(tdf, col) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    col <- .td.gencolumnexpr(tdf[col])
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" AND ", attr(tdf, "whereClause"))
    
    query <- gettextf(paste("SELECT \"median\" FROM (SELECT ", "SUM(CAST(1 AS DECIMAL(18,0)))OVER(ORDER BY %s ROWS UNBOUNDED PRECEDING) AS \"xrnk\" ", 
        ",SUM(CAST(1 AS DECIMAL(18,0)))OVER(ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS \"gcnt\" ", ",SUM(CAST(%s AS FLOAT))OVER(ORDER BY %s ROWS 1 PRECEDING) AS \"xsum2\" ", 
        ",CASE WHEN \"gcnt\" MOD 2 = 0 THEN \"xsum2\" / 2 ELSE %s END AS \"median\" ", "FROM %s WHERE %s IS NOT NULL %s) T1 ", 
        "WHERE (\"xrnk\" + \"xrnk\" = \"gcnt\" + 2 AND \"gcnt\" MOD 2 = 0) ", "OR (\"xrnk\" + \"xrnk\" = \"gcnt\" + 1 AND \"gcnt\" MOD 2 = 1)"), 
        col, col, col, col, obj, col, wc)
    return(query)
}

.td.genmerge <- function(tdf1, tdf2, mergeType = "union") {
    mrg1Obj <- .td.object(attr(tdf1, "tableName"), attr(tdf1, "database"))
    wc1 <- ""
    if (!is.null(attr(tdf1, "whereClause"))) 
        wc1 <- paste(" WHERE ", attr(tdf1, "whereClause"))
    mrg2Obj <- .td.object(attr(tdf2, "tableName"), attr(tdf2, "database"))
    wc2 <- ""
    if (!is.null(attr(tdf2, "whereClause"))) 
        wc1 <- paste(" WHERE ", attr(tdf2, "whereClause"))
    nms1 <- names(tdf1)
    nms2 <- names(tdf2)
    qry1 <- gettextf("SELECT %s FROM %s %s", paste(nms1, collapse = ","), mrg1Obj, wc1)
    qry2 <- gettextf("SELECT %s FROM %s %s", paste(nms2, collapse = ","), mrg2Obj, wc2)
    query <- gettextf("%s %s %s", qry1, mergeType, qry2)
    return(query)
}

.td.genmode <- function(tdf, col) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" WHERE ", attr(tdf, "whereClause"))
    query <- paste("SELECT '`col`' AS \"xcol\" ,\"xmode\",\"xmode_cnt\" ", ",\"xmode_pct\",\"xnbrmodes\" ", "FROM (SELECT '`col`' AS \"xcol\",MIN(\"xmode\") AS \"xmode\" ", 
        ",MIN(\"xmode_cnt\") AS \"xmode_cnt\",MIN(\"xmode_pct\") AS \"xmode_pct\" ", ",CAST(COUNT(*) AS FLOAT) AS \"xnbrmodes\" ", 
        "FROM (SELECT \"xval\" AS \"xmode\",\"xcnt\" AS \"xmode_cnt\" ", ",100 * \"xcnt\" / SUM(\"xcnt\") ", "OVER (ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS \"xmode_pct\" ", 
        "FROM (SELECT \"`col`\" AS \"xval\",CAST(COUNT(*) AS FLOAT) AS \"xcnt\" ", "FROM `obj` `wc` GROUP BY \"`col`\") T1 ", 
        "QUALIFY RANK() OVER ( ORDER BY \"xcnt\" DESC) = 1) T1)T3", sep = "")
    query <- gsub("`obj`", obj, query)
    query <- gsub("`wc`", wc, query)
    query <- gsub("`col`", col, query)
}

.td.genmwnkw <- function(tdf, col1, cols2, thresh = 0.05) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    nrmobj <- .td.locateObject("TWMS_Normal")
    kwobj <- .td.locateObject("TWMS_KruskalWallis")
    chisqobj <- .td.locateObject("TWMS_ChiSquare")
    subq <- paste("(SELECT \"`col1`\" AS \"_twm_Valu\",\"_twm_Sad\" AS \"_twm_Sad\" ", ",CAST(SUM(1) OVER (  ORDER BY \"`col1`\" ROWS UNBOUNDED PRECEDING) AS FLOAT) AS \"_twm_Rnk\" ", 
        "FROM `obj` \"T1\",(SELECT `col2list` ", ",SUM(1) OVER (ORDER BY `col2list` ROWS UNBOUNDED PRECEDING) AS \"_twm_sad\" ", 
        "FROM (SELECT `col2list` FROM `obj` GROUP BY `col2list` ", ") AS \"T3\") AS \"T2\" where `col2where`)", sep = "")
    query <- paste("SELECT \"_twm_ZPos\" AS \"Z\",\"_twm_T\" AS \"ChiSq\",\"_twm_DoF\" AS \"DF\" ", ",(CASE \tWHEN \"_twm_ZPos\" IS NULL AND \"kws\".\"t3\" IS NOT NULL AND \"ChiSq\" > \"kws\".\"t3\" THEN 0.01 ", 
        "WHEN \"_twm_ZPos\" IS NULL AND \"kws\".\"t2\" IS NOT NULL AND \"ChiSq\" > \"kws\".\"t2\" THEN \"PP\".\"T2\"+(\"PP\".\"T3\"-\"PP\".\"T2\")*(\"ChiSq\"-\"kws\".\"t2\")/(\"kws\".\"t3\"-\"kws\".\"t2\") ", 
        "WHEN \"_twm_ZPos\" IS NULL AND \"kws\".\"t1\" IS NOT NULL AND \"ChiSq\" > \"kws\".\"t1\" THEN \"PP\".\"T1\"+(\"PP\".\"T2\"-\"PP\".\"T1\")*(\"ChiSq\"-\"kws\".\"t1\")/(\"kws\".\"t2\"-\"kws\".\"t1\") ", 
        "WHEN \"_twm_ZPos\" IS NULL AND \"kws\".\"t1\" IS NOT NULL  THEN 0.1 ", "WHEN \"ChiSq\" IS NULL THEN NULL ", "WHEN \"_twm_DoF\" >100 THEN (COALESCE(\"nd\".\"p\", 0.000001)) ", 
        "WHEN \"ChiSq\" > COALESCE(\"chi\".\"t9\", 99) THEN 0.0001 ", "WHEN \"ChiSq\" > COALESCE(\"chi\".\"t8\", 99) THEN \"P\".\"T8\"+(\"P\".\"T9\"-\"P\".\"T8\")*(\"ChiSq\"-\"chi\".\"t8\")/(\"chi\".\"t9\"-\"chi\".\"t8\") ", 
        "WHEN \"ChiSq\" > COALESCE(\"chi\".\"t7\", 99) THEN \"P\".\"T7\"+(\"P\".\"T8\"-\"P\".\"T7\")*(\"ChiSq\"-\"chi\".\"t7\")/(\"chi\".\"t8\"-\"chi\".\"t7\") ", 
        "WHEN \"ChiSq\" > COALESCE(\"chi\".\"t6\", 99) THEN \"P\".\"T6\"+(\"P\".\"T7\"-\"P\".\"T6\")*(\"ChiSq\"-\"chi\".\"t6\")/(\"chi\".\"t7\"-\"chi\".\"t6\") ", 
        "WHEN \"ChiSq\" > COALESCE(\"chi\".\"t5\", 99) THEN \"P\".\"T5\"+(\"P\".\"T6\"-\"P\".\"T5\")*(\"ChiSq\"-\"chi\".\"t5\")/(\"chi\".\"t6\"-\"chi\".\"t5\") ", 
        "WHEN \"ChiSq\" > COALESCE(\"chi\".\"t4\", 99) THEN \"P\".\"T4\"+(\"P\".\"T5\"-\"P\".\"T4\")*(\"ChiSq\"-\"chi\".\"t4\")/(\"chi\".\"t5\"-\"chi\".\"t4\") ", 
        "WHEN \"ChiSq\" > COALESCE(\"chi\".\"t3\", 99) THEN \"P\".\"T3\"+(\"P\".\"T4\"-\"P\".\"T3\")*(\"ChiSq\"-\"chi\".\"t3\")/(\"chi\".\"t4\"-\"chi\".\"t3\") ", 
        "WHEN \"ChiSq\" > COALESCE(\"chi\".\"t2\", 99) THEN \"P\".\"T2\"+(\"P\".\"T3\"-\"P\".\"T2\")*(\"ChiSq\"-\"chi\".\"t2\")/(\"chi\".\"t3\"-\"chi\".\"t2\") ", 
        "WHEN \"ChiSq\" > COALESCE(\"chi\".\"t1\", 99) THEN \"P\".\"T1\"+(\"P\".\"T2\"-\"P\".\"T1\")*(\"ChiSq\"-\"chi\".\"t1\")/(\"chi\".\"t2\"-\"chi\".\"t1\") ", 
        "ELSE 0.25 END) AS \"KruskalWallisPValue\" ", ",(CASE \tWHEN \"_twm_ZPos\" IS NULL AND \"kws\".\"t3\" IS NOT NULL AND \"ChiSq\" > \"kws\".\"t3\" THEN '<0.01' ", 
        "WHEN \"_twm_ZPos\" IS NULL AND \"kws\".\"t2\" IS NOT NULL AND \"ChiSq\" > \"kws\".\"t2\" THEN NULL ", "WHEN \"_twm_ZPos\" IS NULL AND \"kws\".\"t1\" IS NOT NULL AND \"ChiSq\" > \"kws\".\"t1\" THEN NULL ", 
        "WHEN \"_twm_ZPos\" IS NULL AND \"kws\".\"t1\" IS NOT NULL  THEN '>0.1' ", "WHEN \"ChiSq\" IS NULL THEN NULL ", 
        "WHEN \"_twm_DoF\" >100 THEN NULL ", "WHEN \"ChiSq\" > COALESCE(\"chi\".\"t9\", 99) THEN '<0.0001' ", "WHEN \"ChiSq\" > COALESCE(\"chi\".\"t1\", 99) THEN NULL ", 
        "ELSE '>0.25' END) AS \"KruskalWallisPText\" ", ",CASE \tWHEN \"KruskalWallisPValue\" IS NULL THEN  NULL ", "WHEN \"KruskalWallisPValue\" <= `thresh` THEN 'p' ", 
        "ELSE 'a' END AS \"KruskalWallisCallP_`thresh`\" ", "FROM (SELECT SUM(\"_twm_Nall\") AS \"_twm_N\" ", ",SUM(\"_twm_SumRnk\"*\"_twm_SumRnk\"/\"_twm_Nall\") AS \"_twm_Ravg\" ", 
        ",\"_twm_N\"*(\"_twm_N\"+1)*(\"_twm_N\"+1)/4E0 AS \"_twm_mu\" ", ",NULLIFZERO(SUM(\"_twm_SumRnkSq\") - \"_twm_mu\")/NULLIFZERO(\"_twm_N\" - 1E0) AS \"_twm_Ssquared\" ", 
        ",(\"_twm_Ravg\" - \"_twm_mu\")/\"_twm_Ssquared\" AS \"_twm_T\" ", ",COUNT(DISTINCT \"_twm_Sad\") -1 AS \"_twm_DoF\" ", 
        ",CASE WHEN \"_twm_DoF\" = 2  THEN MAX(\"_twm_Nall\") ELSE NULL END AS \"_twm_hi\" ", ",CASE WHEN \"_twm_DoF\" = 2  THEN MIN(\"_twm_Nall\") ELSE NULL END AS \"_twm_lo\" ", 
        ",(SUM(CASE WHEN \"_twm_Nall_index\" = 1 THEN \"_twm_Nall\" ELSE 0 END)) AS \"_twm_Nall1\" ", ",(SUM(CASE WHEN \"_twm_Nall_index\" = 2 THEN \"_twm_Nall\" ELSE 0 END)) AS \"_twm_Nall2\" ", 
        ",(SUM(CASE WHEN \"_twm_Nall_index\" = 3 THEN \"_twm_Nall\" ELSE 0 END)) AS \"_twm_Nall3\" ", ",CASE \tWHEN \"_twm_lo\" < \"_twm_Nall1\" AND \"_twm_Nall1\" < \"_twm_hi\" THEN \"_twm_Nall1\" ", 
        "WHEN \"_twm_lo\" < \"_twm_Nall2\" AND \"_twm_Nall2\" < \"_twm_hi\" THEN \"_twm_Nall2\" ", "WHEN \"_twm_lo\" < \"_twm_Nall3\" AND \"_twm_Nall3\" < \"_twm_hi\" THEN \"_twm_Nall3\" ", 
        "ELSE NULL END AS \"_twm_mid\" ", ",CASE \tWHEN (\"_twm_T\" >0 and \"_twm_DoF\">2) THEN (Exp(LN(\"_twm_T\"/\"_twm_DoF\")/3) -1 + 2/(9E0*\"_twm_DoF\") )/SQRT(2/(9E0*\"_twm_DoF\")) ", 
        "WHEN \"_twm_DoF\"=2 THEN NULL ", "ELSE NULL END AS \"_twm_Zpos\" ", "FROM (SELECT \"_twm_Sad\",\"_twm_Nall\" ", 
        ",SUM(1) OVER (  ORDER BY \"_twm_Sad\" ROWS UNBOUNDED PRECEDING) AS \"_twm_Nall_index\" ", ",\"_twm_SumRnk\",\"_twm_SumRnkSq\" ", 
        "FROM (SELECT \"_twm_Sad\",SUM(1) AS \"_twm_Nall\" ", ",SUM(\"_twm_Rnk\") AS \"_twm_SumRnk\" ", ",SUM(\"_twm_Rnk\"*\"_twm_Rnk\") AS \"_twm_SumRnkSq\" ", 
        "FROM (SELECT \"T7\".\"_twm_Sad\" ", ",\"T7\".\"_twm_Valu\" ", ",\"T4\".\"_twm_rnk\" AS \"_twm_Rnk\" ", "FROM (SELECT AVG(\"_twm_Rnk\") AS \"_twm_rnk\",\"_twm_Valu\" ", 
        "FROM `subq` SQ1 GROUP BY \"_twm_Valu\") AS \"T4\", ", "`subq` T7 ", "WHERE \"T4\".\"_twm_Valu\" = \"T7\".\"_twm_Valu\" ", 
        ") AS \"T3\" GROUP BY \"_twm_Sad\") AS \"T2\") AS \"T1\") AS \"T0\" ", "left outer join `kwobj` \"kws\" ON (\"kws\".\"k1\"=\"_twm_hi\" AND \"kws\".\"k2\"=\"_twm_mid\" AND \"kws\".\"k3\"=\"_twm_lo\") ", 
        "left outer join `chisqobj` \"chi\" ON \"chi\".\"k\"=\"_twm_DoF\" ", "left outer join `nrmobj` \"nd\" ON \"nd\".\"z\"*1000(DECIMAL(10,0)) = -ABS(\"_twm_ZPos\")*1000(DECIMAL(10,0)) ", 
        ",(SEL \"T9\",\"T8\",\"T7\",\"T6\",\"T5\",\"T4\",\"T3\",\"T2\",\"T1\" FROM `chisqobj` \"chi\" where \"chi\".\"k\"=0)  AS \"P\" ", 
        ",(SEL \"T3\",\"T2\",\"T1\" FROM `kwobj` \"kws\" where \"kws\".\"k1\"=0)  AS \"PP\" ", sep = "")
    query <- gsub("`subq`", subq, query)
    query <- gsub("`obj`", obj, query)
    query <- gsub("`nrmobj`", nrmobj, query)
    query <- gsub("`kwobj`", kwobj, query)
    query <- gsub("`chisqobj`", chisqobj, query)
    query <- gsub("`col1`", col1, query)
    query <- gsub("`col2list`", paste(paste("\"", cols2, "\"", sep = ""), collapse = ","), query)
    col2where <- paste(paste("T1.\"", cols2, "\" = ", "T2.\"", cols2, "\"", sep = ""), collapse = " AND ")
    query <- gsub("`col2where`", col2where, query)
    query <- gsub("`thresh`", thresh, query)
}

.td.gennrep <- function(col, as = "XNULLREP", rType = "literal", rVal = 0) {
    if (rType == "literal") 
        if (is.numeric(rVal)) 
            x <- gettextf("COALESCE( \"%s\", %s) AS \"%s\"", col, rVal, as) else x <- gettextf("COALESCE( \"%s\", '%s') AS \"%s\"", col, rVal, as) else if (rType == "mean") 
        x <- gettextf("COALESCE( \"%s\", xmean) AS \"%s\"", col, as) else if (rType == "mode") 
        x <- gettextf("COALESCE( \"%s\", xmode) AS \"%s\"", col, as) else if (rType == "median") 
        x <- gettextf("COALESCE( \"%s\", \"median\") AS \"%s\"", col, as) else x <- gettextf("COALESCE( \"%s\", %s) AS \"%s\"", col, rVal, as)
    return(x)
}

.td.genoverlap <- function(tdf1, tdf2, fields1, fields2) {
    obj1 <- .td.object(attr(tdf1, "tableName"), attr(tdf1, "database"))
    wc1 <- ""
    if (!is.null(attr(tdf1, "whereClause"))) 
        wc1 <- paste(" WHERE ", attr(tdf1, "whereClause"))
    obj2 <- .td.object(attr(tdf2, "tableName"), attr(tdf2, "database"))
    wc2 <- ""
    if (!is.null(attr(tdf2, "whereClause"))) 
        wc2 <- paste(" WHERE ", attr(tdf2, "whereClause"))
    tbl1 <- attr(tdf1, "tableName")
    tbl2 <- attr(tdf2, "tableName")
    
    recs <- gettextf(paste("SELECT 1 AS xidx, '#records     ' AS xtable, \"%s\", \"%s\" FROM ", "(SELECT CAST(COUNT(*) AS FLOAT) AS \"%s\" FROM %s %s) T1 ", 
        ", (SELECT CAST(COUNT(*) AS FLOAT) AS \"%s\" FROM %s %s) T2"), tbl1, tbl2, tbl1, obj1, wc1, tbl2, obj2, wc2)
    uniqs <- gettextf(paste("SELECT 2, '#uniques     ', \"%s\", \"%s\" FROM ", "(SELECT CAST(COUNT(DISTINCT %s) AS FLOAT) AS \"%s\" FROM %s %s) T1 ", 
        ", (SELECT CAST(COUNT(DISTINCT %s) AS FLOAT) AS \"%s\" FROM %s %s) T2 "), tbl1, tbl2, paste(fields1, collapse = "||"), 
        tbl1, obj1, wc1, fields2, tbl2, obj2, wc2)
    dists <- gettextf(paste("SELECT 3, '%s', NULL, \"%s\" FROM ", "(SELECT CAST(COUNT(DISTINCT c1) AS FLOAT) AS \"%s\" FROM ", 
        "(SELECT DISTINCT %s AS c1 FROM %s %s) A, ", "(SELECT DISTINCT %s AS c2 FROM %s %s) B ", "WHERE c1 = c2) T2 "), 
        tbl1, tbl2, tbl2, paste(fields1, collapse = "||"), obj1, wc1, paste(fields2, collapse = "||"), obj2, wc2)
    query <- gettextf("%s UNION %s UNION %s ORDER BY 1", recs, uniqs, dists)
    return(query)
}

.td.genquantiles <- function(tdf, col, qType = c("quar", "ter", "dec")) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" WHERE ", attr(tdf, "whereClause"))
    clist <- character(0)
    if ("ter" %in% qType) 
        clist <- c(clist, paste(gettextf("MAX(CASE WHEN qtile <= %d THEN %s ELSE NULL END) AS %s", c(33, 67), col, c("ter1", 
            "ter2"))))
    if ("quar" %in% qType) 
        clist <- c(clist, paste(gettextf("MAX(CASE WHEN qtile <= %d THEN %s ELSE NULL END) AS %s", c(25, 50, 75), col, 
            c("quar1", "quar2", "quar3"))))
    if ("dec" %in% qType) 
        clist <- c(clist, paste(gettextf("MAX(CASE WHEN qtile <= %d THEN %s ELSE NULL END) AS %s", seq(10, 90, by = 10), 
            col, paste("dec", 1:9, sep = ""))))
    query <- gettextf(paste("SELECT ", paste(clist, collapse = ","), " FROM (SELECT DISTINCT %s, ", "((RANK() OVER (ORDER BY %s ) - 1) * 100) / COUNT(*) OVER ", 
        "(ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS qtile ", "FROM %s %s) A", sep = ""), col, col, obj, 
        wc)
    return(query)
}

.td.genquartiles <- function(tdf, col, cnt) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    clist <- character(0)
    clist <- c(clist, paste(gettextf("MAX(CASE WHEN qtile <= %d THEN cval ELSE NULL END) AS %s", c(round(0.25 * cnt), round(0.75 * 
        cnt)), c("quar1", "quar3"))))
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" AND ", attr(tdf, "whereClause"))
    
    if (cnt%%2 == 0) {
        clist <- c(clist, paste(gettextf(paste("(MAX(CASE WHEN qtile <= %d THEN cval ELSE NULL END) + ", "MAX(CASE WHEN qtile <= %d THEN cval ELSE NULL END)) / 2 as %s "), 
            cnt/2, cnt/2 + 1, "med1")))
    } else clist <- c(clist, paste(gettextf("MAX(CASE WHEN qtile <= %d THEN cval ELSE NULL END) AS %s", (cnt + 1)/2, "med1")))
    
    query <- gettextf(paste("SELECT ", paste(clist, collapse = ","), " FROM (SELECT DISTINCT \"%s\" as cval, ", "RANK() OVER (ORDER BY \"%s\" ) ", 
        " AS qtile ", "FROM %s WHERE cval IS NOT NULL %s) A", sep = ""), col, col, obj, wc)
    return(query)
}

.td.genrank <- function(tdf, col) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    wc <- ""
    if (!is.null(attr(tdf, "whereClause"))) 
        wc <- paste(" WHERE ", attr(tdf, "whereClause"))
    query <- gettextf(paste("SELECT '%s' AS \"xcol\", %s,%s,%s,%s FROM ", "(SELECT %s,%s,%s,%s FROM ", "(SELECT \"%s\",\"xcnt\"", 
        ",CASE WHEN \"%s\" IS NULL THEN NULL ELSE RANK() ", "OVER(PARTITION BY  CASE WHEN \"%s\" IS NULL THEN NULL ELSE 0 END ", 
        "ORDER BY \"%s\" ASC) END AS \"rnk_a\"", ",CASE WHEN \"%s\" IS NULL THEN NULL ELSE RANK() ", "OVER(PARTITION BY  CASE WHEN \"%s\" IS NULL THEN NULL ELSE 0 END ", 
        "ORDER BY \"%s\" DESC) END AS \"rnk_d\" ", "FROM (SELECT \"%s\" ,CAST(COUNT(*) AS FLOAT) AS \"xcnt\" ", "FROM %s %s GROUP BY \"%s\") A) B )T5", 
        sep = ""), col, paste(paste("xmin_", 1:5, sep = ""), collapse = ","), paste(paste("xmincnt_", 1:5, sep = ""), collapse = ","), 
        paste(paste("xmax_", 5:1, sep = ""), collapse = ","), paste(paste("xmaxcnt_", 5:1, sep = ""), collapse = ","), 
        paste(paste("MIN(CASE WHEN rnk_a = ", 1:5, " THEN \"", col, "\" ELSE NULL END) AS \"xmin_", 1:5, "\"", sep = ""), 
            collapse = ","), paste(paste("MIN(CASE WHEN rnk_d = ", 5:1, " THEN \"", col, "\" ELSE NULL END) AS \"xmax_", 
            5:1, "\"", sep = ""), collapse = ","), paste(paste("MIN(CASE WHEN rnk_a = ", 1:5, " THEN \"xcnt\"", " ELSE NULL END) AS \"xmincnt_", 
            1:5, "\"", sep = ""), collapse = ","), paste(paste("MIN(CASE WHEN rnk_d = ", 5:1, " THEN \"xcnt\"", " ELSE NULL END) AS \"xmaxcnt_", 
            5:1, "\"", sep = ""), collapse = ","), col, col, col, col, col, col, col, col, obj, wc, col)
    return(query)
}

.td.genrecode <- function(col, codes, as = "XCODE", other = "same") {
    lst <- c()
    for (i in 1:length(codes)) {
        if (i%%2 == 0) {
            if (is.numeric(codes)) 
                lst[i/2] <- gettextf("WHEN \"%s\" = %s THEN %s", col, prev, codes[i]) else lst[i/2] <- gettextf("WHEN \"%s\" = '%s' THEN '%s'", col, prev, codes[i])
        } else prev <- codes[i]
    }
    if (other == "same") 
        return(gettextf("CASE %s ELSE \"%s\" END AS %s", paste(lst, collapse = " "), col, as))
    if (other == "null") 
        return(gettextf("CASE %s ELSE NULL END AS %s", paste(lst, collapse = " "), as))
    stop("Unknown value for 'other'")
}

.td.genrescale <- function(col, lower = 0, upper = 1, as = "XSCALE", xmin = "XMIN", xmax = "XMAX") {
    diff <- upper - lower
    x <- gettextf("%f + (CAST(\"%s\" AS FLOAT) - XMIN) * %f / (XMAX - XMIN) AS \"%s\"", lower, col, diff, as)
    if (xmin != "XMIN" || xmax != "XMAX") {
        x <- gsub("XMIN", xmin, x)
        x <- gsub("XMAX", xmax, x)
    }
    return(x)
}

.td.genshapirowilk <- function(tdf, col, thresh = 0.05) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    nrmobj <- .td.locateObject("TWMS_Normal")
    swqobj <- .td.locateObject("TWMS_ShapiroWilkQuantiles")
    swcobj <- .td.locateObject("TWMS_ShapiroWilkCoefficients")
    subq <- paste("(SELECT \"`col`\" AS \"_twm_Valu\" ", ",SUM(1) OVER (  ORDER BY \"`col`\" ROWS UNBOUNDED PRECEDING) AS \"_twm_Rnk\" ", 
        "FROM `obj`)")
    query <- paste("SELECT \"_twm_T\" AS \"Shw\",\"T00\".\"_twm_N\" AS \"N\" ", ",CASE \tWHEN \"T00\".\"_twm_N\" >2000  THEN NULL ", 
        "WHEN \"T00\".\"_twm_N\" <3  THEN NULL ", "WHEN \"T00\".\"_twm_N\" >50  THEN (COALESCE(\"nd\".\"p\", 0.000001)) ", 
        "WHEN \"_twm_T\" > COALESCE(\"shw\".\"t9\", 99) THEN 0.99 ", "WHEN \"_twm_T\" > COALESCE(\"shw\".\"t8\", 99) THEN \"P\".\"T8\"+(\"P\".\"T9\"-\"P\".\"T8\")*(\"Shw\"-\"shw\".\"t8\")/(\"shw\".\"t9\"-\"shw\".\"t8\") ", 
        "WHEN \"_twm_T\" > COALESCE(\"shw\".\"t7\", 99) THEN \"P\".\"T7\"+(\"P\".\"T8\"-\"P\".\"T7\")*(\"Shw\"-\"shw\".\"t7\")/(\"shw\".\"t8\"-\"shw\".\"t7\") ", 
        "WHEN \"_twm_T\" > COALESCE(\"shw\".\"t6\", 99) THEN \"P\".\"T6\"+(\"P\".\"T7\"-\"P\".\"T6\")*(\"Shw\"-\"shw\".\"t6\")/(\"shw\".\"t7\"-\"shw\".\"t6\") ", 
        "WHEN \"_twm_T\" > COALESCE(\"shw\".\"t5\", 99) THEN \"P\".\"T5\"+(\"P\".\"T6\"-\"P\".\"T5\")*(\"Shw\"-\"shw\".\"t5\")/(\"shw\".\"t6\"-\"shw\".\"t5\") ", 
        "WHEN \"_twm_T\" > COALESCE(\"shw\".\"t4\", 99) THEN \"P\".\"T4\"+(\"P\".\"T5\"-\"P\".\"T4\")*(\"Shw\"-\"shw\".\"t4\")/(\"shw\".\"t5\"-\"shw\".\"t4\") ", 
        "WHEN \"_twm_T\" > COALESCE(\"shw\".\"t3\", 99) THEN \"P\".\"T3\"+(\"P\".\"T4\"-\"P\".\"T3\")*(\"Shw\"-\"shw\".\"t3\")/(\"shw\".\"t4\"-\"shw\".\"t3\") ", 
        "WHEN \"_twm_T\" > COALESCE(\"shw\".\"t2\", 99) THEN \"P\".\"T2\"+(\"P\".\"T3\"-\"P\".\"T2\")*(\"Shw\"-\"shw\".\"t2\")/(\"shw\".\"t3\"-\"shw\".\"t2\") ", 
        "WHEN \"_twm_T\" > COALESCE(\"shw\".\"t1\", 99) THEN \"P\".\"T1\"+(\"P\".\"T2\"-\"P\".\"T1\")*(\"Shw\"-\"shw\".\"t1\")/(\"shw\".\"t2\"-\"shw\".\"t1\") ", 
        "ELSE 0.01 END AS \"ShapiroWilkPValue\" ", ",CASE \tWHEN \"T00\".\"_twm_N\" >2000  THEN NULL ", "WHEN \"T00\".\"_twm_N\" <3  THEN NULL ", 
        "WHEN \"T00\".\"_twm_N\" >50  THEN NULL ", "WHEN \"_twm_T\" > COALESCE(\"shw\".\"t9\", 99) THEN '>0.99' ", "WHEN \"_twm_T\" > COALESCE(\"shw\".\"t1\", 99) THEN NULL ", 
        "ELSE '<0.01' END AS \"ShapiroWilkPText\" ", ",CASE \tWHEN \"ShapiroWilkPValue\" IS NULL THEN  NULL ", "WHEN \"ShapiroWilkPValue\" <= `thresh` THEN 'p' ", 
        "ELSE 'a' END AS \"ShapiroWilkCallP_`thresh`\" ", "FROM ", "( ", "SELECT ", "(SUM(\"_twm_Coeff\"*(\"_twm_V2\"-\"_twm_V1\"))*SUM(\"_twm_Coeff\"*(\"_twm_V2\"-\"_twm_V1\")))/NULLIFZERO(MAX(\"_twm_Vx\")) AS \"_twm_T\" ", 
        ",MAX(\"_twm_NS\") AS \"_twm_N\" ", ",CASE WHEN \"_twm_N\"<21 THEN 3 ELSE 5 END AS \"_twm_d\" ", ",LN(\"_twm_N\")-\"_twm_d\" AS \"_twm_g\" ", 
        ",CASE WHEN \"_twm_N\"<21 THEN 0.118898+0.133414*\"_twm_g\"+0.327907*\"_twm_g\"*\"_twm_g\" ELSE 0.480385+0.318828*\"_twm_g\"-0.0241665*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"+0.00879701*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"+0.002989646*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\" END AS \"_twm_lamda\" ", 
        ",CASE WHEN \"_twm_N\"<21 THEN EXP(-0.37542-0.492145*\"_twm_g\"-1.124332*\"_twm_g\"*\"_twm_g\"-0.199422*\"_twm_g\"*\"_twm_g\"*\"_twm_g\") ELSE EXP(-1.91487-1.37888*\"_twm_g\"-0.04183209*\"_twm_g\"*\"_twm_g\"+0.1066339*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"-0.03513666*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"-0.01504614*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\") END AS \"_twm_mu\" ", 
        ",CASE WHEN \"_twm_N\"<21 THEN EXP(-3.15805+0.729399*\"_twm_g\"+3.01855*\"_twm_g\"*\"_twm_g\"+1.558776*\"_twm_g\"*\"_twm_g\"*\"_twm_g\") ELSE EXP(-3.73538-1.015807*\"_twm_g\"-0.331885*\"_twm_g\"*\"_twm_g\"+0.1773538*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"-0.01638782*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"-0.03215018*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"+0.003852646*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\"*\"_twm_g\") END AS \"_twm_sigma\" ", 
        ",(EXP(LN(1-\"_twm_T\")*\"_twm_lamda\")-\"_twm_mu\")/NULLIFZERO(\"_twm_sigma\") AS \"_twm_zT\" ", "FROM ", "( ", 
        "SELECT ", "COUNT(*) AS \"_twm_NS\" ", "FROM `subq` SQ1 ", ") AS \"TA\", ", "( ", "SELECT \"_twm_Valu\" AS \"_twm_V1\",\"_twm_Rnk\" AS \"_twm_i\" ", 
        "FROM `subq` SQ1 ", ") AS \"TB\", ", "( ", "SELECT ", "\"_twm_Valu\" AS \"_twm_V2\" ", ",\"_twm_Rnk\" AS \"_twm_j\" ", 
        "FROM `subq` SQ1 ", ") AS \"TC\", ", "( ", "SELECT ", "\"N\" AS \"_twm_tN\" ", ",\"I\" AS \"_twm_Ti\" ", ",\"Coeff\" AS \"_twm_Coeff\" FROM `swcobj` ", 
        ") AS \"TD\", ", "( ", "SELECT ", "VAR_POP(\"_twm_Valu\")*COUNT(*) AS \"_twm_Vx\" ", "FROM `subq` SQ1 ", ") AS \"TE\" ", 
        "WHERE \"_twm_i\" = \"_twm_NS\"-\"_twm_j\"+1 AND \"_twm_i\" < \"_twm_NS\"/2+1 AND \"_twm_NS\" = \"_twm_tN\" AND \"_twm_i\" = \"_twm_Ti\" ", 
        ") AS \"T00\" ", "LEFT OUTER JOIN `nrmobj` \"nd\" ", "ON \"nd\".\"z\"*1000(DECIMAL(10,0)) = -ABS(\"_twm_zT\")*1000(DECIMAL(10,0)) ", 
        "LEFT OUTER JOIN `swqobj` \"shw\" ", "ON (\"shw\".\"n\" = \"T00\".\"_twm_N\"), ", "( ", "SELECT \"T9\",\"T8\",\"T7\",\"T6\",\"T5\",\"T4\",\"T3\",\"T2\",\"T1\" ", 
        "FROM `swqobj` AS \"shw\" ", "WHERE \"shw\".\"n\" = 0 ", ") AS \"P\"", sep = "")
    query <- gsub("`subq`", subq, query)
    query <- gsub("`obj`", obj, query)
    query <- gsub("`nrmobj`", nrmobj, query)
    query <- gsub("`swqobj`", swqobj, query)
    query <- gsub("`swcobj`", swcobj, query)
    query <- gsub("`col`", col, query)
    query <- gsub("`thresh`", thresh, query)
    return(query)
}

.td.gensigmoid <- function(col, sigmoidType = "logit", as = "XSIG") {
    if (sigmoidType == "logit") {
        x <- gettextf(paste("CASE \tWHEN \"%s\" >= 36  THEN 1.0E0 ", "WHEN \"%s\" < -36  THEN 0.0E0 ", "ELSE 1 / (1+EXP(-CAST(\"%s\" AS FLOAT))) END AS \"%s\""), 
            col, col, col, as)
    }
    if (sigmoidType == "mlogit") {
        x <- gettextf(paste("CASE \tWHEN \"%s\" >= 36  THEN 1.0E0 ", "WHEN \"%s\" < -36  THEN 0.0E0 ", "ELSE 2 / (1+EXP(-CAST(\"%s\" AS FLOAT))) - 1 END AS \"%s\""), 
            col, col, col, as)
    }
    if (sigmoidType == "tanh") 
        x <- gettextf("TANH(\"%s\") AS \"%s\"", col, as)
    return(x)
}

.td.gensmirnov <- function(tdf, col1, col2, thresh = 0.05) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    smirobj <- .td.locateObject("TWMS_Smirnov")
    subq <- paste("(SELECT \"`col1`\" AS \"_twm_Valu\",\"_twm_Sad\" AS \"_twm_Sad\" ", ",CAST(SUM(1) OVER (  ORDER BY \"`col1`\" ROWS UNBOUNDED PRECEDING) AS FLOAT) AS \"_twm_Rnk\" ", 
        "FROM `obj` \"T1\", ", "(SELECT \"`col2`\" ", ",SUM(1) OVER (ORDER BY \"`col2`\" ROWS UNBOUNDED PRECEDING) AS \"_twm_sad\" ", 
        "FROM (SELECT \"`col2`\" ", "FROM `obj` ", "GROUP BY \"`col2`\") AS \"T3\") AS \"T2\" ", "where \"T1\".\"`col2`\" = \"T2\".\"`col2`\")", 
        sep = "")
    query <- paste("SELECT ", "MAX(\"T1\".\"_twm_M\") AS \"M\" ", ",MAX(\"T1\".\"_twm_N\") AS \"N\" ", ",MAX(\"T1\".\"_twm_D\") AS \"D\" ", 
        ",CASE \tWHEN MIN(\"PVal\") IS NOT NULL THEN MIN(\"PVal\") ", "WHEN MAX(\"_twm_Asy\") <= 1.07 THEN 0.2 ", "WHEN MAX(\"_twm_Asy\") <= 1.22 THEN 0.1  +  (0.2-0.1)*(MAX(\"_twm_Asy\")- 1.22)/(1.07- 1.22) ", 
        "WHEN MAX(\"_twm_Asy\") <= + 1.36 THEN 0.05 + (0.1-0.05)*(MAX(\"_twm_Asy\")- 1.36)/(1.22- 1.36) ", "WHEN MAX(\"_twm_Asy\") <= 1.52 THEN 0.01 + (0.05-0.01)*(MAX(\"_twm_Asy\")- 1.52)/(1.36- 1.52) ", 
        "WHEN MAX(\"_twm_Asy\") <= + 1.63 THEN 0.005 +(0.01-0.005)*(MAX(\"_twm_Asy\")- 1.63)/(1.52- 1.63) ", "ELSE 0.005 END AS \"SmirnovPValue\" ", 
        ",CASE \tWHEN MIN(\"PVal\") IS NOT NULL THEN NULL ", "WHEN MAX(\"_twm_Asy\") <= + 1.07 THEN '>0.2' ", "WHEN MAX(\"_twm_Asy\") <= 1.63 THEN NULL ", 
        "ELSE '<0.005' END AS \"SmirnovPText\" ", ",CASE \tWHEN \"SmirnovPValue\" IS NULL THEN  NULL ", "WHEN \"SmirnovPValue\" <= `thresh` THEN 'p' ", 
        "ELSE 'a' END AS \"SmirnovCallP_`thresh`\" ", "FROM ", "( ", "SELECT ", "CASE WHEN MAX(\"_twm_N1\")<MAX(\"_twm_N2\") THEN MAX(\"_twm_N1\") ELSE MAX(\"_twm_N2\") END AS \"_twm_M\" ", 
        ",CASE WHEN MAX(\"_twm_N1\")<MAX(\"_twm_N2\") THEN MAX(\"_twm_N2\") ELSE MAX(\"_twm_N1\") END AS \"_twm_N\" ", 
        ",\"_twm_M\"*\"_twm_N\"*MAX(ABS(\"_twm_s1\"-\"_twm_s2\")) AS \"_twm_c\" ", ",\"_twm_c\"-\"_twm_M\"/2E0 AS \"_twm_cc\" ", 
        ",\"_twm_M\"*\"_twm_N\"*MAX(ABS(\"_twm_s1\"-\"_twm_s2\"))/(\"_twm_M\"*\"_twm_N\") AS \"_twm_D\" ", ",CASE WHEN (\"_twm_M\">=86 OR \"_twm_N\">=86) THEN \"_twm_D\"/SQRT((\"_twm_M\"+\"_twm_N\")/(CAST(1 AS FLOAT)*\"_twm_M\"*\"_twm_N\")) ELSE NULL END AS \"_twm_Asy\" ", 
        "FROM ", "( ", "SELECT ", "\"T3A\".\"_twm_sad\" ", ",\"_twm_valu\" ", ",CASE WHEN \"T3A\".\"_twm_sad\" = 1 THEN \"_twm_N\" ELSE NULL END AS \"_twm_N1\" ", 
        ",CASE WHEN \"T3A\".\"_twm_sad\" = 2 THEN \"_twm_N\" ELSE NULL END AS \"_twm_N2\" ", ",1/CAST(\"_twm_N1\" AS FLOAT) AS \"_twm_f1\" ", 
        ",1/CAST(\"_twm_N2\" AS FLOAT) AS \"_twm_f2\" ", ",SUM(CASE WHEN \"T3A\".\"_twm_sad\" = 1 THEN \"_twm_f1\" ELSE 0 END) OVER(ORDER BY\"_twm_valu\", \"T3A\".\"_twm_sad\" ROWS UNBOUNDED PRECEDING) AS \"_twm_s1\" ", 
        ",SUM(CASE WHEN \"T3A\".\"_twm_sad\" = 2 THEN \"_twm_f2\" ELSE 0 END) OVER(ORDER BY\"_twm_valu\", \"T3A\".\"_twm_sad\" ROWS UNBOUNDED PRECEDING) AS \"_twm_s2\" ", 
        ",ABS(\"_twm_s1\"-\"_twm_s2\") AS \"_twm_diff\" ", "FROM `subq` AS \"T3A\" , ", "( ", "SELECT ", "\"_twm_sad\" ", 
        ",COUNT(*) AS \"_twm_N\" ", "FROM `subq` SQ1 ", "GROUP BY \"_twm_sad\" ", ") AS \"T3B\" ", "WHERE \"T3A\".\"_twm_sad\" = \"T3B\".\"_twm_sad\" ", 
        ") AS \"T2\" ", ") AS \"T1\" ", "LEFT OUTER JOIN `smirobj` \"KS\" ", "ON (\"KS\".\"n\"=\"T1\".\"_twm_N\" AND \"KS\".\"m\"=\"T1\".\"_twm_M\" AND \"KS\".\"c\"<=\"T1\".\"_twm_cc\") ", 
        sep = "")
    query <- gsub("`subq`", subq, query)
    query <- gsub("`obj`", obj, query)
    query <- gsub("`smirobj`", smirobj, query)
    query <- gsub("`col1`", col1, query)
    query <- gsub("`col2`", col2, query)
    query <- gsub("`thresh`", thresh, query)
}

.td.genstats <- function(col, statlist = "all", type = "population") {
    if (length(statlist) == 1L && statlist == "all") 
        statlist <- c("cnt", "min", "max", "mean", "sum", "uss", "css", "var", "std", "skew", "kurtosis", "stderr", "cvar")
    vec <- character(length(statlist))
    if ("cnt" %in% statlist) {
        vec[which(statlist == "cnt")] <- sprintf("CAST(COUNT(%s) AS FLOAT) AS xcnt", col)
    }
    if ("min" %in% statlist) 
        vec[which(statlist == "min")] <- sprintf("MIN(CAST(%s AS FLOAT)) AS xmin", col)
    if ("max" %in% statlist) 
        vec[which(statlist == "max")] <- sprintf("MAX(CAST(%s AS FLOAT)) AS xmax", col)
    if ("mean" %in% statlist) 
        vec[which(statlist == "mean")] <- sprintf("AVG(CAST(%s AS FLOAT)) AS xmean", col)
    if ("sum" %in% statlist) 
        vec[which(statlist == "sum")] <- sprintf("SUM(CAST(%s AS FLOAT)) AS xsum", col)
    if ("uss" %in% statlist) 
        vec[which(statlist == "uss")] <- sprintf("SUM(CAST(%s AS FLOAT)**2) AS xuss", col)
    if ("css" %in% statlist) 
        vec[which(statlist == "css")] <- sprintf(paste("SUM(CAST(%s AS FLOAT)**2)", "-CAST(COUNT(%s) AS FLOAT)*", "AVG(CAST(%s AS FLOAT))**2 AS xcss", 
            sep = ""), col, col, col)
    if (type == "population") {
        if ("var" %in% statlist) 
            vec[which(statlist == "var")] <- sprintf("STDDEV_POP(CAST(%s AS FLOAT))**2 AS xvar", col)
        if ("std" %in% statlist) 
            vec[which(statlist == "std")] <- sprintf("STDDEV_POP(CAST(%s AS FLOAT)) AS xstd", col)
        if ("skew" %in% statlist) 
            vec[which(statlist == "skew")] <- sprintf(paste("((STDDEV_SAMP(CAST(%s AS FLOAT)) ** 3) /", " (NULLIF(STDDEV_POP(CAST(%s AS FLOAT)), 0)", 
                " ** 3)) * SKEW(CAST(%s AS FLOAT)) AS xskew"), col, col, col)
        if ("kurtosis" %in% statlist) 
            vec[which(statlist == "kurtosis")] <- sprintf(paste("(KURTOSIS(CAST(%s AS FLOAT)) * ", "STDDEV_SAMP(CAST(%s AS FLOAT)) ** 4 + ", 
                "(STDDEV_SAMP(CAST(%s AS FLOAT)) ** 4 - ", "STDDEV_POP(CAST(%s AS FLOAT)) ** 4) * ", "(3*(CAST(COUNT(%s) AS FLOAT)-1) ** 2 / ", 
                "((CAST(COUNT(%s) AS FLOAT)-2) * ", "(CAST(COUNT(%s) AS FLOAT) -3)))) / ", "(STDDEV_POP(CAST(%s AS FLOAT)) ** 4) AS xkurt"), 
                col, col, col, col, col, col, col, col)
        if ("stderr" %in% statlist) 
            vec[which(statlist == "stderr")] <- sprintf("STDDEV_POP(CAST(%s AS FLOAT)) / SQRT(CAST(COUNT(%s) AS FLOAT)) AS xstderr", 
                col, col)
        if ("cvar" %in% statlist) 
            vec[which(statlist == "cvar")] <- sprintf(paste("CASE WHEN AVG(CAST(%s AS FLOAT)) = 0 THEN NULL ", "ELSE 100* STDDEV_POP(CAST(%s AS FLOAT)) / ", 
                "AVG(CAST(%s AS FLOAT)) END AS xcvar"), col, col, col)
    } else {
        if ("var" %in% statlist) 
            vec[which(statlist == "var")] <- sprintf("STDDEV_SAMP(CAST(%s AS FLOAT))**2 AS xvar", col)
        if ("std" %in% statlist) 
            vec[which(statlist == "std")] <- sprintf("STDDEV_SAMP(CAST(%s AS FLOAT)) AS xstd", col)
        if ("skew" %in% statlist) 
            vec[which(statlist == "skew")] <- sprintf("SKEW(CAST(%s AS FLOAT)) AS xskew", col)
        if ("kurtosis" %in% statlist) 
            vec[which(statlist == "kurtosis")] <- sprintf("KURTOSIS(CAST(%s AS FLOAT)) AS xkurt", col)
        if ("stderr" %in% statlist) 
            vec[which(statlist == "stderr")] <- sprintf("STDDEV_SAMP(CAST(%s AS FLOAT)) / SQRT(CAST(COUNT(%s AS FLOAT)) AS xstderr", 
                col, col)
        if ("cvar" %in% statlist) 
            vec[which(statlist == "cvar")] <- sprintf(paste("CASE WHEN AVG(CAST(%s AS FLOAT)) = 0 THEN NULL ", "ELSE 100* STDDEV_SAMP(CAST(%s AS FLOAT)) / ", 
                "AVG(CAST(%s AS FLOAT)) END AS xcvar"), col, col, col)
    }
    return(vec)
}

.td.gentpaired <- function(tdf, col1, col2, thresh = 0.05) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    tobj <- .td.locateObject("TWMS_T")
    query <- paste("SELECT \"_twm_DoF\" AS \"D_F\" ,CASE \tWHEN \"p\" IS NULL THEN 0 ", "ELSE \"p\" END AS \"TTestPValue\" ", 
        ",\"T1\".\"_twm_T\" AS \"T\" ", ",CASE \tWHEN \"T1\".\"_twm_T\" > 0 AND \"TTestPValue\" <= `thresh` THEN 'p' ", 
        "WHEN \"TTestPValue\" > `thresh` THEN 'a' ", "WHEN \"T1\".\"_twm_T\" < 0 AND \"TTestPValue\" <= `thresh` THEN 'n' ", 
        "ELSE NULL END AS \"TTestCallP_`thresh`\" ", "FROM (SELECT  ", "\"_twm_Avgdif\"/NULLIFZERO(SQRT(SUM((\"_twm_DifMeans\" - \"_twm_Avgdif\")**2)/NULLIFZERO((\"_twm_N\" - 1)*\"_twm_N\"))) AS \"_twm_T\" ", 
        ",\"_twm_N\" - 1 AS \"_twm_Dof\" ", "FROM (SELECT CAST(COUNT(*) AS FLOAT) AS \"_twm_N\" ", ",(CAST(SUM(\"`col2`\" - \"`col1`\") AS FLOAT))/\"_twm_N\" AS \"_twm_Avgdif\" ", 
        "FROM `obj` ", "WHERE \"`col1`\" IS NOT NULL AND \"`col2`\" IS NOT NULL ", ") AS \"T2\",(SELECT \"`col2`\" - \"`col1`\" AS \"_twm_DifMeans\" ", 
        "FROM `obj` ", "WHERE \"`col1`\" IS NOT NULL AND \"`col2`\" IS NOT NULL) AS \"T3\" ", "WHERE \"_twm_N\" >= 4 GROUP BY \"_twm_N\", \"_twm_Avgdif\") AS \"T1\" ", 
        "LEFT OUTER JOIN `tobj` \"_twm_TD\" ON \"_twm_TD\".\"df\" = \"_twm_Dof\" ", "AND ABS(\"T1\".\"_twm_T\")(DECIMAL(10,2)) = COALESCE(\"_twm_TD\".\"t\", 0.01)(DECIMAL(10,2)) ", 
        sep = "")
    query <- gsub("`obj`", obj, query)
    query <- gsub("`tobj`", tobj, query)
    query <- gsub("`col1`", col1, query)
    query <- gsub("`col2`", col2, query)
    query <- gsub("`thresh`", thresh, query)
}

.td.gentunpaired <- function(tdf, col1, col2, thresh = 0.05) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    tobj <- .td.locateObject("TWMS_T")
    query <- paste("SELECT MAX(\"_twm_Dof\") AS \"D_F\",MAX(\"T1\".\"_twm_T\") AS \"T\" ", ",CASE \tWHEN MAX(\"T1\".\"_twm_T\") IS NULL THEN NULL ", 
        "WHEN MAX(\"p\") IS NULL THEN 0 ", "ELSE MAX(\"p\") - (MAX(\"p\") - MIN(\"p\"))*(MAX(\"T1\".\"_twm_T\") - MAX(\"_twm_Tlo\"))/ ", 
        "(CASE \tWHEN \"D_F\" = 1 THEN 1 ", "WHEN \"D_F\" = 2 THEN 0.1 ", "ELSE 0.01 END) END AS \"TTestPValue\" ", ",CASE \tWHEN MAX(\"T1\".\"_twm_T\") > 0 AND \"TTestPValue\" <= `thresh` THEN 'p' ", 
        "WHEN \"TTestPValue\" > `thresh` THEN 'a' ", "WHEN MAX(\"T1\".\"_twm_T\") < 0 AND \"TTestPValue\" <= `thresh` THEN 'n' ", 
        "ELSE NULL END AS \"TTestCallP_`thresh`\" ", "FROM (SELECT COUNT(\"`col1`\") AS \"_twm_M\",COUNT(\"`col2`\") AS \"_twm_N\" ", 
        ",VAR_POP(\"`col1`\")/NULLIFZERO(\"_twm_M\" - 1) AS \"_twm_V1\" ", ",VAR_POP(\"`col2`\")/NULLIFZERO(\"_twm_N\" - 1) AS \"_twm_V2\" ", 
        ",(\"_twm_V1\" + \"_twm_V2\")**2/NULLIFZERO(\"_twm_V1\"*\"_twm_V1\"/NULLIFZERO(\"_twm_M\" - 1) + \"_twm_V2\"*\"_twm_V2\"/NULLIFZERO(\"_twm_N\" - 1)) AS \"_twm_Dof\" ", 
        ",ABS(AVG(\"`col2`\") - AVG(\"`col1`\"))/NULLIFZERO(SQRT(\"_twm_V1\" + \"_twm_V2\")) AS \"_twm_T\" ", ",CASE \tWHEN \"_twm_Dof\" <121  THEN \"_twm_Dof\" ", 
        "ELSE 1000 END AS \"_twm_MDof\" ", ",\"_twm_T\"(DECIMAL(10,0)) AS \"_twm_Thi0\" ", ",\"_twm_T\"(DECIMAL(10,1)) AS \"_twm_Thi1\" ", 
        ",\"_twm_T\"(DECIMAL(10,2)) AS \"_twm_Thi2\" ", ",CASE \tWHEN \"_twm_Dof\" =1  THEN \"_twm_Thi0\" ", "WHEN \"_twm_Dof\" =2  THEN \"_twm_Thi1\" ", 
        "ELSE \"_twm_Thi2\" END AS \"_twm_Thi\" ", ",CASE \tWHEN \"_twm_Dof\" =1  THEN \"_twm_Thi\"-1 ", "WHEN \"_twm_Dof\" =2  THEN \"_twm_Thi\"-0.1 ", 
        "ELSE \"_twm_Thi\"-0.01 END AS \"_twm_Tlo\" ", "FROM `obj`) AS \"T1\" ", "LEFT OUTER JOIN `tobj` \"_twm_TD\" ON \"_twm_TD\".\"df\" = (CAST(\"_twm_MDof\" AS INTEGER)) ", 
        "AND (\"_twm_Thi\" = COALESCE(\"_twm_TD\".\"t\", 0.01)(DECIMAL(10,2)) ", "OR \"_twm_Tlo\" = COALESCE(\"_twm_TD\".\"t\", 0.01)(DECIMAL(10,2))) ", 
        sep = "")
    query <- gsub("`obj`", obj, query)
    query <- gsub("`tobj`", tobj, query)
    query <- gsub("`col1`", col1, query)
    query <- gsub("`col2`", col2, query)
    query <- gsub("`thresh`", thresh, query)
}

.td.gentunpairedi <- function(tdf, col1, col2, thresh = 0.05) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    tobj <- .td.locateObject("TWMS_T")
    subq <- paste("(SELECT CASE WHEN \"`col2`\"<=0 THEN `col1` ELSE NULL END AS \"_twm_x1\" ", ",CASE WHEN \"`col2`\">0 THEN \"`col1`\" ELSE NULL END AS \"_twm_x2\" ", 
        "FROM `obj`)", sep = "")
    query <- paste("SELECT MAX(\"_twm_Dof\") AS \"D_F\",MAX(\"T1\".\"_twm_T\") AS \"T\" ", ",CASE \tWHEN MAX(\"T1\".\"_twm_T\") IS NULL THEN NULL ", 
        "WHEN MAX(\"p\") IS NULL THEN 0 ", "ELSE MAX(\"p\") - (MAX(\"p\") - MIN(\"p\"))*(MAX(\"T1\".\"_twm_T\") - MAX(\"_twm_Tlo\"))/ ", 
        "(CASE \tWHEN \"D_F\" = 1 THEN 1 ", "WHEN \"D_F\" = 2 THEN 0.1 ", "ELSE 0.01 END) END AS \"TTestPValue\" ", ",CASE \tWHEN MAX(\"T1\".\"_twm_T\") > 0 AND \"TTestPValue\" <= `thresh` THEN 'p' ", 
        "WHEN \"TTestPValue\" > `thresh` THEN 'a' ", "WHEN MAX(\"T1\".\"_twm_T\") < 0 AND \"TTestPValue\" <= `thresh` THEN 'n' ", 
        "ELSE NULL END AS \"TTestCallP_`thresh`\" ", "FROM (SELECT COUNT(\"_twm_x1\") AS \"_twm_M\" ", ",COUNT(\"_twm_x2\") AS \"_twm_N\" ", 
        ",VAR_POP(\"_twm_x1\")/NULLIFZERO(\"_twm_M\" - 1) AS \"_twm_V1\" ", ",VAR_POP(\"_twm_x2\")/NULLIFZERO(\"_twm_N\" - 1) AS \"_twm_V2\" ", 
        ",(\"_twm_V1\" + \"_twm_V2\")**2/NULLIFZERO(\"_twm_V1\"*\"_twm_V1\"/NULLIFZERO(\"_twm_M\" - 1) + \"_twm_V2\"*\"_twm_V2\"/NULLIFZERO(\"_twm_N\" - 1)) AS \"_twm_Dof\" ", 
        ",ABS(AVG(\"_twm_x2\") - AVG(\"_twm_x1\"))/NULLIFZERO(SQRT(\"_twm_V1\" + \"_twm_V2\")) AS \"_twm_T\" ", ",CASE \tWHEN \"_twm_Dof\" <121  THEN \"_twm_Dof\" ", 
        "ELSE 1000 END AS \"_twm_MDof\" ", ",\"_twm_T\"(DECIMAL(10,0)) AS \"_twm_Thi0\" ", ",\"_twm_T\"(DECIMAL(10,1)) AS \"_twm_Thi1\" ", 
        ",\"_twm_T\"(DECIMAL(10,2)) AS \"_twm_Thi2\" ", ",CASE \tWHEN \"_twm_Dof\" =1  THEN \"_twm_Thi0\" ", "WHEN \"_twm_Dof\" =2  THEN \"_twm_Thi1\" ", 
        "ELSE \"_twm_Thi2\" END AS \"_twm_Thi\" ", ",CASE \tWHEN \"_twm_Dof\" =1  THEN \"_twm_Thi\"-1 ", "WHEN \"_twm_Dof\" =2  THEN \"_twm_Thi\"-0.1 ", 
        "ELSE \"_twm_Thi\"-0.01 END AS \"_twm_Tlo\" ", "FROM `subq` SQ1) AS \"T1\" ", "LEFT OUTER JOIN `tobj` \"_twm_TD\" ON \"_twm_TD\".\"df\" = (CAST(\"_twm_MDof\" AS INTEGER)) ", 
        "AND (\"_twm_Thi\" = COALESCE(\"_twm_TD\".\"t\", 0.01)(DECIMAL(10,2)) ", "OR \"_twm_Tlo\" = COALESCE(\"_twm_TD\".\"t\", 0.01)(DECIMAL(10,2))) ", 
        sep = "")
    query <- gsub("`subq`", subq, query)
    query <- gsub("`obj`", obj, query)
    query <- gsub("`tobj`", tobj, query)
    query <- gsub("`col1`", col1, query)
    query <- gsub("`col2`", col2, query)
    query <- gsub("`thresh`", thresh, query)
}

.td.genvalues <- function(col) {
    vals <- c("IS NULL", "= 0", "> 0", "< 0")
    cnms <- c("xnull", "xzero", "xpos", "xneg")
    vec <- c()
    idx <- 1
    for (i in 1:length(vals)) vec[i] <- gettextf("CAST(SUM( CASE WHEN %s %s THEN 1.0E0 ELSE 0.0E0 END) AS FLOAT) AS %s", 
        col, vals[i], cnms[i])
    vec[length(vec) + 1] <- gettextf("CAST(COUNT(DISTINCT %s) AS FLOAT) AS \"xunique\"", col)
    vec[length(vec) + 1] <- gettextf("CAST(SUM(CASE WHEN %s = ' ' THEN 1.0E0 ELSE 0.0E0 END) AS FLOAT) AS \"xblank\"", 
        col)
    
    return(vec)
}

.td.genwilcoxon <- function(tdf, col1, col2, thresh = 0.05) {
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    nrmobj <- .td.locateObject("TWMS_Normal")
    subq <- paste("(SELECT CAST((\"`col2`\" - \"`col1`\") AS FLOAT) AS \"_twm_Diff\" ", ",CAST(SUM(1) OVER (  ORDER BY ABS(\"_twm_Diff\") ROWS UNBOUNDED PRECEDING) AS FLOAT)  AS \"_twm_Rnk\" ", 
        "FROM `obj` WHERE \"`col2`\" <> \"`col1`\")")
    query <- paste("SELECT \"_twm_N\" AS \"N\",CASE \tWHEN \"_twm_N\" > 50 THEN -ABS(\"_twm_ZPos2\") ", "ELSE -ABS(\"_twm_ZPos\") END AS \"Z_\" ", 
        ",2*(COALESCE(\"_twm_ND\".\"p\", 0.0000003)) AS \"WilcoxonPValue\" ", ",CASE \tWHEN \"_twm_RnkPos\" > \"_twm_RnkNeg\" AND \"WilcoxonPValue\" <= `thresh` THEN 'p' ", 
        "WHEN \"WilcoxonPValue\" > `thresh` THEN 'a' ", "WHEN \"_twm_RnkPos\" < \"_twm_RnkNeg\" AND \"WilcoxonPValue\" <= `thresh` THEN 'n' ", 
        "ELSE NULL END AS \"WilcoxonCallP_`thresh`\" ", "FROM (SELECT COUNT(*) AS \"_twm_N\" ", ",(SUM(CASE WHEN \"_twm_Diff\" > 0 THEN \"_twm_Rnk\" ELSE 0 END) - (\"_twm_N\"*(\"_twm_N\"+ 1E0)/4E0))/(NULLIF(SQRT((\"_twm_N\"*(\"_twm_N\" + 1E0)*(2E0*\"_twm_N\" + 1E0))/24E0), 0)) AS \"_twm_ZPos\" ", 
        ",SUM(CASE WHEN \"_twm_Diff\" > 0 THEN \"_twm_Rnk\" ELSE 0 END) AS \"_twm_RnkPos\" ", ",SUM(CASE WHEN \"_twm_Diff\" < 0 THEN \"_twm_Rnk\" ELSE 0 END) AS \"_twm_RnkNeg\" ", 
        ",(1 + SUM(\"_twm_signrank\"))/SQRT(SUM(\"_twm_SRSq\")) AS \"_twm_ZPos2\" ", "FROM (SELECT \"T7\".\"_twm_Diff\",\"T3\".\"_twm_Srnk\" AS \"_twm_Rnk\" ", 
        ",CASE \tWHEN \"T7\".\"_twm_Diff\" < 0  THEN -1.000 ", "WHEN \"T7\".\"_twm_Diff\" > 0  THEN 1.000 ", "ELSE 0.000 END AS \"_twm_sgn\" ", 
        ",\"_twm_sgn\"*\"T3\".\"_twm_Srnk\" AS \"_twm_signrank\" ", ",\"_twm_signrank\"*\"_twm_signrank\" AS \"_twm_SRSq\" ", 
        "FROM (SELECT AVG(\"_twm_Rnk\") AS \"_twm_Srnk\" ", ",ABS(\"_twm_Diff\") AS \"_twm_Adiff\" ", "FROM `subq` SQ1 ", 
        "GROUP BY \"_twm_Adiff\") AS \"T3\",`subq` T7 ", "WHERE \"T3\".\"_twm_Adiff\" = ABS(\"T7\".\"_twm_Diff\") ", ") AS \"T2\") AS \"T1\" ", 
        "LEFT OUTER JOIN `nrmobj` \"_twm_ND\" ON ", "\"_twm_ND\".z*1000(DECIMAL(10,0)) = \"Z_\"*1000(DECIMAL(10,0)) ", 
        sep = "")
    query <- gsub("`subq`", subq, query)
    query <- gsub("`obj`", obj, query)
    query <- gsub("`nrmobj`", nrmobj, query)
    query <- gsub("`col1`", col1, query)
    query <- gsub("`col2`", col2, query)
    query <- gsub("`thresh`", thresh, query)
}

.td.genzscore <- function(col, as = "XZSCORE") {
    x <- gettextf("(\"%s\" - xmean) / xstd AS \"%s\"", col, as)
    return(x)
}

.td.getPrimaryIndicies <- function(tdf) {
    lst <- c()
    obj <- .td.object(attr(tdf, "tableName"), attr(tdf, "database"))
    query <- gettextf("HELP INDEX %s", obj)
    df <- tdQuery(query)
    for (i in 1:nrow(df)) {
        if (as.character(df[i, 2]) == "P") 
            lst[as.integer(df[[i, 4]])] <- as.character(df[i, 3])
    }
    return(lst)
}

.td.locateObject <- function(table) {
    if (exists("tdMetadata", envir = .GlobalEnv)) 
        return(.td.object(table, get("tdMetadata", envir = .GlobalEnv)))
    
    query <- gettextf("SELECT CAST(databasename AS VARCHAR(30)) FROM dbc.tablesX WHERE tablename = '%s'", table)
    df <- try(tdQuery(query))
    if (nrow(df) == 0L) {
        stop(gettextf("Unable to locate object %s in any database", table))
    } else {
        tdMetadataDB(df[[1]])
        return(.td.object(table, df[[1]]))
    }
}

.td.object <- function(tbl, db) {
    if (is.null(db) || !nchar(db)) 
        return(gettextf("\"%s\"", tbl))
    return(gettextf("\"%s\".\"%s\"", db, tbl))
}

.td.objectExists <- function(obj) {
    obj_split = unlist(strsplit(obj, "\\."))
    query <- gettextf("SELECT 1 FROM dbc.tablesX WHERE databasename = '%s' AND tablename = '%s'", gsub("(^\"|\"$)", "", 
        obj_split[1]), gsub("(^\"|\"$)", "", obj_split[2]))
    df <- try(tdQuery(query))
    if (nrow(df) > 0) 
        return(TRUE) else return(FALSE)
}

.td.objectIsNumeric <- function(db = "", tbl, col) {
    
    obj <- .td.object(tbl, db)
    df <- tdQuery(gettextf(paste("SELECT CASE WHEN TYPE(%s.%s) IN ", "('BYTEINT','FLOAT','INTEGER','SMALLINT','BIGINT') OR ", 
        "TYPE(%s.%s) LIKE 'DECIMAL%%' THEN 1 ELSE 0 END", sep = ""), obj, col, obj, col))
    if (as.numeric(df) == 1) 
        return(TRUE)
    return(FALSE)
}

.td.objectIsNumeric2 <- function(db = "", tbl, col) {
    
    obj <- .td.object(tbl, db)
    df <- tdQuery(gettextf(paste("SELECT CASE WHEN TYPE(%s) IN ", "('BYTEINT','FLOAT','INTEGER','SMALLINT','BIGINT') OR ", 
        "TYPE(%s) LIKE 'DECIMAL%%' THEN 1 ELSE 0 END as v FROM ", obj, " SAMPLE 1", sep = ""), col, col))
    if (as.numeric(df) == 1) 
        return(TRUE)
    return(FALSE)
}

.td.gencolumnexpr <- function(x) {
    if (!inherits(x, "td.data.frame")) 
        stop("Error generating column expression.  Data type must be td data frame")
    
    # if(length(x) > 1) stop('Error generating column expression.  Multiple input values not allowed')
    
    val <- character(0)
    for (i in 1:length(names(x))) {
        if (as.character(i) %in% names(attr(x, "expressions"))) 
            val[i] <- attr(x, "expressions")[[as.character(i)]] else val[i] <- paste("\"", names(x)[[i]], "\"", sep = "")
    }
    
    return(paste(val, collapse = ","))
}

.td.dataframe2array <- function(df, INDEX) {
    vals <- list()
    dms <- integer(0)
    for (i in 1:length(INDEX)) {
        vals[[i]] <- as.vector(sort(unique(df[[i]])))
        dms[i] <- length(vals[[i]])
    }
    names(vals) <- names(INDEX)
    
    if (length(df) - length(INDEX) == 1) 
        arr <- array(NA, dim = dms, dimnames = vals) else arr <- array(vector("list", prod(dms)), dim = dms, dimnames = vals)
    
    arridx <- array(1:prod(dms), dim = dms, dimnames = vals)
    
    rf <- sapply(df, is.factor)
    df[rf] <- lapply(df[rf], as.character)
    
    for (i in 1:nrow(df)) {
        mat <- matrix(as.character(df[i, 1:length(INDEX)]), nrow = 1)
        if ((length(df) - length(INDEX)) == 1) 
            arr[[arridx[mat]]] <- df[i, (length(INDEX) + 1):length(df)] else arr[[arridx[mat]]] <- df[i, (length(INDEX) + 1):length(df)]
        row.names(arr[[arridx[mat]]]) <- NULL
    }
    
    return(arr)
}

.td.tdf2sql <- function(x, samp = missing) {
    obj <- .td.object(attr(x, "tableName"), attr(x, "database"))
    wc <- ""
    if (!is.null(attr(x, "whereClause"))) 
        wc <- paste("WHERE", attr(x, "whereClause"))
    if (missing(samp)) 
        selfmt <- "SELECT %s FROM %s %s" else selfmt <- "SELECT SAMPLEID AS SAMPLE_ID, %s FROM %s %s SAMPLE %s"
    
    val <- character(0)
    for (i in 1:length(names(x))) {
        if (as.character(i) %in% names(attr(x, "expressions"))) 
            val[i] <- paste(attr(x, "expressions")[[as.character(i)]], "AS", names(x)[i]) else val[i] <- paste("\"", names(x)[[i]], "\"", sep = "")
    }
    
    
    val <- paste(val, collapse = ",")
    if (missing(samp)) 
        query <- gettextf(selfmt, val, obj, wc) else query <- gettextf(selfmt, val, obj, wc, paste(samp, collapse = ","))
    
    return(query)
} 
