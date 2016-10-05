# Below is the table used for the test, called "numManip"
# c1                                           c2
# -----  ----------------------------------------
# 2                                         8.289
# 5                                         13.99
# 4                                         17.06
# 3                                          48.1
# 1                                         555.3
#
# the R code is below:
tdf <- td.data.frame("numManip")
tdf["c3"] <- TO_NUMBER(tdf['c1'])
as.td.data.frame(tdf, tableName="numManip2")
#
# Below are the results of the table, when you type "select * from numManip2"
# c1                                          c2                           c3
# ----- ---------------------------------------- ----------------------------
# 2                                        8.289                            2
# 5                                        13.99                            5
# 4                                        17.06                            4
# 3                                         48.1                            3
# 1                                        555.3                            1
#
# To verify the data type of c3, type 'show table numManip2' into bteq:
# CREATE SET TABLE ECOLE.numManip2 ,NO FALLBACK ,
# NO BEFORE JOURNAL,
# NO AFTER JOURNAL,
# CHECKSUM = DEFAULT,
# DEFAULT MERGEBLOCKRATIO
# (
# c1 CHAR(5) CHARACTER SET LATIN NOT CASESPECIFIC,
# c2 NUMBER,
# c3 NUMBER)
# PRIMARY INDEX ( c1 );
#
# To verify each entry of c3:
res1 = tdQuery("select c3 from numManip2 where c1='2'") == 2
stopifnot(res1)

res2 = tdQuery("select c3 from numManip2 where c1='5'") == 5
stopifnot(res2)

res3 = tdQuery("select c3 from numManip2 where c1='4'") == 4
stopifnot(res3)

res4 = tdQuery("select c3 from numManip2 where c1='3'") == 3
stopifnot(res4)

res5 = tdQuery("select c3 from numManip2 where c3='1'") == 1
stopifnot(res5)