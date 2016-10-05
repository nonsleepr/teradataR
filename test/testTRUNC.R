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
tdf["c3"] <- TRUNC(tdf["c2"], 1)
as.td.data.frame(tdf, tableName="numManip2")
# Below are the results of the table, when you type "select * from numManip2"
# c1                                           c2                        c3
# -----  ---------------------------------------- -------------------------
# 2                                         8.289                        8.2
# 5                                         13.99                       13.9
# 4                                         17.06                         17
# 3                                          48.1                       48.1
# 1                                         555.3                      555.3  
#
# To verify each entry of c3:
res1 = tdQuery("select c3 from numManip2 where c1='2'") == 8.2
stopifnot(res1)

res2 = tdQuery("select c3 from numManip2 where c1='5'") == 13.9
stopifnot(res2)

res3 = tdQuery("select c3 from numManip2 where c1='4'") == 17
stopifnot(res3)

res4 = tdQuery("select c3 from numManip2 where c1='3'") == 48.1
stopifnot(res4)

res5 = tdQuery("select c3 from numManip2 where c1='1'") == 555.3
stopifnot(res5)