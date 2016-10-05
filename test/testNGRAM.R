# Below is the table used for the test, called "test"
#        c1         c2          c3        
# ---------- ----------- ----------
#         5          cat        dog
#         4        robot      human
#         6        horse      force
#         3         ball        bat
#         1        mouse      house
#         2         fork      spoon

tdf <- td.data.frame("test")
tdf["c4"] <- NGRAM(tdf["c2"], tdf["c3"], 2)
as.td.data.frame(tdf, tableName="test2")

#this is the resulting table, called "test2"
#        c1         c2          c3         c4     
# ---------- ----------- ---------- ---------
#         5          cat        dog         0
#         4        robot      human         0
#         6        horse      force         1
#         3         ball        bat         1
#         1        mouse      house         3
#         2         fork      spoon         0
#
res1 = tdQuery("select c4 from test2 where c1=5") == 0
stopifnot(res1)

res2 = tdQuery("select c4 from test2 where c1=4") == 0
stopifnot(res2)

res3 = tdQuery("select c4 from test2 where c1=6") == 1
stopifnot(res3)

res4 = tdQuery("select c4 from test2 where c1=3") == 1
stopifnot(res4)

res5 = tdQuery("select c4 from test2 where c1=1") == 3
stopifnot(res5)

res6 = tdQuery("select c4 from test2 where c1=2") == 0
stopifnot(res6)