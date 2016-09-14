# Below is the table used for the test, called "test"
#        c1         c2          c3        
# ---------- ----------- ----------
#         3       ball          bat
#         4       robot       human
#         1         cat         dog
#         2        fork       spoon
#
# the R code is below:
tdf <- td.data.frame("test")
tdf["c4"] <- INITCAP(tdf["c2"])
as.td.data.frame(tdf, tableName="test2")

# Below are the results of the table, when you type "select * from test2"
#        c1         c2          c3        c4       
# ---------- ----------- ---------- ---------
#         3       ball          bat       Ball
#         4       robot       human      Robot
#         1         cat         dog        Cat
#         2        fork       spoon       Fork
#
# To verify each entry of test2
res1 = tdQuery("select c4 from test2 where c1=3") == "Ball"
stopifnot(res1)

res2 = tdQuery("select c4 from test2 where c1=4") == "Robot"
stopifnot(res2)

res3 = tdQuery("select c4 from test2 where c1=1") == "Cat"
stopifnot(res3)

res4 = tdQuery("select c4 from test2 where c1=2") == "Fork"
stopifnot(res4)