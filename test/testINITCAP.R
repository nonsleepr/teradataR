# Below is the table used for the test, called "test"
#        c1         c2          c3        
# ---------- ----------- ----------
#         5          cat        dog
#         4        robot      human
#         6        horse      force
#         3         ball        bat
#         1        mouse      house
#         2         fork      spoon
# the R code is below:
tdf <- td.data.frame("test")
tdf["c4"] <- INITCAP(tdf["c2"])
as.td.data.frame(tdf, tableName="test2")

# Below are the results of the table, when you type "select * from test2"
#        c1         c2          c3         c4    
# ---------- ----------- ---------- ---------
#         5          cat        dog       Cat
#         4        robot      human     Robot
#         6        horse      force     Horse
#         3         ball        bat      Ball
#         1        mouse      house     Mouse
#         2         fork      spoon      Fork
#
# To verify each entry of test2
res1 = tdQuery("select c4 from test2 where c1=5") == "Cat"
stopifnot(res1)

res2 = tdQuery("select c4 from test2 where c1=4") == "Robot"
stopifnot(res2)

res3 = tdQuery("select c4 from test2 where c1=6") == "Horse"
stopifnot(res3)

res4 = tdQuery("select c4 from test2 where c1=3") == "Ball"
stopifnot(res4)

res5 = tdQuery("select c4 from test2 where c1=1") == "Mouse"
stopifnot(res5)

res6 = tdQuery("select c4 from test2 where c1=2") == "Fork"
stopifnot(res6)