# Below is the table used for the test, called "numTab"
#
# c1                    c2  c3
# -----------  -----------  --
# 5                     2   9
# 6                     3   9
# 3                     2   9
# 8                     3   9
# 2                     4   9

tdf <- td.data.frame("numTab")
tdf['c4'] <- POWER(tdf['c1'], tdf['c2'])
as.td.data.frame(tdf, tableName="numTab2")

# this is the resulting table, called "numTab2"
# c1           c2           c3                                        c4
# -----------  -----------  --  ----------------------------------------
# 5            2            9                                         25
# 6            3            9                                        216
# 3            2            9                                          9
# 8            3            9                                        512
# 2            4            9                                         16

# To verify each entry of c4:
res1 = tdQuery("select c4 from numTab2 where c1=5") == 25
stopifnot(res1)

res2 = tdQuery("select c4 from numTab2 where c1=6") == 216
stopifnot(res2)

res3 = tdQuery("select c4 from numTab2 where c1=3") == 9
stopifnot(res3)

res4 = tdQuery("select c4 from numTab2 where c1=8") == 512
stopifnot(res4)

res5 = tdQuery("select c4 from numTab2 where c1=2") == 16
stopifnot(res5)