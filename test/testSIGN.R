# Below is the table used for the test, called "negNums"
#
#          c1
# -----------
#           3
#         -11
#           0
#           4
#          -2
#         555 

tdf <- td.data.frame("negNums")
tdf["c2"] <- SIGN(tdf["c1"])
as.td.data.frame(tdf, tableName="negNums2")

#this is the resulting table, called "negNums2"
#          c1                                        c2
# -----------  ----------------------------------------
#           3                                         1
#         -11                                        -1
#           0                                         0
#           4                                         1
#          -2                                        -1
#         555                                         1

# To verify each entry of c2:
res1 = tdQuery("select c2 from negNums2 where c1=3") ==1
stopifnot(res1)

res2 = tdQuery("select c2 from negNums2 where c1=-11") == -1
stopifnot(res2)

res3 = tdQuery("select c2 from negNums2 where c1=0") == 0
stopifnot(res3)

res4 = tdQuery("select c2 from negNums2 where c1=4") == 1
stopifnot(res4)

res5 = tdQuery("select c2 from negNums2 where c1=-2") == -1
stopifnot(res5)

res6 = tdQuery("select c2 from negNums2 where c1=555") == 1
stopifnot(res6)