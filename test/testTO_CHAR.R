# Below is the table used for the test, called "numTab"
# c1                    c2  c3
# -----------  -----------  --
# 5                     2   9
# 6                     3   9
# 3                     2   9
# 8                     3   9
# 2                     4   9

tdf <- td.data.frame("numTab")
tdf["c4"] <- TO_CHAR(tdf["c1"], tdf["c3"])
as.td.data.frame(tdf, tableName="numTab2")

# this is the resulting table, called "numTab2"  Although the values look 
# unchanged, the type of 'c4' is a varchar, while the type of 'c1' is an 
# integer. This can be verified by typing "show table numTab2" into bteq or 
# "tdQuery("show table numTab2")" into RStudio.
# c1                   c2 c3 c4
# ----------- ----------- -- ------------------------------------------------
# 5                     2 9   5
# 6                     3 9   6
# 3                     2 9   3
# 8                     3 9   8
# 2                     4 9   2

# To verify each entry of c4:
res1 = tdQuery("select c4 from numTab2 where c1=5") == '5'
stopifnot(res1)

res2 = tdQuery("select c4 from numTab2 where c1=6") == '6'
stopifnot(res2)

res3 = tdQuery("select c4 from numTab2 where c1=3") == '3'
stopifnot(res3)

res4 = tdQuery("select c4 from numTab2 where c1=8") == '8'
stopifnot(res4)

res5 = tdQuery("select c4 from numTab2 where c1=2") == '2'
stopifnot(res5)