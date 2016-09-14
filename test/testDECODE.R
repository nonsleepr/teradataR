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
tdf["c4"] <- DECODE(tdf["c1"], default='none', 1, 'Alpha', 2, 'Bravo', 3, 'Charlie', 4, 'Delta', 5, 'Echo')
as.td.data.frame(tdf, tableName="numTab2")

# This is the resulting table, called "numTab2"
# c1           c2           c3  c4
# -----------  -----------  --  -------
# 5            2            9   Echo
# 6            3            9   none
# 3            2            9   Charlie
# 8            3            9   none
# 2            4            9   Bravo

# To verify each entry of c4:
res1 = tdQuery("select c4 from numTab2 where c1=5") == "Echo"
stopifnot(res1)

res2 = tdQuery("select c4 from numTab2 where c1=6") == "none"
stopifnot(res2)

res3 = tdQuery("select c4 from numTab2 where c1=3") == "Charlie"
stopifnot(res3)

res4 = tdQuery("select c4 from numTab2 where c1=8") == "none"
stopifnot(res4)

res5 = tdQuery("select c4 from numTab2 where c1=2") == "Bravo"
stopifnot(res5)