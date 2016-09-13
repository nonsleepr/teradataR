# Below is the table used for the test, called "tab1"
# c1  c2  c3
# -------  --
#  3.  9   ?
#  9.  9   ?
#  6.  9   ?
#  1.  9   ?

tdf <- td.data.frame("tab1")
tdf["c3"] <- TO_CHAR(tdf["c1"], tdf["c2"])
as.td.data.frame(tdf, tableName="tab2")

# this is the resulting table, called "tab2"
# c1   c2   c3
# ---  ----  --
# 3.   9    ?
# 9.   9    ?
# 6.   9    ?
# 1.   9    ?