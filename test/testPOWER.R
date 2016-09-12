# Below is the table used for the test, called "tab1"
# c1           c2           
# -----------  -----------
# 5            6
# 4            8
# 3            9
# 1            2
# 2            7

tdf <- td.data.frame("tab")
tdf['c3'] <- POWER(tdf['c1'], tdf['c2'])
as.td.data.frame(tdf, tableName="tab2")

#this is the resulting table, called "tab2"
# c1          c2          c3                                      c
# ----------- ----------- ----------
# 5           6           156
# 4           8           655
# 3           9           196
# 1           2
# 2           7             1
