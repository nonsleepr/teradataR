# Below is the table used for the test, called "tab1"
#
# c1  
# ----
# 5 
# 4 
# 3 
# 2 

tdf <- td.data.frame("tab")
tdf["c2"] <- SIGN(tdf["c1"])
as.td.data.frame(tdf, tableName="tab2")

#this is the resulting table, called "tab2"
# c1    c2
# ---- -------
# 5    1
# 4    1
# 3    1
# 2    1