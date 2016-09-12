# Below is the table used for the test, called "tab1"
#
# c1  
# ----
# 5 
# 4 
# 3 
# 2 

tdf <- td.data.frame("tab1")
tdf["c10"] <- DECODE(tdf["c1"], default='none', 1, 'Alpha', 2, 'Bravo', 3, 'Charlie', 4, 'Delta')
as.td.data.frame(tdf, tableName="tab2")

#this is the resulting table, called "tab2"
# c1    c10
# ---- -------
# 5    none
# 4    Delta
# 3    Charlie
# 2    Bravo
