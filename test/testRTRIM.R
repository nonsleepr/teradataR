# Below is the table used for the test, called "tab1"
#c1          c2          c3
#----------  ----------  ----------
#    horse   horse       ?
#  mouse     mouse       ?
#     dog    dog         ?
# cat        cat         ?

tdf <- td.data.frame("tab1")
tdf["c3"] <- LTRIM(tdf["c2"])
as.td.data.frame(tdf, tableName="tab2")
#c1          c2          c3
#----------  ----------  -----
#    horse   horse       horse
#  mouse     mouse       mouse
#     dog    dog         dog
# cat        cat         cat