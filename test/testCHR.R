# Below is the table used for the test, called "test"
#        c1         c2          c3        
# ---------- ----------- ----------
#         5          cat        dog
#         4        robot      human
#         6        horse      force
#         3         ball        bat
#         1        mouse      house
#         2         fork      spoon
tdf <- td.data.frame("test")
tdf["c4"] <- CHR(tdf["c1"])
as.td.data.frame(tdf, tableName="test2")

# the resulting table is below, called "test2"
#        c1         c2          c3         c4    
# ---------- ----------- ---------- ---------
#         5          cat        dog       
#         4        robot      human
#         6        horse      force
#         3         ball        bat
#         1        mouse      house
#         2         fork      spoon

#Although the resulting table does not seem to give the expected output, the same output occurs in bteq and in RStudio.
#Further testing is recommended