# Below is the table used for the test, called "charTab"
# c1          c2        c3
# ----------  ---- -------
# explore     lo        xx
# interrupt   ter      xyz
# disappear   ar        yy
# factor      ac        xy
# appreciate  pp        xx

tdf <- td.data.frame("charTab")
tdf["c3"] <- INSTR(tdf["c1"], tdf["c2"])
as.td.data.frame(tdf, tableName="charTab2")

# This is the resulting table, called "charTab2"
# c1          c2         c3
# ----------  ----  -------
# explore     lo         xx                                  
# interrupt   ter       xyz                                   
# disappear   ar         yy                                   
# factor      ac         xy                                   
# appreciate  pp         xx                                  

# While running the equivalent code in bteq produces the same table, this test does not seem to give the expected output
# I recommend further testing.