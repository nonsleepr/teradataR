# Below is the table used for the test, called "tab1"
# c1          c2
# ----------  ----
# explore     lo
# interrupt   ter
# disappear   ar
# factor      ac
# appreciate  pp

tdf <- td.data.frame("tab1")
tdf["c3"] <- INSTR(tdf["c1"], tdf["c2"])
as.td.data.frame(tdf, tableName="tab2")

# This is the resulting table, called "tab2"
# c1          c2                                         c3
# ----------  ----  ----------------------------------------
# explore     lo                                           0
# interrupt   ter                                          0
# disappear   ar                                           0
# factor      ac                                           0
# appreciate  pp                                           0

# While running the equivalent code in bteq produces the same table, this test does not seem to give the expected output
# I recommend further testing.