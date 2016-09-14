# Below is the table used for the test, called "charTab"
# c1          c2        c3        c4        c5
# ----------  ---- ------- --------- ---------
# explore     lo        xx         e         z
# interrupt   ter      xyz         u         z
# disappear   ar        yy         s         z
# factor      ac        xy         c         z
# appreciate  pp        xx         r         z

tdf <- td.data.frame("charTab")
tdf["c6"] <- INSTR(tdf["c1"], tdf["c2"])
as.td.data.frame(tdf, tableName="charTab2")

# This is the resulting table, called "charTab2"
# c1          c2        c3        c4        c5        c6
# ----------  ---- ------- --------- --------- ---------
# explore     lo        xx         e         z         4
# interrupt   ter      xyz         u         z         3
# disappear   ar        yy         s         z         8
# factor      ac        xy         c         z         2
# appreciate  pp        xx         r         z         2                

# While running the equivalent code in bteq produces the same table, this test does not seem to give the expected output
# I recommend further testing.