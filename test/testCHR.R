# Below is the table used for the test, called "test"
# c1  c2        c5          c6          c7
# -----------  --------  ----------  ----------  --
#  5  Emily     cat         dog
#  4  Daisy     fork        spoon
#  3  Hank      ball        bat
#  2  Amy       robot       human

tdf <- td.data.frame("test")
tdf["c7"] <- CHR(tdf["c1"])
as.td.data.frame(tdf, "test2")

# the resulting table is below, called "test2"
# c1  c2        c5          c6          c7
# -----------  --------  ----------  ----------  --
#  5  Emily     cat         dog
#  4  Daisy     fork        spoon
#  3  Hank      ball        bat
#  2  Amy       robot       human

#Although the resulting table does not seem to give the expected output, the same output occurs in bteq and in RStudio.
#Further testing is recommended