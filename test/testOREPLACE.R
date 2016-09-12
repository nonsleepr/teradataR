# Below is the table used for the test, called "tab2"
# c1          c2    c3
# ----------  ----  ----------
# explore     lo    ?
# interrupt   ter   xyz
# disappear   ar    xy
# factor      ac    xy
# appreciate  pp    xy

tdf <- td.data.frame("tab2")
tdf["c4"] <- OREPLACE(tdf["c1"], tdf["c2"], tdf["c3"])
as.td.data.frame(tdf, tableName="tab3")

#this is the resulting table, tab3
# c1         c2   c3         c4
# ---------- ---- ---------- ------------------------------------------------
# explore    lo   ?          explore
# interrupt  ter  xyz        interrupt
# disappear  ar   xy         disappear
# factor     ac   xy         factor
# appreciate pp   xy         appreciate

#although this test did produce the same table/dataframe in both bteq and 
#RStudio, further testing on how to get the expected output is suggested