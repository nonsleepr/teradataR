# Below is the table used for the test, called "tab2"
# c1          c2          c3
# ----------  ----------  ----------
# mouse       house       ?
# wagon       wagon       ?
# cart        heart       ?
# horse       force       ?

tdf <- td.data.frame("tab2")
tdf["c3"] <- NGRAM(tdf["c1"], tdf["c2"], 2)
as.td.data.frame(tdf, tableName="tab3")

#this is the resulting table
# c1          c2                   c3
# ----------  ----------  -----------
# mouse       house                 8
# wagon       wagon                 9
# cart        heart                 8
# horse       force                 6