# Below is the table used for the test, called "tab1"
# c1          c2  c3  c4
# ----------  --  --  ----------
# outstanding  s   x   ?
# shrewd       r   x   ?
# amicable     i   x   ?
# introverted  t   x   ?

tdf <- td.data.frame("tab1")
tdf["c4"] <- OTRANSLATE(tdf["c1"], tdf["c2"], tdf["c3"])
as.td.data.frame(tdf, tableName="tab2")

#this is the resulting table, tab2

# c1         c2 c3 c4
# ---------- -- -- ----------------------------------------------------------
# outstanding s  x  outxtanding
# shrewd      r  x  shxewd
# amicable    i  x  amxcable
# introverted t  x  inxroverxed