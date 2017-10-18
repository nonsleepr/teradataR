# Below is the table used for the test, called "charTab"
# c1          c2        c3        c4        c5
# ----------  ---- ------- --------- ---------
# explore     lo        xx         e         z
# interrupt   ter      xyz         u         z
# disappear   ar        yy         s         z
# factor      ac        xy         c         z
# appreciate  pp        xx         r         z

tdf <- td.data.frame("charTab")
tdf["c6"] <- OREPLACE(tdf["c1"], tdf["c2"], tdf["c3"])
as.td.data.frame(tdf, tableName="charTab2")

#this is the resulting table, charTab2
# c1          c2        c3        c4        c5         c6
# ----------  ---- ------- --------- --------- ----------
# explore     lo        xx         e         z    expxxre
# interrupt   ter      xyz         u         z  inxyzrupt
# disappear   ar        yy         s         z  disappeyy
# factor      ac        xy         c         z     fxytor
# appreciate  pp        xx         r         z axxreciate
#
# To verify each entry of c6:
res1 = tdQuery("select c6 from charTab2 where c1='explore'") == "expxxre"
stopifnot(res1)

res2 = tdQuery("select c6 from charTab2 where c1='interrupt'") == "inxyzrupt"
stopifnot(res2)

res3 = tdQuery("select c6 from charTab2 where c1='disappear'") == "disappeyy"
stopifnot(res3)

res4 = tdQuery("select c6 from charTab2 where c1='factor'") == "fxytor"
stopifnot(res4)

res5 = tdQuery("select c6 from charTab2 where c1='appreciate'") == "axxreciate"
stopifnot(res5)