# Below is the table used for the test, called "charTab"
# c1          c2        c3        c4        c5
# ----------  ---- ------- --------- ---------
# explore     lo        xx         e         z
# interrupt   ter      xyz         u         z
# disappear   ar        yy         s         z
# factor      ac        xy         c         z
# appreciate  pp        xx         r         z

tdf <- td.data.frame("charTab")
tdf["c6"] <- OTRANSLATE(tdf["c1"], tdf["c4"], tdf["c5"])
as.td.data.frame(tdf, tableName="charTab2")

#this is the resulting table, charTab2
#
# c1          c2        c3        c4        c5         c6
# ----------  ---- ------- --------- --------- ----------
# explore     lo        xx         e         z    zxplorz
# interrupt   ter      xyz         u         z  interrzpt
# disappear   ar        yy         s         z  dizappear
# factor      ac        xy         c         z     faztor
# appreciate  pp        xx         r         z appzeciate

# To verify each entry of c6:
res1 = tdQuery("select c6 from charTab2 where c1='explore'") == "zxplorz"
stopifnot(res1)

res2 = tdQuery("select c6 from charTab2 where c1='interrupt'") == "interrzpt"
stopifnot(res2)

res3 = tdQuery("select c6 from charTab2 where c1='disappear'") == "dizappear"
stopifnot(res3)

res4 = tdQuery("select c6 from charTab2 where c1='factor'") == "faztor"
stopifnot(res4)

res5 = tdQuery("select c6 from charTab2 where c1='appreciate'") == "appzeciate"
stopifnot(res5)