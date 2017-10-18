# Below is the table used for the test, called "padTab"
# c1               c2               c3
# ---------------  ---------------  ---------------
# Emily                 Emily       Emily
# Daisy                 Daisy       Daisy
# Hank                   Hank       Hank
# Amy                     Amy       Amy

tdf <- td.data.frame("padTab")
tdf["c4"] <- RTRIM(tdf["c1"])
as.td.data.frame(tdf, tableName="padTab2")
# c1          c2          c3          c4
# ----------  ----------  ----------  ----------
# Emily            Emily  Emily       Emily
# Daisy            Daisy  Daisy       Daisy
# Hank              Hank  Hank        Hank
# Amy                Amy  Amy         Amy
res1 = tdQuery("select c4 from padTab2 where c3='Emily'") == "Emily"
stopifnot(res1)

res2 = tdQuery("select c4 from padTab2 where c3='Daisy'") == "Daisy"
stopifnot(res2)

res3 = tdQuery("select c4 from padTab2 where c3='Hank'") == "Hank"
stopifnot(res3)

res4 = tdQuery("select c4 from padTab2 where c3='Amy'") == "Amy"
stopifnot(res4)