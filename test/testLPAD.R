# Below is the table used for the test, called "padTab"
# c1               c2               c3
# ---------------  ---------------  ---------------
# Emily                 Emily       Emily
# Daisy                 Daisy       Daisy
# Hank                   Hank       Hank
# Amy                     Amy       Amy
#
# the R code is below:
tdf <- td.data.frame("padTab")
tdf["c4"] <- LPAD(tdf["c3"], 15, "x")
as.td.data.frame(tdf, tableName="padTab2")

# Below are the results of the table, when you type "select * from padTab2"
# c1              c2              c3              c4
# --------------- --------------- --------------- ---------------------------
# Emily                 Emily     Emily           xxxxxxxxxxEmily
# Daisy                Daisy      Daisy           xxxxxxxxxxDaisy
# Hank                  Hank      Hank            xxxxxxxxxxxHank
# Amy                    Amy      Amy             xxxxxxxxxxxxAmy

# To verify each entry of c4:
res1 = tdQuery("select c4 from padTab2 where c3='Emily'") == "xxxxxxxxxxEmily"
stopifnot(res1)

res2 = tdQuery("select c4 from padTab2 where c3='Daisy'") == "xxxxxxxxxxDaisy"
stopifnot(res2)

res3 = tdQuery("select c4 from padTab2 where c3='Hank'") == "xxxxxxxxxxxHank"
stopifnot(res3)

res4 = tdQuery("select c4 from padTab2 where c3='Amy'") == "xxxxxxxxxxxxAmy"
stopifnot(res4)