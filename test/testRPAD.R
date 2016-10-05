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
tdf["c4"] <- RPAD(tdf["c3"], 15, "x")
as.td.data.frame(tdf, tableName="padTab2")

# Below are the results of the table, when you type "select * from padTab2" 
# into bteq
# c1         c2         c3         c4
# ---------- ---------- ---------- ------------------------------------------
# Emily           Emily Emily      Emilyxxxxxxxxxx
# Daisy           Daisy Daisy      Daisyxxxxxxxxxx
# Hank             Hank Hank       Hankxxxxxxxxxxx
# Amy               Amy Amy        Amyxxxxxxxxxxxx
#
res1 = tdQuery("select c4 from padTab2 where c3='Emily'") == "Emilyxxxxxxxxxx"
stopifnot(res1)

res2 = tdQuery("select c4 from padTab2 where c3='Daisy'") == "Daisyxxxxxxxxxx"
stopifnot(res2)

res3 = tdQuery("select c4 from padTab2 where c3='Hank'") == "Hankxxxxxxxxxxx"
stopifnot(res3)

res4 = tdQuery("select c4 from padTab2 where c3='Amy'") == "Amyxxxxxxxxxxxx"
stopifnot(res4)