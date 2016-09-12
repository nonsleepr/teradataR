# Below is the table used for the test, called "test"
#  c1  c2       c3  c4  c5          c6
#-----------  --------  --  --  ----------  ----------
#  5  Emily         ?   cat         dog
#  4  Daisy     ?   ?   fork        spoon
#  3  Hank      ?   ?   ball        bat
#  2  Amy       ?   ?   robot       human
#
# the R code is below:
tdf <- td.data.frame("test")
tdf["c7"] <- RPAD(tdf["c5"], 15, " ")
as.td.data.frame(tdf, tableName="testY")

# Below are the results of the table, when you type "select * from testY"
# c1 c2       c3 c4 c5         c6         c7
#----------- -------- -- -- ---------- ------------------------------------
#  5 Emily       ?  cat        dog        cat
#  4 Daisy    ?  ?  fork       spoon      fork
#  3 Hank     ?  ?  ball       bat        ball
#  2 Amy      ?  ?  robot      human      robot