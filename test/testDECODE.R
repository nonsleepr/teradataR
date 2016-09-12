tdf <- td.data.frame("test")
tdf["c10"] <- DECODE(tdf["c1"], default=NULL, 1, 'Alpha', 2, 'Bravo', 3, 'Charlie', 4, 'Delta')
as.td.data.frame(tdf, tableName="test47")