#test makePartition()
#res= .td.makePartition("col1, col2") == "partition by col1, col2"
#print(res)
#stopifnot(res)

#res = .td.makePartition(partition = "any") == "partition by any"
#stopifnot(res)

#test makeHash()
#res = .td.makeHash("col1") == "hash by col1"
#stopifnot(res)

#res = .td.makeHash("col1") == "nope"
#stopifnot(res)

#test makeOrder()
#res = .td.makeOrder("col1, col2") == "order by col1, col2"
#stopifnot(res)

#test makeDimension()
#res = .td.makeDimension() == "dimension"
#stopifnot(res)

#test makeLocalOrder()
#res = .td.makeLocalOrder("col1")
