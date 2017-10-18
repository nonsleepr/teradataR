#test On()
#res1 = on(target="select *", from="tab1", partition = "any", hash = "col1") == "on (select * from tab1)\npartition by any\nhash by col1"
#print(res1)
#stopifnot(res1)

#res2 = on(target="select *", from="tab1", hash="col1", local_order="col2") == "on (select * from tab1)\nhash by col1\nlocal order by col2"
#stopifnot(res2)

#res3 = on(target="tab1", partition="col1") == "on tab1\npartition by col1"
#print(res3)
#stopifnot(res3)

#res4 = on(target="tab1", dimension= " ") == "on tab1\ndimension"
#stopifnot(res4)

#res5 = on(target="select 1", partition="1") == "on (select 1)\npartition by 1"
#print(res5)
#stopifnot(res5)

#res6 = on(target="select *", from="tab1", hash="col1", local_order="col1, col2") == "on (select * from tab1)\nhash by col1\nlocal order by col1, col2"
#print(res6)
#stopifnot(res6)

#res7 = on(target="select *", from="tab1", as="test") == "on (select * from tab1)\nas test"
#print(res7)
#stopifnot(res7)

#on1 = on(target="select p, var1, var2, var3, var4, var5", from="TestCM_Mult", local_order="p")
#littleOnClause= toQuery(selectPhrase="select * from TD_SYSFNLIB.calcmatrix", ons=on1, phase="local", as="D1")
#ons2 = on(target="select *", from="TD_SYSFNLIB.calcmatrix", subQuery=on1, hash="p", local_order="p")
#print(ons2)