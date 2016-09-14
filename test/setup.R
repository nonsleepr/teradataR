# Although this file is an R file, for the sake of consistency with the other
# files, the code is in SQL.  It is all commented out so that the file does not
# produce compile errors.  Please copy and past the desired code into bteq in 
# order to make the needed tables

# Table Name: numTab
# This table is used for testing numeric functions.  It works with the tests for
# DECODE(), POWER(), and TO_CHAR().
# 
# drop table numTab;
# drop table numTab2;
# create table numTab (
# c1 integer,
# c2 integer,
# c3 character(1));
#
# insert into numTab (c1, c2, c3) values (5, 2, '9');
# insert into numTab (c1, c2, c3) values (8, 3, '9');
# insert into numTab (c1, c2, c3) values (2, 4, '9');
# insert into numTab (c1, c2, c3) values (6, 3, '9');
# insert into numTab (c1, c2, c3) values (3, 2, '9');
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table Name: negNums
# This table is used for testing the SIGN() function
#
# drop table negNums;
# drop table negNums2;
# create table negNums (
# c1 integer);
#
# insert into negNums values(-11);
# insert into negNums values(3);
# insert into negNums values(-2);
# insert into negNums values(4);
# insert into negNums values(555);
# insert into negNums values(0);
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table Name: test
# This table is used for testing functions for character manipulation.  It 
# works with the tests for CHR() and INITCAP().
#
# drop table test;
# drop table test2;
# create table test (
# c1 integer,
# c2 varchar(256),
# c3 varchar(256));
#
# insert into test (c1, c2, c3) values (1, 'cat', 'dog');
# insert into test (c1, c2, c3) values (2, 'fork', 'spoon');
# insert into test (c1, c2, c3) values (3, 'ball', 'bat');
# insert into test (c1, c2, c3) values (4, 'robot', 'human');
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table Name: charTab
# This table is used for testing functions that search characters under 
# certain conditions.  It works with tests for INSTR(),
# 
# drop table charTab;
# drop table charTab2;
# create table charTab (
# c1 varchar(50),
# c2 varchar(50));
#
# insert into charTab (c1, c2) values ('explore', 'lo');
# insert into charTab (c1, c2) values ('interrupt', 'ter');
# insert into charTab (c1, c2) values ('disappear', 'ar');
# insert into charTab (c1, c2) values ('factor', 'ac');
# insert into charTab (c1, c2) values ('appreciate', 'pp');
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~