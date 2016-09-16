# Although this file is an R file, for the sake of consistency with the other
# files, the code is in SQL.  It is all commented out so that the file does not
# produce compile errors.  Please copy and past the desired code into bteq in 
# order to make the needed tables

# Table Name: numTab
# This table is used for testing numeric functions.  It works with the tests for
# AVG(), DECODE(), POWER(), and TO_CHAR().
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
# works with the tests for CHR(), INITCAP(), and NGRAM().
#
# drop table test;
# drop table test2;
# create table test (
# c1 integer,
# c2 varchar(100),
# c3 varchar(100));
#
# insert into test (c1, c2, c3) values (1, 'mouse', 'house');
# insert into test (c1, c2, c3) values (2, 'fork', 'spoon');
# insert into test (c1, c2, c3) values (3, 'ball', 'bat');
# insert into test (c1, c2, c3) values (4, 'robot', 'human');
# insert into test (c1, c2, c3) values (5, 'cat', 'dog');
# insert into test (c1, c2, c3) values (6, 'horse', 'force');
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table Name: charTab
# This table is used for testing functions that search characters under 
# certain conditions.  It works with tests for INSTR(), OREPLACE(), OTRANSLATE().
# 
# drop table charTab;
# drop table charTab2;
# create table charTab (
# c1 varchar(50),
# c2 varchar(50),
# c3 varchar(50),
# c4 varchar(50),
# c5 varchar(50));
#
# insert into charTab (c1, c2, c3, c4, c5) values ('explore', 'lo', 'xx', 'e', 'z');
# insert into charTab (c1, c2, c3, c4, c5) values ('interrupt', 'ter', 'xyz', 'u', 'z');
# insert into charTab (c1, c2, c3, c4, c5) values ('disappear', 'ar', 'yy', 's', 'z');
# insert into charTab (c1, c2, c3, c4, c5) values ('factor', 'ac', 'xy', 'c', 'z');
# insert into charTab (c1, c2, c3, c4, c5) values ('appreciate', 'pp', 'xx', 'r', 'z');
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table Name: padTab
# This table is used for testing functions that manipulate white space.  It
# works with tests for LPAD(), LTRIM(), RPAD(), and RTRIM().
# 
# drop table padTab;
# drop table padTab2;
# create table padTab (
# c1 varchar(10),
# c2 varchar(10),
# c3 varchar(10));
#
# insert into padTab (c1, c2, c3) values ('Emily      ','     Emily', 'Emily');
# insert into padTab (c1, c2, c3) values ('Daisy      ', '     Daisy', 'Daisy');
# insert into padTab (c1, c2, c3) values ('Hank       ', '      Hank', 'Hank');
# insert into padTab (c1, c2, c3) values ('Amy        ', '       Amy', 'Amy');
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table Name: numManip
# This table is used for testing functions that manupulate numbers.  It works 
# with tests for TO_NUMBER() and TRUNC().
#
#
# drop table numManip;
# drop table numManip2;
# create table numManip (
# c1 character(5),
# c2 number);
#
# insert into numManip (c1, c2) values ('1', 555.3);
# insert into numManip (c1, c2) values ('2', 8.289);
# insert into numManip (c1, c2) values ('3', 48.1);
# insert into numManip (c1, c2) values ('4', 17.06);
# insert into numManip (c1, c2) values ('5', 13.99);