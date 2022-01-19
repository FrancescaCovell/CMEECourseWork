#### SwS 01 ####

# Welcome - this is your workspace for hand out SwS 01 #

# Please first work through the hand out using this script #

## Then work in groups to solve the following tasks in this script at first.
## When you have agreed on a solution, make a final solution script for the 
## below and hand that in with your word document. One script and one word 
## document per group.


# 1.	How many repeats are there per bird per year? 
# 2.	How many individuals did we capture per year for each 
#     sex? Compute the numbers, devise a useful table format, and 
#     fill it in.
# 3.	Think about how you can communicate (1) and (2) best 
#     in tables, and how you can visualise (1) and (2) using plots. 
#     Produce several solutions, and discuss in the group which 
#     the pros and cons for each solution to communicate and 
#     visualize the data structure for (1) and (2). 
# 4.	Write and submit two results sections for (1) and (2). 
#     Each result section should use different means of communicating 
#     the results, visually and in a table. Submit 1 word document, and 
#     1 script per group.


rm(list=ls())

getwd()
setwd( "/home/frcovell/CMEECourseWork/week4/sandbox")

d <-read.table("../data/SparrowSize.txt", header=T)
str(d)
head(d)
summary(d)


table(d$Year)
table(d$BirdID)
table(table(d$BirdID))

require(dplyr)
d$BirdID(454)


BirdIDCount <- d %>% count(BirdID,BirdID, sort = T)
BirdIDCount %>% count(n)
Num_Bird_Year <- d %>% group_by(Year) %>% count(BirdID)
Sex_Bird_Year <- d %>% group_by(Year) %>% count(Sex.1)

table(d$Sex.1, d$Year)
table(d$BirdID, d$Year)
max(d$BirdID)

hist(Sex_Bird_Year$Year)
shapiro.test(Sex_Bird_Year$Year)
