library(tidyverse)
library(readxl)
library(vcd)
df <- read_excel('./Durham-Middlefield-Youth-and-Family/2015/ab52064x SIS data file.xlsx')
head(df)
names(df)

missing <- sapply(df, function(x) is.na(x) %>% sum)
sort(round(missing / dim(df)[1], digits = 2))
hist(missing) ## not bad, most of the data is < 5% missing

## Q21: What grades do you earn in school? (1: mostly A --> 8: mostly D)
table(df$Q21)

## Q22: How often does 1 of your parents help you with school work? (1: very often)
table(df$Q22)

table(df$Q21, df$Q22)

t <- structable(Q21 ~ Q22, df) 
mosaic(t, shade = TRUE, legend = TRUE) ## no group pops out badly 




##-----------------
## 2017
##--------------------
df2 <- read_excel('./Durham-Middlefield-Youth-and-Family/2017/Copy of df10883 (002).xlsx')

names(df2)
missing <- sapply(df2, function(x) is.na(x) %>% sum)
sort(round(missing / dim(df2)[1], digits = 2))
hist(missing) ## bit worse than 2015 but still good 


table(df2$L19)

