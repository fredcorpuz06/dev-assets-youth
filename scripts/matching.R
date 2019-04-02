library(tableone)
library(Matching)
library(magrittr)
library(readxl)
library(tidyverse)
df <- read_excel('./Durham-Middlefield-Youth-and-Family/2015/ab52064x SIS data file.xlsx')
names(df)


##-------------------
## Matching variables
##-------------------
## External variables
commit_learning <- c("Q26","Q28","Q13","Q34","Q35",
                     "Q36","Q37","Q20","Q49","Q65")
pos_value <- c("Q7","Q9","Q14","Q8","Q11",
               "Q15","Q16","Q17","Q18","Q19",
               "Q39","Q47")
social_comp <- c("Q72","Q81","Q71","Q77","Q78",
                 "Q74","Q79","Q80","Q70","Q76",
                 "Q135")
pos_iden <- c("Q119","Q141","Q38","Q41","Q43","Q44","Q113","Q120")

## Deficits
deficits <- c("Q152","Q150","Q153","Q154","Q102")

race <- c("RaceIndian","RaceAsian","RaceBlack",
            "RaceHispanic","RaceIslander","RaceWhite","RaceOther")

matching_vars <- c(commit_learning, pos_value, social_comp, 
                   pos_iden, deficits, race, 'CollectorID')

## Treatment variables
treat_vars <- c("Q25","Q149","Q115",
                "Q67","Q63","Q69")

## Outcome variable
y_var <- 'THRIVE8'

bad_smd <- c('Q26','Q28','Q34','Q49','Q74','Q119',
             "Q120","Q38","Q7","Q8","Q47","Q78","Q43")

matching_vars <- setdiff(matching_vars, bad_smd)
##-------------------
## Data management
##----------------
# my_facs <- 
df2 <- df %>% 
  select(matching_vars, treat_vars, y_var) %>% 
  mutate_at(race, function(x) ifelse(is.na(x), 0, x)) %>% 
  mutate(CollectorID = ifelse(CollectorID == 'Coginchaug Regional High School - Durham Middlefield',
                              1,0)) %>% 
  na.omit() 
df2

sapply(df2, function(x) sum(is.na(x)))




##-------------------
## Data exploration
##------------------
# par(mfrow = c(2,2))


ggplot(df2, aes(x = Q115, y = THRIVE8)) +
  stat_summary(geom = 'bar', fun.y = mean)
ggplot(df2, aes(x = Q67, y = THRIVE8)) +
  stat_summary(geom = 'bar', fun.y = mean)
ggplot(df2, aes(x = Q63, y = THRIVE8)) +
  stat_summary(geom = 'bar', fun.y = mean)
ggplot(df2, aes(x = Q69, y = THRIVE8)) +
  stat_summary(geom = 'bar', fun.y = mean)
ggplot(df2, aes(x = parentSchool, y = THRIVE8)) +
  stat_summary(geom = 'bar', fun.y = mean)

##------------------
## Matching per treatment
##------------------
ggplot(df2, aes(x = Q25, y = THRIVE8)) +
  stat_summary(geom = 'bar', fun.y = mean)
ggplot(df2, aes(x = Q25)) + 
  geom_bar() ## look at frequencies
df2$parentSchool <- ifelse(df2$Q25 <= 2, 1, 0)
table1 <- df2 %>% 
  select(matching_vars, y_var, 'parentSchool') %>% 
  CreateTableOne(vars = matching_vars,
                         strata = 'parentSchool',
                         data = .,
                         test = FALSE)
print(table1,smd=TRUE)
set.seed(1234)
greedymatch<- Match(Tr=df2$parentSchool,
                    M=1,
                    X=df2[matching_vars],
                    replace=FALSE, 
                    caliper = 2.0)
matching_idx <- unlist(greedymatch[c("index.treated","index.control")])
matched<-df2[matching_idx, ]

#get table 1 for matched data with standardized differences
matchedtab1<-CreateTableOne(vars=matching_vars, strata ="parentSchool", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)
y_trt <- matched %>% 
  filter(parentSchool == 1) %>% 
  .$THRIVE8

y_con <- matched %>% 
  filter(parentSchool == 0) %>% 
  .$THRIVE8

mod1 <- t.test(y_trt, y_con, paired = TRUE)
mod1
ggplot(df2, aes(x = factor(parentSchool), y = THRIVE8, fill = factor(parentSchool))) +
  stat_summary(geom = 'bar', fun.y = mean) +
  labs(title = 'Do parents attend meetings/events at your school?') +
  theme(legend.position = "none")

  

##-----------------------------
## Propensity matching
##-----------------------------
for_prop <- df2 %>% 
  select(matching_vars, 'parentSchool')
mod1 <- glm(parentSchool ~ ., family = binomial(),
    data = for_prop)
summary(mod1)

pscore <- mod1$fitted.values

ggplot() +
  geom_density(aes(pscore, color = factor(df2$parentSchool))) + 
  xlab("Propensity Score")+
  scale_color_manual("Group Assignment", values=c("blue","orange"))+
  ggtitle("Pre-Matched Propensity Scores by Treatment")


#do greedy matching on propensity score using Match with a caliper
psmatch <- Match(Tr = df2$parentSchool,
                 M = 1, X = pscore,
                 replace = FALSE,
                 caliper = .2)


matching_idx <- unlist(psmatch[c("index.treated","index.control")])
matched <- df2[matching_idx, ]


matchedtab1<-CreateTableOne(vars=matching_vars, strata ="parentSchool", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)
y_trt <- matched %>% 
  filter(parentSchool == 1) %>% 
  .$THRIVE8

y_con <- matched %>% 
  filter(parentSchool == 0) %>% 
  .$THRIVE8

mod1 <- t.test(y_trt, y_con, paired = TRUE)
mod1
ggplot(df2, aes(x = factor(parentSchool), y = THRIVE8, fill = factor(parentSchool))) +
  stat_summary(geom = 'bar', fun.y = mean) +
  labs(title = 'Do parents attend meetings/events at your school?') +
  theme(legend.position = "none")




##----------------------------------
ggplot(df2, aes(x = Q149, y = THRIVE8)) +
  stat_summary(geom = 'bar', fun.y = mean)
ggplot(df2, aes(x = Q149)) + 
  geom_bar()
df2$noAdultFriend <- ifelse(df2$Q149 <= 3, 1, 0)
ggplot(df2, aes(x = noAdultFriend)) +
  geom_bar()
table1 <- df2 %>% 
  select(matching_vars, y_var, 'noAdultFriend') %>% 
  CreateTableOne(vars = matching_vars,
                 strata = 'noAdultFriend',
                 data = .,
                 test = FALSE)
print(table1,smd=TRUE)
set.seed(1234)
greedymatch<- Match(Tr=df2$noAdultFriend,
                    M=1,
                    X=df2[matching_vars],
                    replace=FALSE)
matching_idx <- unlist(greedymatch[c("index.treated","index.control")])
matched<-df2[matching_idx, ]

#get table 1 for matched data with standardized differences
matchedtab1<-CreateTableOne(vars=matching_vars, strata ="noAdultFriend", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)
y_trt <- matched %>% 
  filter(noAdultFriend == 1) %>% 
  .$THRIVE8

y_con <- matched %>% 
  filter(noAdultFriend == 0) %>% 
  .$THRIVE8

mod1 <- t.test(y_trt, y_con, paired = TRUE)
mod1
ggplot(df2, aes(x = factor(noAdultFriend), y = THRIVE8, fill = factor(noAdultFriend))) +
  stat_summary(geom = 'bar', fun.y = mean) +
  labs(title = 'Do you have 3+ adult friends you regularly talk to?') +
  theme(legend.position = "none")


##----------------------------------
ggplot(df2, aes(x = Q115, y = THRIVE8)) +
  stat_summary(geom = 'bar', fun.y = mean)
ggplot(df2, aes(x = Q115)) + 
  geom_bar()
df2$adultsListen <- ifelse(df2$Q115 <= 2, 1, 0)
table1 <- df2 %>% 
  select(matching_vars, y_var, 'adultsListen') %>% 
  CreateTableOne(vars = matching_vars,
                 strata = 'adultsListen',
                 data = .,
                 test = FALSE)
print(table1,smd=TRUE)
set.seed(1234)
greedymatch<- Match(Tr=df2$adultsListen,
                    M=1,
                    X=df2[matching_vars],
                    replace=FALSE)
matching_idx <- unlist(greedymatch[c("index.treated","index.control")])
matched<-df2[matching_idx, ]

#get table 1 for matched data with standardized differences
matchedtab1<-CreateTableOne(vars=matching_vars, strata ="adultsListen", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)
y_trt <- matched %>% 
  filter(adultsListen == 1) %>% 
  .$THRIVE8

y_con <- matched %>% 
  filter(adultsListen == 0) %>% 
  .$THRIVE8

mod1 <- t.test(y_trt, y_con, paired = TRUE)
mod1
ggplot(df2, aes(x = factor(adultsListen), y = THRIVE8, fill = factor(adultsListen))) +
  stat_summary(geom = 'bar', fun.y = mean) +
  labs(title = 'Adults in my city/town listen to what I have to say') +
  theme(legend.position = "none")


##----------------------------------
ggplot(df2, aes(x = Q67, y = THRIVE8)) +
  stat_summary(geom = 'bar', fun.y = mean)
ggplot(df2, aes(x = Q67)) + 
  geom_bar()
df2$doVolunteer <- ifelse(df2$Q115 =< 1, 1, 0)
table1 <- df2 %>% 
  select(matching_vars, y_var, 'doVolunteer') %>% 
  CreateTableOne(vars = matching_vars,
                 strata = 'doVolunteer',
                 data = .,
                 test = FALSE)
print(table1,smd=TRUE)
set.seed(1234)
greedymatch<- Match(Tr=df2$doVolunteer,
                    M=1,
                    X=df2[matching_vars],
                    replace=FALSE)
matching_idx <- unlist(greedymatch[c("index.treated","index.control")])
matched<-df2[matching_idx, ]

#get table 1 for matched data with standardized differences
matchedtab1<-CreateTableOne(vars=matching_vars, strata ="doVolunteer", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)
y_trt <- matched %>% 
  filter(doVolunteer == 1) %>% 
  .$THRIVE8

y_con <- matched %>% 
  filter(doVolunteer == 0) %>% 
  .$THRIVE8

mod1 <- t.test(y_trt, y_con, paired = TRUE)
mod1
ggplot(df2, aes(x = factor(doVolunteer), y = THRIVE8, fill = factor(doVolunteer))) +
  stat_summary(geom = 'bar', fun.y = mean) +
  labs(title = "I don't do any volunteer work") +
  theme(legend.position = "none")


##----------------------------------
ggplot(df2, aes(x = Q63, y = THRIVE8)) +
  stat_summary(geom = 'bar', fun.y = mean)
ggplot(df2, aes(x = Q63)) + 
  geom_bar()
df2$moreClubs <- ifelse(df2$Q115 >= 4, 1, 0)
table1 <- df2 %>% 
  select(matching_vars, y_var, 'moreClubs') %>% 
  CreateTableOne(vars = matching_vars,
                 strata = 'moreClubs',
                 data = .,
                 test = FALSE)
print(table1,smd=TRUE)
set.seed(1234)
greedymatch<- Match(Tr=df2$moreClubs,
                    M=1,
                    X=df2[matching_vars],
                    replace=FALSE)
matching_idx <- unlist(greedymatch[c("index.treated","index.control")])
matched<-df2[matching_idx, ]

#get table 1 for matched data with standardized differences
matchedtab1<-CreateTableOne(vars=matching_vars, strata ="moreClubs", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)
y_trt <- matched %>% 
  filter(moreClubs == 1) %>% 
  .$THRIVE8

y_con <- matched %>% 
  filter(moreClubs == 0) %>% 
  .$THRIVE8

mod1 <- t.test(y_trt, y_con, paired = TRUE)
mod1
ggplot(df2, aes(x = factor(moreClubs), y = THRIVE8, fill = factor(moreClubs))) +
  stat_summary(geom = 'bar', fun.y = mean) +
  labs(title = "I spend 3+ hours at school clubs/organizations") +
  theme(legend.position = "none")



##----------------------------------
ggplot(df2, aes(x = Q69, y = THRIVE8)) +
  stat_summary(geom = 'bar', fun.y = mean)
ggplot(df2, aes(x = Q69)) + 
  geom_bar()
df2$moreArt <- ifelse(df2$Q69 >= 4, 1, 0)
table1 <- df2 %>% 
  select(matching_vars, y_var, 'moreArt') %>% 
  CreateTableOne(vars = matching_vars,
                 strata = 'moreArt',
                 data = .,
                 test = FALSE)
print(table1,smd=TRUE)
set.seed(1234)
greedymatch<- Match(Tr=df2$moreArt,
                    M=1,
                    X=df2[matching_vars],
                    replace=FALSE)
matching_idx <- unlist(greedymatch[c("index.treated","index.control")])
matched<-df2[matching_idx, ]

#get table 1 for matched data with standardized differences
matchedtab1<-CreateTableOne(vars=matching_vars, strata ="moreArt", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)
y_trt <- matched %>% 
  filter(moreArt == 1) %>% 
  .$THRIVE8

y_con <- matched %>% 
  filter(moreArt == 0) %>% 
  .$THRIVE8

mod1 <- t.test(y_trt, y_con, paired = TRUE)
mod1
ggplot(df2, aes(x = factor(moreArt), y = THRIVE8, fill = factor(moreArt))) +
  stat_summary(geom = 'bar', fun.y = mean) +
  labs(title = "I spend 3+ hours working on music, art, drama") +
  theme(legend.position = "none")
