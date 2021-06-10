############################################################################################################################################################
##   Recommendation Systems questions 
############################################################################################################################################################


library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")


#### Q1

ex1<-movielens %>% filter(!is.na(rating))%>%group_by(movieId,year)%>%summarize(n=n(),year = as.character(first(year)))

qplot(year,n,data=ex1,geom = "boxplot") +  coord_trans(y = "sqrt") +  theme(axis.text.x = element_text(angle = 90, hjust = 1))

v<-ex1%>%group_by(year)%>%summarize(m=median(n))

v$year[which.max(v$m)]

#### Q2

ex2<-movielens %>% filter(year>=1993)
ex2$timestamp<-as.Date(as.POSIXct(ex2$timestamp,tz = "UTC",origin = "1970-01-01"),format ="%Y-%m-%d")

##  The Shawshank Redemption

idx<-grep("Shawshank",ex2$title)
ex2$movieId[idx]
movielens %>% filter(movieId==318)%>%summarize(m=mean(rating))

## "Forrest Gump"

a<-ex2 %>% filter(title=="Forrest Gump")

a<-a%>% mutate(R_year=year(timestamp))

a2<-a%>%group_by(R_year)%>%summarize(n=n())

sum(a2$n)/(2018-min(a$year))


movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))



#### Q3
         
a<-movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId,year) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) 

a%>% ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()


#### Q5
movielens <- mutate(movielens, date = as_datetime(timestamp))
###as.Date(as.POSIXct(ex2$timestamp,tz = "UTC",origin = "1970-01-01"),format ="%Y-%m-%d")


#### Q6

movielens <- mutate(movielens,week=round_date(date,unit="week"))
movielens%>% group_by(week)%>%summarize(m=mean(rating))%>% ggplot(aes(week, m)) +
  geom_point() +
  geom_smooth()


#### Q8

gui<-movielens%>% group_by(genres)%>%summarize(n=n(),avg=mean(rating),se=sd(rating)/sqrt(n()))%>%filter(n>1000)

gui%>%ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


gui$genres[which.min(gui$avg)]
