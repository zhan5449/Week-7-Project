# R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(GGally)


# Data Import and Cleaning
week7_tbl <- read_csv("../Data/week3.csv") %>%
  mutate(timeStart=as.POSIXct(timeStart),
         timeEnd=as.POSIXct(timeEnd),
         gender=factor(gender,levels=c("M","F"),labels=c("Male","Female")),
         condition=factor(condition,levels=c("A","B","C"),labels=c("Block A","Block B","Control"))) %>%
  filter(q6==1) %>%
  select(-q6)

# Visualization
ggpairs(select(week7_tbl,q1:q10),method="pearson",density=T)
ggplot(week7_tbl,aes(x=timeStart,y=q1))+
  geom_point()+
  labs(x="Date of Experiment",
       y="Q1 Score")
ggplot(week7_tbl,aes(x=q1,y=q2,col=gender))+
  geom_jitter()
ggplot(week7_tbl,aes(x=q1,y=q2))+
  geom_jitter()+
  facet_grid(.~gender)+
  labs(x="Score on Q1",
       y="Score on Q2")
week7_tbl <- week7_tbl%>%
  mutate(`Time Elapsed (secs)`=difftime(timeStart,timeEnd,units=c("secs")))
ggplot(week7_tbl,aes(x=gender,y=`Time Elapsed (secs)`))+
  geom_boxplot()

ggplot(week7_tbl,aes(x=q5,y=q7,col=condition))+
  geom_jitter()+
  geom_smooth(method="lm",se=F)+
  labs(x="Score on Q5",
       y="Score on Q7",
       col="Experimental Condition")+
  theme(legend.position="bottom",
        panel.background=element_blank(),
        legend.background=element_rect(fill="lightgrey"))
  
