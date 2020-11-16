rm(list=ls())

library(dplyr)
library(varhandle)
## LAUSD district code = 64733 
setwd("C:/Users/brios/Desktop/project/data_txtfiles")
create_subset1 <- function(df,year) {
  df$Year = year
  df$Gr07_Stu <- as.numeric(paste(df$Gr07_Stu))
  df$Gr05_Stu <- as.numeric(paste(df$Gr05_Stu))
  df$Gr09_Stu <- as.numeric(paste(df$Gr09_Stu))
  newdata <- df %>% filter(Dcode==64733,
                           Gr07_Stu>0,
                           Level==1,
                           RptType==1,
                           line_num==2,
                           SubGrp==0)#,
                           #charternum==c(0|0000))
  return(newdata)
}

create_subset2 <- function(df,year) {
  df$Year = year
  df$ChrtNum <- as.character(df$ChrtNum)
  df$NoStud5 <- as.numeric(paste(df$NoStud5))
  df$NoStud7 <- as.numeric(paste(df$NoStud7))
  df$NoStud9 <- as.numeric(paste(df$NoStud9))
  
  newdata <- df %>% filter(DIST==64733,
                          
                           NoStud7>0,
                           Level_Number==1,
                           Report_Number==0,
                           ChrtNum=='0000',
                           Line_Text==levels(Line_Text)[13])
  return(newdata)
}

find_diff <- function(df1,df2){
  school1 <- df1$Scode
  school2 <- df2$Scode
  uni1 <- unique(school1)
  uni2 <- unique(school2)
  k=c()
  for(i in 1:length(uni1)){
    for(j in 1:length(uni2)){
      if(uni1[i]==uni2[j]){
      k = c(k,uni1[i])  
      }
    }
  }
  return(k)
}

#x <- find_diff(y4,y5)
#x2 <- find_diff(y3,y4)
#test <- subset(y3, Scode%in%x2)

yr06 <- read.table("PhysFit2006.txt",header=TRUE, sep=",")
y1 <- create_subset1(yr06,2006)


yr07 <- read.delim("PhysFit2007.txt",header=T, sep=",")
y2 <- create_subset1(yr07,2007)

yr08 <- read.delim("PhysFit2008.txt",header=TRUE, sep=",")
y3 <- create_subset1(yr08, 2008)
y3 <- y3[-c(81:85),]

yr09 <- read.delim("PhysFit2009.txt",header=TRUE, sep=",")
y4 <- create_subset1(yr09, 2009)
y4 <- y4[-c(83:93),]

yr10 <- read.delim("PhysFit2010.txt",header=TRUE, sep=",")
y5 <- create_subset1(yr10, 2010)
y5 <- y5[-c(81:96),]
rm(yr06,yr07,yr08,yr09,yr10)

#### New function ####

yr11 <- read.delim("PhysFit2011.txt",header=TRUE)
y6 <- create_subset2(yr11, 2011)

yr12 <- read.delim("PhysFit2012.txt",header=T)
y7 <- create_subset2(yr12, 2012)

yr13 <- read.delim("PhysFit2013.txt",header=T,sep=",")
y8 <- create_subset2(yr13,2013)

rm(yr11,yr12,yr13)

### Combine dataframes ###
dat <- rbind(y1,y2,y3,y4,y5)
dat2 <- rbind(y6,y7,y8)

### unfactor numerical ###
dat$Gr7PctIn <- unfactor(dat$Gr7PctIn)
dat$Gr7PctNotIn <- unfactor(dat$Gr7PctNotIn)

dat2$Gr7PctNotIn = 0
dat2$Perc7b <- unfactor(dat2$Perc7b)
dat2$Perc7c <- unfactor(dat2$Perc7c)

dat2$Perc7b <- as.numeric(dat2$Perc7b)
dat2$Perc7c <- as.numeric(dat2$Perc7c)

for(i in 1:nrow(dat2)) {
  dat2$Gr7PctNotIn[i] = dat2$Perc7b[i] + dat2$Perc7c[i]
}

rm(yr11, yr12, yr13)

y1$ins = TRUE

in1 = y2$Scode %in% y1$Scode
y2$ins <- in1
y2 <- subset(y2, ins == TRUE)

in2 <- y3$Scode %in% y1$Scode
y3$ins <- in2
y3 <- subset(y3, ins==TRUE)

in3 <- y4$Scode %in% y1$Scode
y4$ins <- in3
y4 <- subset(y4, ins==TRUE)

in4 <- y5$Scode %in% y1$Scode
y5$ins <- in4
y5 <- subset(y5,ins == TRUE)

dat <- dat %>% select(Dcode, Scode,Gr07_Stu, Gr7PctIn, Gr7PctNotIn,Year)
dat2 <- dat2 %>% select(DIST, SCHL, NoStud7, Perc7a, Gr7PctNotIn, Year)


colnames(dat) <- c("DIST", "SCHL","Gr07_Stu" , "Gr7PctIn", "Gr7PctNotIn", "Year")
colnames(dat2) <- c("DIST", "SCHL","Gr07_Stu" , "Gr7PctIn", "Gr7PctNotIn", "Year")

finaldat <- rbind(dat,dat2)

write.csv(finaldat,"fitnessgram.csv")
rm(list=ls())

