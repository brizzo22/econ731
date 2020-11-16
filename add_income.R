library(dplyr)
library(reshape)
ftgram <- read.csv("C:/Users/brios/Desktop/project/data_txtfiles/fitnessgram3.csv")
ftgram <- ftgram[,-c(1,2,3)]
ftgram$income = NA

lapanel <- read.csv("C:/Users/brios/Desktop/project/la_panel.csv")
d4mean <- subset(lapanel, indicator==levels(indicator)[1])
d4median <- subset(lapanel, indicator==levels(indicator)[19])
summary(lapanel$category)
options(scipen = 99)

d4mean <- subset(d4mean, value < 90000)
d4mean$avginc = 0

k = c()
df <- d4mean %>% select(council_district,fiscal_year,value)
df$council_year <- sort(df$council_year)
totavg <- dcast(df, fiscal_year~council_district, mean)
totavg <- totavg[,-c(3:8,17)]
colnames(totavg) <- c("year","c1","c2","c3","c4","c5","c6","c7","c8","c9")
totavgLong <- reshape(totavg, varying = c("c1","c2","c3","c4","c5","c6","c7","c8","c9"), v.names="average",
                      timevar = "council",direction = "long")


for(i in 1:nrow(ftgram)) {
  for(j in 1:nrow(totavgLong)){
    if(ftgram$Year[i]==totavgLong$year[j]){
      if(ftgram$localdistrict[i] == totavgLong$council[j]) {
          ftgram$income[i] = totavgLong$average[j]      
        }
    }
  }
}

