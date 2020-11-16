ftgram <- read.csv("C:/Users/brios/Desktop/project/data_txtfiles/fitnessgram2.csv")
#sub <- subset(ftgram, Year==2006)
#sub <- sub[,-c(1,2,3,5,6,7,8)]
#write.csv(sub,"C:/Users/brios/Desktop/nliquor.csv")

nliquor <- read.csv("C:/Users/brios/Desktop/project/data_txtfiles/nliquor.csv")

ftgram$nstores <- NA
ftgram$localdistrict <- NA
ftgram$ffprox <- NA
for(i in 1:nrow(ftgram)) {
  for(j in 1:nrow(nliquor)){
    if(ftgram$SCHL[i]==nliquor$SCHL[j]){
      ftgram$nstores[i] = nliquor$nliquor[j]
      ftgram$localdistrict[i] = nliquor$council_district[j]
      ftgram$ffprox[i] = nliquor$ffprox[j]
    }
  }
}

write.csv(ftgram, "C:/Users/brios/Desktop/project/data_txtfiles/fitnessgram3.csv")
rm(list=ls())
