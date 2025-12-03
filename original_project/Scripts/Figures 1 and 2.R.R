library(Synth)
library(readxl)
library(writexl)

#Data import####
synthdata <- read_excel("Data/processed data/synthdata.xlsx", 
                        col_types = c("numeric", "text", "text", 
                                      "text", "numeric", "date", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric",
                                      "text"))
View(synthdata)

#The synth package demands unit codes to be in a numeric format. Therefore, the first row is reformatted here.
y=1:10
i=1
while (i<=dim(synthdata)[1]/245) {
  y[((1+245*(i-1)):(245*i))]<-i
  i<-i+1
}

synthdata2<-as.data.frame(y)
synthdata2[,2:56]<-synthdata[2:56]
colnames(synthdata2)[1]<-"UnitNumeric"
synthdata<-synthdata2

synthdata$Date<-as.Date(synthdata$Date)
rm(synthdata2)

#setting the system language to english for plotting
Sys.setlocale("LC_TIME", "C")
# Figure 1####
png(filename="Figures and Tables/Figure 1 - COVID Incidence.png", 
    units="px", 
    width=1980, 
    height=1080, 
    pointsize=25, 
    res=72)

#plotting Hamburg
plot(synthdata$Date[synthdata$UnitNumeric==1 & synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
     synthdata$`covid incidence`[synthdata$UnitNumeric==3 & synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
     type="l",lwd=4, col=2, #line type
     ylim = c(0,quantile(synthdata$`covid incidence`,0.98, na.rm=TRUE)),
     main="Incidence of COVID-19 Cases in German States", #sub="Germany and German Federal States",
     ylab = "Seven Days Incidence",xlab=""
     
)

#plotting MV
lines(synthdata$Date[synthdata$UnitNumeric==1 & synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
      synthdata$`covid incidence`[synthdata$UnitNumeric==14 & synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
      type="l",lwd=4,col=3)

#Other states
i=1
donorstates=c(2,4,5,6,7,8,9,10,11,12,13,15,16,17)
while (i<=14) {
  lines(synthdata$Date[synthdata$UnitNumeric==1 & synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
        synthdata$`covid incidence`[synthdata$UnitNumeric==donorstates[i]& synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
        type="l",col=8,lty=2,
        lwd=1.5)
  i<-i+1
}

#Low-level editing
legend(x="topright",legend=c("Hamburg","Mecklenburg-Vorpommern","Other States"),
       fill = c(2,3,8),
       cex = 1)

abline(v = as.Date("2022-04-03"),lty=1,col=8)
dev.off()


# Figure 2####

png(filename="Figures and Tables/Figure 2 - Hospitalization Incidence.png", 
    units="px", 
    width=1980, 
    height=1080, 
    pointsize=25, 
    res=72)
#Hamburg
plot(synthdata$Date[synthdata$UnitNumeric==1 & synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
     synthdata$`Hospitalization incidence 00+`[synthdata$UnitNumeric==14 & synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
     type="l",lwd=3, col=3, #line type
     #ylim = c(0,quantile(synthdata$`Hospitalization incidence 00+`,0.98, na.rm=TRUE)),
     main="Incidence of Hospitalized COVID-19 Cases in German States", #sub="Germany and German Federal States",
     ylab = "Seven Days Hospitalization Incidence",xlab=""
     
)

#MV
lines(synthdata$Date[synthdata$UnitNumeric==1 & synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
      synthdata$`Hospitalization incidence 00+`[synthdata$UnitNumeric==3 & synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
      type="l",lwd=3,col=2)

#Other states
i=1
donorstates=c(2,4,5,6,7,8,9,10,11,12,13,15,16,17)
while (i<=14) {
  lines(synthdata$Date[synthdata$UnitNumeric==1 & synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
        synthdata$`Hospitalization incidence 00+`[synthdata$UnitNumeric==donorstates[i] & synthdata$Date %in% seq(from = as.Date("2022-02-01"), to = as.Date("2022-06-01"), by="day")],
        type="l",col=8,lty=2)
  i<-i+1
}

#Low-level editing

legend(x="topright",legend=c("Hamburg","Mecklenburg-Vorpommern","Other States"),
       fill = c(2,3,8), cex = 1)

abline(v = as.Date("2022-04-03"),lty=1,col=8)

dev.off()
