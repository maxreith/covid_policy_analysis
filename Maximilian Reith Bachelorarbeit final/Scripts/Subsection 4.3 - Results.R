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
#estimation setup, defining variables for the functions dataprep and synth####
#The following variables are passed on to the functions synth and dataprep of the package Synth.

var.dependent = "14 days covid incidence growth rate" #defining the dependent variable

var.special.predictors = list( #(predictor, observation date, operator), an operator has to be specified. Therefore, the mean is chosen, even though no averaging occurs. 
  list("Third dose vaccinations", 93 , "mean"),
  list("Unemployment rate in relation to employed labor force", 93 , "mean"),
  list("Population Density", 93, "mean"),
  list("14 days covid incidence growth rate", 65 , "mean"),
  list("14 days covid incidence growth rate", 72 , "mean"),
  list("14 days covid incidence growth rate", 93 , "mean")
)

optimization.period = synthdata$DateNumeric[synthdata$UnitNumeric==1 & synthdata$Date %in% seq(from = as.Date("2022-02-21"), to = as.Date("2022-04-03"), by="day")]
plot.period = synthdata$DateNumeric[synthdata$UnitNumeric==1 & synthdata$Date %in% seq(from = as.Date("2022-02-21"), to = as.Date("2022-05-15"), by="day")]

pool<-synthdata[c(which(synthdata$`County type`=="SK" & synthdata$StateId%in%c(1,3,12,15) &synthdata$DateNumeric==1),419*245),
                c(1,4)]
treated.unit=treated.unit=pool$UnitNumeric[pool$Name=="SK MV aggregated"]

#MSPE restriction
MSPErestrict = 5 #only placebo controls are considered, that have an MSPE of less than five times that of the original control
# running the functions dataprep & synth####
dataprep.out.orig <- dataprep(
  foo = synthdata,
  special.predictors = var.special.predictors,
  
  time.predictors.prior = synthdata$DateNumeric[which(synthdata$AdmUnitId=="0" 
                                                      & synthdata$Date%in%seq(as.Date("2022-03-18"),by="day",length.out=14))], #as I am using special.predictors with individual time frames, this line has no effect, but needs to be included for the synth- function to run
  
  dependent = var.dependent,
  unit.variable = "UnitNumeric",
  unit.names.variable = "Name",
  time.variable = "DateNumeric",
  
  treatment.identifier = treated.unit,
  controls.identifier = pool$UnitNumeric[-which(pool$UnitNumeric==treated.unit)], #as controls, all units from the pool, excluding the treated.unit, are chosen   
  
  time.optimize.ssr = optimization.period, #period over which loss funcion is minimized
  
  time.plot = plot.period
)

synth.out.orig <- synth(data.prep.obj = dataprep.out.orig, method="BFGS")
synth.tables.orig <- synth.tab(dataprep.res = dataprep.out.orig,
                               synth.res = synth.out.orig)
synth.tables.orig
# Table 9####
write_xlsx(data.frame(as.vector(rownames(synth.tables.orig$tab.pred)[c(4:6,2,3,1)]), #row names, the vector c(4:6,2,3,1) correct the order of rows to fit the final text
                      synth.tables.orig$tab.pred[c(4:6,2,3,1),], #donor unit weights w
                      unlist(synth.tables.orig$tab.v)[c(4:6,2,3,1)]), #predictor weights v
           "Figures and Tables/Table 9.xlsx") 
# Table 10####
write_xlsx(data.frame(synth.tables.orig$tab.w), 
           "Figures and Tables/Table 10.xlsx") 

# Figure 13: treated and synthetic####
png(filename="Figures and Tables/Figure 13 - MV Cities Growth Rate _ Synthetic Control.png", 
    units="px", 
    width=1980, 
    height=1080, 
    pointsize=25, 
    res=72)

plot(synthdata$Date[plot.period],dataprep.out.orig$Y1plot,type="l",lwd=4,
     xlab = "", ylab = "Growth rate of 14-days incidence",
     ylim = c(-65,74),
     cex.lab = 1.5)

lines(synthdata$Date[plot.period],dataprep.out.orig$Y0plot %*% synth.out.orig$solution.w,lwd=4,lty=2)
abline(v = synthdata$Date[synthdata$UnitNumeric==1 
                          & synthdata$Date==as.Date("2022-04-03")])
legend(x="topright", legend=c("Aggregated Cities of MV","Synthetic Control"),
       lty = c(1,2), lwd = c(4,4), cex = 1.5)
dev.off()
# Figure 14: placebo tests####

png(filename="Figures and Tables/Figure 14 - MV Cities Growth Rate _ Placebo Tests.png", 
    units="px", 
    width=1980, 
    height=1080, 
    pointsize=25, 
    res=72)

#creating list
placebolistMV<-list()
a<-list(dataprep.out.orig$Y1plot,dataprep.out.orig$Y0plot,synth.out.orig,synth.tables.orig)
names(a)<-c("Y1","Y0","synth.out","synth.tables")
placebolistMV[length(placebolistMV)+1] <- list(a)
names(placebolistMV)[length(placebolistMV)] <- "original treatment unit"

#gaps plot
plot(synthdata$Date[plot.period],
     dataprep.out.orig$Y1plot - dataprep.out.orig$Y0plot %*% synth.out.orig$solution.w,
     type = "l", lwd = 4,
     ylab = "Outcome variable gap", xlab="",
     cex.lab = 1.5,
     main = "Gaps: Treated - Synthetic",
     ylim= c(-40,40))
abline(h = 0,lty = 2,lwd=3)
abline(v = synthdata$Date[synthdata$UnitNumeric==1 
                          & synthdata$Date==as.Date("2022-04-03")])

#loop
for (unit in pool$UnitNumeric) {
  #jump original treatment unit
  if (unit==treated.unit){next}
  
  #Creating Synthetic Control
  dataprep.out <- dataprep(
    foo = synthdata,
    special.predictors = var.special.predictors,
    time.predictors.prior = synthdata$DateNumeric[which(synthdata$AdmUnitId=="0" 
                                                        & synthdata$Date%in%seq(as.Date("2022-03-18"),by="day",length.out=14))], #as I am using special.predictors with individual time frames, this line has no effect, but needs to be included for the synth- function to run
    dependent = var.dependent,
    unit.variable = "UnitNumeric",
    unit.names.variable = "Name",
    time.variable = "DateNumeric",
    treatment.identifier = unit,
    controls.identifier = pool$UnitNumeric[-which(pool$UnitNumeric==unit)],
    time.optimize.ssr = optimization.period, #period over which loss funcion is minimized
    time.plot = plot.period
  )
  synth.out <- synth(data.prep.obj = dataprep.out, method="BFGS")
  synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
  
  #skip, if MSPE exceeds restriction
  if (synth.tables$tab.loss[2]>MSPErestrict*placebolistMV[[1]][[4]]$tab.loss[2])
  {next}
  
  #Creating Output List
  a<-list(dataprep.out$Y1plot,dataprep.out$Y0plot,synth.out,synth.tables)
  names(a)<-c("Y1","Y0","synth.out","synth.tables")
  placebolistMV[length(placebolistMV)+1] <- list(a)
  names(placebolistMV)[length(placebolistMV)] <- unit
  
  lines(synthdata$Date[plot.period],dataprep.out$Y1plot - dataprep.out$Y0plot %*% synth.out$solution.w)
  print(unit)
}


dev.off()


