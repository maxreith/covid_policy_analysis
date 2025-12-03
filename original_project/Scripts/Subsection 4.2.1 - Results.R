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
var.dependent = "14 days hospitalization incidence growth rate" #defining the dependent variable
var.special.predictors = list( #(predictor, observation date, operator), an operator has to be specified. Therefore, the mean is chosen, even though no averaging occurs. 
  list("Third dose vaccinations", 93 , "mean"),
  list("Unemployment rate in relation to employed labor force", 93 , "mean"),
  list("Population Density", 93, "mean"),
  list("14 days hospitalization incidence growth rate", 65 , "mean"),
  list("14 days hospitalization incidence growth rate", 72 , "mean"),
  list("14 days hospitalization incidence growth rate", 93 , "mean")
)

optimization.period = synthdata$DateNumeric[synthdata$UnitNumeric==1 & synthdata$Date %in% seq(from = as.Date("2022-02-21"), to = as.Date("2022-04-03"), by="day")]
plot.period = synthdata$DateNumeric[synthdata$UnitNumeric==1 & synthdata$Date %in% seq(from = as.Date("2022-02-21"), to = as.Date("2022-05-15"), by="day")]

pool<-synthdata[which(synthdata$UnitNumeric%in%c(2,4,6:11,13:17) &synthdata$DateNumeric==1),c(1,4)]
treated.unit=pool$UnitNumeric[pool$Name=="Mecklenburg-Vorpommern"]
print(pool)

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
# Table 5####
write_xlsx(data.frame(as.vector(rownames(synth.tables.orig$tab.pred)[c(4:6,2,3,1)]), #row names, the vector c(4:6,2,3,1) correct the order of rows to fit the final text
                      synth.tables.orig$tab.pred[c(4:6,2,3,1),], #donor unit weights w
                      unlist(synth.tables.orig$tab.v)[c(4:6,2,3,1)]), #predictor weights v
           "Figures and Tables/Table 5.xlsx") 
# Table 6####
write_xlsx(data.frame(synth.tables.orig$tab.w), 
           "Figures and Tables/Table 6.xlsx") 

# Figure 10: treated and synthetic####
png(filename="Figures and Tables/Figure 10 - MV Hospitalizations Growth Rate _ Synthetic Control.png", 
    units="px", 
    width=1980, 
    height=1080, 
    pointsize=25, 
    res=72)

plot(synthdata$Date[plot.period],dataprep.out.orig$Y1plot,type="l",lwd=4,
     xlab = "", ylab = "Growth rate of 14-days hospitalization incidence",
     ylim = c(-50,74),
     cex.lab = 1.5)

lines(synthdata$Date[plot.period],dataprep.out.orig$Y0plot %*% synth.out.orig$solution.w,lwd=4,lty=2)
abline(v = synthdata$Date[synthdata$UnitNumeric==1 
                          & synthdata$Date==as.Date("2022-04-03")])
legend(x="topright", legend=c("Mecklenburg-Vorpommern","Synthetic Mecklenburg-Vorpommern"),
       lty = c(1,2), lwd = c(4,4), cex = 1.5)
dev.off()
# Figure 11: placebo tests####

png(filename="Figures and Tables/Figure 11 - MV Hospitalization Growth Rate _ Placebo Tests.png", 
    units="px", 
    width=1980, 
    height=1080, 
    pointsize=25, 
    res=72)

#creating a list that stores the results of the placebo estimations
placebolistMV.hosp<-list()
a<-list(dataprep.out.orig$Y1plot,dataprep.out.orig$Y0plot,synth.out.orig,synth.tables.orig)
names(a)<-c("Y1","Y0","synth.out","synth.tables")
placebolistMV.hosp[length(placebolistMV.hosp)+1] <- list(a)
names(placebolistMV.hosp)[length(placebolistMV.hosp)] <- "original treatment unit"

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
  
  
  #Creating Output List
  a<-list(dataprep.out$Y1plot,dataprep.out$Y0plot,synth.out,synth.tables)
  names(a)<-c("Y1","Y0","synth.out","synth.tables")
  placebolistMV.hosp[length(placebolistMV.hosp)+1] <- list(a)
  names(placebolistMV.hosp)[length(placebolistMV.hosp)] <- unit
  
  #skip, if MSPE exceeds restriction
  if (synth.tables$tab.loss[2]>MSPErestrict*placebolistMV.hosp[[1]][[4]]$tab.loss[2])
  {next}
  
  lines(synthdata$Date[plot.period],dataprep.out$Y1plot - dataprep.out$Y0plot %*% synth.out$solution.w)
  print(unit)
}


dev.off()



# Figure 15 - Appendix: placebo tests, Post/Pre MPSE####

i=1
PostPre<-c()
while (i<= length(placebolistMV.hosp)) { #this loop uses the data stored in placebolistMV.hosp to calculate the Post/Pre Interventions MSPEs for all synthetic controls (subject to the constraint, that the pretreatment MSPE is not 'MSPErestrict' times bigger than the original MSPE)
  PostPre[length(PostPre)+1]<-
    (sum((placebolistMV.hosp[[i]][[1]][c((1+length(optimization.period)):length(plot.period)),]- #Post-Intervention MSPE
            placebolistMV.hosp[[i]][[2]][c((1+length(optimization.period)):length(plot.period)),] %*% placebolistMV.hosp[[i]][[3]]$solution.w)^2#Post-Intervention MSPE
    )/(length(plot.period)-length(optimization.period)))/ 
    (sum((placebolistMV.hosp[[i]][[1]][1:length(optimization.period),] - #Pre-Intervention MSPE
            placebolistMV.hosp[[i]][[2]][1:length(optimization.period),] %*% placebolistMV.hosp[[i]][[3]]$solution.w)^2
    )/length(optimization.period))
  i<-i+1
}
plot(table(round(PostPre,digits = 0)),
     main = "Post-MSPE/Pre-MSPE")

png(filename="Figures and Tables/Figure 15 - MV Hospitalizations Growth Rate _ PrePost MSPE.png", 
    units="px", 
    width=1080, 
    height=1080, 
    pointsize=25, 
    res=72)


hist(round(PostPre,digits = 0), breaks = seq(min(round(PostPre,digits = 0))-0.5,
                                             max(round(PostPre,digits = 0))+0.5, 
                                             length.out = max(round(PostPre,digits = 0))+2),
     main = "Pre-MSPE/Post-MSPE",
     xlab="",xlim = c(0,30), ylim = c(0,6)
)

dev.off()


# Figure 16 - Appendix: robustness checks - dropping donor unit####
png(filename="Figures and Tables/Figure 16 - MV Hospitalization Growth Rate _ Leave-one-out Donors.png", 
    units="px", 
    width=1980, 
    height=1080, 
    pointsize=25, 
    res=72)

#creating list
robustnesslistHH<-list()
a<-list(dataprep.out.orig$Y1plot,dataprep.out.orig$Y0plot,synth.out.orig,synth.tables.orig)
names(a)<-c("Y1","Y0","synth.out","synth.tables")
robustnesslistHH[length(robustnesslistHH)+1] <- list(a)
names(robustnesslistHH)[length(robustnesslistHH)] <- "original treatment unit"

#plot of treated unit and synthetic control
plot(synthdata$Date[plot.period],dataprep.out.orig$Y1plot,type="l",lwd=5,
     xlab = "", ylab = "Growth rate of 14-days hospitalization incidence",
     ylim = c(-50,74),
     cex.lab = 1.5)

lines(synthdata$Date[plot.period],dataprep.out.orig$Y0plot %*% synth.out.orig$solution.w,lwd=5,lty=2)
abline(v = synthdata$Date[synthdata$UnitNumeric==1 
                          & synthdata$Date==as.Date("2022-04-03")])
legend(x="topright", legend=c("Mecklenburg-Vorpommern","Synthetic Mecklenburg-Vorpommern","leave-one-out estimates"),
       lty = c(1,2,1), 
       cex = 1.5,
       lwd = c(4,4,4) )

#loop going through all units in donor pool
for (unit in pool$UnitNumeric) {
  #jump original treatment unit
  if (unit==treated.unit){next}
  if(round(synth.out.orig$solution.w,digits = 4)[which(pool$UnitNumeric[-which(pool$UnitNumeric==treated.unit)]==unit),]==0){next} #skips, if unit in donor pool has a weight w = 0 in original synthetic control estimation
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
    treatment.identifier = treated.unit,
    controls.identifier = pool$UnitNumeric[-which(pool$UnitNumeric%in%c(treated.unit,unit))],
    time.optimize.ssr = optimization.period, #period over which loss funcion is minimized
    time.plot = plot.period
  )
  synth.out <- synth(data.prep.obj = dataprep.out, method="BFGS")
  synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
  
  
  #Creating Output List
  a<-list(dataprep.out$Y1plot,dataprep.out$Y0plot,synth.out,synth.tables)
  names(a)<-c("Y1","Y0","synth.out","synth.tables")
  robustnesslistHH[length(robustnesslistHH)+1] <- list(a)
  names(robustnesslistHH)[length(robustnesslistHH)] <- paste(as.character(unit), " dropped")
  
  #Lines
  lines(synthdata$Date[plot.period], dataprep.out$Y0plot %*% synth.out$solution.w)
  print(unit)
}
dev.off()




# Figure 17 - Appendix: robustness checks - dropping predictor####
png(filename="Figures and Tables/Figure 17 - MV Hospitalization Growth Rate _ Leave-one-out Predictors.png", 
    units="px", 
    width=1980, 
    height=1080, 
    pointsize=25, 
    res=72)


robustnesslist2.MV.hospitalizations<-list() #creating a list that contains all synthetic control estimations conducted for this robustness test. This list won't be used for further results, but has a purpose for double-checking results
a<-list(dataprep.out.orig$Y1plot,dataprep.out.orig$Y0plot,synth.out.orig,synth.tables.orig)
names(a)<-c("Y1","Y0","synth.out","synth.tables")
robustnesslist2.MV.hospitalizations[length(robustnesslist2.MV.hospitalizations)+1] <- list(a)
names(robustnesslist2.MV.hospitalizations)[length(robustnesslist2.MV.hospitalizations)] <- "original treatment unit"

#plot of treated unit and synthetic control
plot(synthdata$Date[plot.period],dataprep.out.orig$Y1plot,type="l",lwd=5,
     xlab = "", ylab = "Growth rate of 14-days hospitalization incidence",
     ylim = c(-50,74),
     cex.lab = 1.5)

lines(synthdata$Date[plot.period],dataprep.out.orig$Y0plot %*% synth.out.orig$solution.w,lwd=5,lty=2)
abline(v = synthdata$Date[synthdata$UnitNumeric==1 
                          & synthdata$Date==as.Date("2022-04-03")])
legend(x="topright", legend=c("Mecklenburg-Vorpommern","Synthetic Mecklenburg-Vorpommern", "leave-one-out estimates"),
       lty = c(1,2,1), 
       cex = 1.5,
       lwd = c(4,4,1) )

#loop going through all units in donor pool
for (predictor.number in c(1:length(var.special.predictors))) {
  if(round(as.numeric(synth.tables.orig$tab.v[,1]), digits = 4)[predictor.number]==0){next} #skips, if predictor has a weight v of 0 in original estimation
  #Creating Synthetic Control
  dataprep.out <- dataprep(
    foo = synthdata,
    special.predictors = var.special.predictors[-predictor.number], #includes all predictors except the omitted predictor
    time.predictors.prior = synthdata$DateNumeric[which(synthdata$AdmUnitId=="0" 
                                                        & synthdata$Date%in%seq(as.Date("2022-03-18"),by="day",length.out=14))], #period over which predictors are averaged
    dependent = var.dependent,
    unit.variable = "UnitNumeric",
    unit.names.variable = "Name",
    time.variable = "DateNumeric",
    treatment.identifier = treated.unit,
    controls.identifier = pool$UnitNumeric[-which(pool$UnitNumeric==treated.unit)], #as controls, all units from the pool, excluding the treated.unit, are chosen   
    time.optimize.ssr = optimization.period, #period over which loss funcion is minimized
    time.plot = plot.period
  )
  synth.out <- synth(data.prep.obj = dataprep.out, method="BFGS")
  synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
  
  
  #Creating Output List
  a<-list(dataprep.out$Y1plot,dataprep.out$Y0plot,synth.out,synth.tables)
  names(a)<-c("Y1","Y0","synth.out","synth.tables")
  robustnesslist2.MV.hospitalizations[length(robustnesslist2.MV.hospitalizations)+1] <- list(a)
  names(robustnesslist2.MV.hospitalizations)[length(robustnesslist2.MV.hospitalizations)] <- paste(as.character(predictor.number), " dropped")
  
  #Lines
  lines(synthdata$Date[plot.period], dataprep.out$Y0plot %*% synth.out$solution.w)
  print(predictor.number) #indicates the progress of the loop
}
dev.off()


