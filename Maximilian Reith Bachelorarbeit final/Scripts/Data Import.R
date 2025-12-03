library(readr)
library(readxl)
library(writexl)

#Data import####


#RKI covid data
RKI_History<- read_csv("Data/raw data/RKI_History.csv", 
                       col_types = cols(AdmUnitId = col_character(), 
                                        BundeslandId = col_character()))
View(RKI_History)

#RKI admin units, containing the regional codes used for German states and municipalities
RKI_AdmUnit <- read_csv("Data/raw data/RKI_AdmUnit.csv", 
                        col_types = cols(AdmUnitId = col_character()))
View(RKI_AdmUnit)

#population data
population.data <- read_excel("Data/raw data/Kreisfreie Staedte und Landkreise Flache, Bevoelkerung.xlsx", 
                              sheet = "edited", range = "A7:I480")[-c(1,20,71,75,135,166,207,269,374,384,404,414,432,448),]
population.data <-population.data[-which(is.na(population.data[,3])),]
View(population.data)
population.data[418,1]<-"0"

#vaccination data
vac.data <- read_csv("Data/raw data/Impfungen Laender.csv", 
                     col_types = cols(Impfdatum = col_date(format = "%Y-%m-%d")))
View(vac.data)

vac.data_counties <- read_csv("Data/raw data/Impfungen Landkreise.csv", 
                              col_types = cols(Impfdatum = col_character()))
View(vac.data_counties)

#unemployment data
unempl.data <- read_excel("Data/raw data/Arbeitslose_Kreise.xlsx", 
                          sheet = "edited for data import", col_types = c("text","text", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric"))
View(unempl.data)
colnames(unempl.data)[1]<-"Kreise und kreisfreie Staedte" #no special character in data frame (German Umlaute)

#Hospitalization data
hosp.data <- read_csv("Data/raw data/Hospitalisierungen.csv")
View(hosp.data)
 
#Cleaning data and data matching####
  # RKI_history#### 
  RKI_History[,3] <- as.Date(RKI_History$Datum) #changing the date format of the RKI data
  regionnumbers=RKI_AdmUnit$AdmUnitId #vector contains all region codes used by the RKI
  Time=sum(format(as.Date(RKI_History$Datum, format="%d/%m/%Y"),"%Y")=="2022" & RKI_History$AdmUnitId=="0") #variable equaling the number of dates over which each unit is observed
  
  #Vector that numbers the units in the data frame
  x=vector()
  i=1
  while (i<=417) {
    x[((i-1)*Time+1):(Time*i)]<-i
    i<-i+1
  }
  
  
  i=1
  synthdata<-as.data.frame(x) #creation of new data frame
  while (i<=length(regionnumbers)) { #i runs through all regional entities observed
    placeholder <- RKI_History[RKI_History$AdmUnitId==regionnumbers[i],] #placeholder is a variable that is assigned the data for region the region with number i
    placeholder <- placeholder[order(placeholder$Datum,decreasing=FALSE),] #placeholder is sorted by date
    placeholder <- placeholder[which(format(as.Date(placeholder$Datum, format="%d/%m/%Y"),"%Y")=="2022"),] #only data of 2022 is used
    
    rownumbers = ((i-1)*dim(placeholder)[1]+1):(i*dim(placeholder)[1]) #rownumbers to put data in
    synthdata[rownumbers,2:10] <- placeholder #creating the data frame
    i <- i+1
  }
  View(synthdata)
  
  #The RKI reports COVID cases for Berlin as a state and also for Berlin's different districts. 
  #It is unclear whether the data on the state of Berlin matches the aggregated districts. 
  #Therefore the data of the districts is aggregated here. In the final data set, Berlin will be reported once as a city and once as a state.
  #This is done, because some data sets used only contain data for cities and others only contain data for states.
  Berlinunits <- RKI_AdmUnit[342:353,1] #Berlin admin units
  Dates <- synthdata$Datum[synthdata$AdmUnitId=="0"]
  
  i=1
  olddim <- dim(synthdata)[1]
  while (i<=length(Dates)) { #i goes through all dates
    j=1
    while (j<=dim(Berlinunits)[1]) { #for a given date, j goes through all Berlin districs
      identifier <- which(synthdata$AdmUnitId==as.character(Berlinunits[j,]) & synthdata$Datum==Dates[i]) #finds a Berlin district on a given date
      
      ifelse(is.na(synthdata[olddim+i,-c(1:4)]), #aggregates observations
             synthdata[olddim+i,-c(1:4)] <- synthdata[identifier,-c(1:4)], 
             synthdata[olddim+i,-c(1:4)] <- synthdata[identifier,-c(1:4)] + synthdata[olddim[1]+i,-c(1:4)])
      j <- j+1
    }
    i <- i+1
  }
  
  synthdata[c((olddim+1):(olddim+length(Dates))) , 4] <- Dates #adds dates
  synthdata[c((olddim+1):(olddim+length(Dates))) , 3] <- synthdata[identifier,3] #adds StateId
  synthdata[c((olddim+1):(olddim+length(Dates))) , 2] <- "11000" #adds AdmUnitId
  
  synthdata[which(synthdata[,2]=="11001"),2:10] <- synthdata[c((olddim+1):(olddim+length(Dates))) , -1]
  #removing disaggregated Berlin districts
  a=which(synthdata[,2]=="11002")[1]
  b=which(synthdata[,2]=="11012")[length(Dates)]
  synthdata <- synthdata[-c(a:b,(olddim+1):(olddim+length(Dates))),] 
  
  RKI_AdmUnit[429,1] <- "11000"
  RKI_AdmUnit[429,2] <- "Berlin"
  
  
  #Next, extra columns will be added, for the use of the synth package later on
  synthdata[,6:12] <- synthdata[,4:10]
  synthdata[,13:14] <- NA #these two columns serve as placeholders columns for later on
  synthdata[,4] <- NA
  
  #Creating numeric dates, with January 1 being 1, January 2 being 2, and so on
  i=1
  while (i<=417) {
    synthdata[((i-1)*Time+1):(Time*i),1]<-i
    i<-i+1
  }
  synthdata[,5] <- rep(1:Time,dim(synthdata)[1]/Time) 
  
  #next, the names of states and municipalities are added to the data frame
    #this loop adds the names of the states to the data frame
    i=1
    while (i<=which(synthdata[,2]=="1001")[1]-1) {  
      x <- which( as.character(synthdata[i,3])
                  ==RKI_AdmUnit[,1])
      synthdata[i,4] <- RKI_AdmUnit[x,2]
      i <- i+1
    }
    
    #this loop adds the names of the counties to the data frame
    i=which(synthdata[,2]=="1001")[1]
    while (i<=dim(synthdata)[1]) {  
      x <- which( as.character(synthdata[i,2])
                  ==RKI_AdmUnit[,1])
      synthdata[i,4] <- RKI_AdmUnit[x,2]
      i <- i+1
    }
  
  #renaming the created columns    
  colnames(synthdata)[1:14] <-  c("UnitNumeric","AdmUnitId","StateId","Name","DateNumeric","Date",colnames(RKI_History)[4:9],"Placeholder","Placeholder")
  
  
  #The AdminUnitIds (characters) of the RKI are partly only four digits long. But some Ids (like 1001) are reported with a starting 0 (01001) in other data sets
  #This loop therefore adds 0s to make the AdmUnitIds five digits long. 
  i=1
  upperlimit=which(synthdata[,2]=="9780")[length(which(synthdata[,2]=="9780"))] # upper limit marks the position of the last AdmUnitID with only four digits
  tofix <- c((sum(synthdata[,2]=="0")+1):(which(synthdata[,2]=="10")[1]-1) , (which(synthdata[,2]=="1001")[1]):upperlimit) #tofix marks the position of all 4-digit AdmUnitIDs
  
  while (i<=length(tofix)) {
    synthdata[tofix[i],2] <- gsub("^(.{0})(.*)$","\\10\\2",synthdata[tofix[i],2]) #adds a 0 in front of the 4-digit AdmUnitIds
    i <- i+1
  }
  
  
  # adding Population Data####
  
  columns <-  c( (dim(synthdata)[2]+1) : (dim(synthdata)[2]+5)) #defining the new columns, where the population data will be added
  i=1
  while (i<=dim(synthdata)[1]/Time) # i runs through from 1 to the number of all regional entities in the synthdata dataframe
  {
    identifier=which(as.character(synthdata[ (Time*(i-1)+1) ,2]) 
                     ==population.data[,1]) #identifier links the regional identity identified by i to a row in the population data frame
    synthdata[c( (Time*(i-1)+1) : (Time*i) ), columns] <- population.data[identifier,5:9] #adding the data
    i <- i+1
  }
  colnames(synthdata)[columns] <- c("Area in Square Kilometers", "Population", "Male Population", "Female Population", "Population Density")
  # Unemployment Data####
  #reformatting and dropping unused information
  unempl.data <- unempl.data[1:which(unempl.data$`Kreise und kreisfreie Staedte`=="16077")[1],-which(colnames(unempl.data)%in%c("Arbeitslose schwerbehindert","...2"))]
  
  #matching
  i=1
  while (i<=dim(unempl.data)[1]) {
    identifier <- which(synthdata$AdmUnitId==unempl.data$`Kreise und kreisfreie Staedte`[i]) #which rows in synthdata contain the current county
    synthdata[identifier,20:31] <- unempl.data[i,2:13] #adding unempl.data to the data set
    i <- i+1
  }
  
  colnames(synthdata)[20:31]<-c("Unemployed","Unemployed Foreigners","Unemployed Age 15-20","Unemployed Age 15-25","Unemployed Age 55-65","Long-term Unemployed",
                                "Unemployment rate in relation to employed labor force","Unemployment rate in relation to total labor force",
                                "Unemployment rate of men in relation to total male labor force", "Unemployment rate of women in relation to total female labor force","Unemployment rate of foreigners in relation to total foreign labor force", 
                                "Unemployment rate of people aged 15-25 in relation to total labor force aged 15-25")
  # Vaccination Data####
  
  #matching daily vaccinations of states with synthdata
  j=1
  while (j<=max(vac.data$Impfserie)) { #outer loop, going through the different dose numbers
    i=Time+1
    while (i<=Time*17) {
      synthdata[i,31+j] <- sum( #sums all doses with same dose number, date, and state and adds them in the respective column
        vac.data$Anzahl[vac.data$Impfdatum <= synthdata$Date[i] #same dates
                        & vac.data$BundeslandId_Impfort == synthdata$AdmUnitId[i] #same state
                        & vac.data$Impfserie == j #dose number 
        ]) 
      i<-i+1
    }
    j<-j+1  
  }
  colnames(synthdata)[c(32:37)] <- c("First dose vaccinations","Second dose vaccinations","Third dose vaccinations","Fourth dose vaccinations","Fifth dose vaccinations","Sixth dose vaccinations")
  
  #matching daily vaccinations of municipalities with synthdata
  #Due to the large amount of computing time required, vaccinations rates are calculated until April 15 only. This does not restrict the analysis, where the date of intervention is April 3
  
  synthdata$Date<-as.character(synthdata$Date)
  j=1
  while (j<=max(vac.data_counties$Impfschutz)) { #outer loop, going through the different dose numbers
    Impfschutz<-(vac.data_counties$Impfschutz==j) #marks all rows in vac.data_counties with the dose number equaling j. THis saves computing time, as the inner loop would have to do this with each iteration otherwise 
    
    #this loop goes through the all regional entities and calculates all vaccinations that have been conducted prior to the first date in the data (January 1 2022)
    i=17
    while (i<417) {
      synthdata[(Time*i)+1,31+j] <- sum( #sums all doses with same dose number, date, and state and adds them in the respective column
                                        vac.data_counties$Anzahl[vac.data_counties$Impfdatum<="2022-01-01" #date
                                                                 &vac.data_counties$LandkreisId_Impfort==synthdata$AdmUnitId[(Time*i)+1]
                                                                 &Impfschutz==TRUE]) #dose number
      i<-i+1
    }
    
    #this loop goes through all dates in 2022 included in the synthdata dataframe
    h=17
    while (h<417) { #h skips through all regional entities
      i=2
      while (i<=100) { #i skips through all dates until April 15 (April 15 is equivalent to 100 in DateNumeric).
        synthdata[h*Time+i,31+j] <- sum(
                                        vac.data_counties$Anzahl[vac.data_counties$Impfdatum==synthdata$Date[h*Time+i]
                                                                 &vac.data_counties$LandkreisId_Impfort==synthdata$AdmUnitId[h*Time+i]
                                                                 &Impfschutz==TRUE],
                                        synthdata[h*Time+i-1,31+j])
        i<-i+1
      }
      h<-h+1
      print(h)
    }
    
    j<-j+1  
  }  
  
  
  synthdata$Date<-as.Date(synthdata$Date)
  
  # Hospitalization Data####
  hosp.data$Datum<-as.Date(hosp.data$Datum)
  hosp.data$Bundesland_Id[hosp.data$Bundesland_Id=="00"]<-"0"
  
  x1=c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+") #different age categories
  j=1
  while (j<=7) { #outer loop, going through the age categories
    i=1
    while (i<=Time*17) {
      synthdata[i,37+j] <- hosp.data$`7T_Hospitalisierung_Faelle`[hosp.data$Datum==synthdata$Date[i]
                                                                  &hosp.data$Bundesland_Id==synthdata$AdmUnitId[i]
                                                                  &hosp.data$Altersgruppe==x1[j]]
      i<-i+1
    }
    j<-j+1  
  }
  
  colnames(synthdata)[38:44]<-c("Hospitalizations 00+", "Hospitalizations 00-04", "Hospitalizations 05-14", "Hospitalizations 15-34", "Hospitalizations 35-59", "Hospitalizations 60-79", "Hospitalizations 80+")
#further editing####
  # Hospitalization incidence####
  x1=c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+")
  j=1
  while (j<=7) { #outer loop, going through the age categories
    i=1
    while (i<=Time*17) {
      synthdata[i,44+j] <- hosp.data$`7T_Hospitalisierung_Inzidenz`[hosp.data$Datum==synthdata$Date[i]
                                                                    &hosp.data$Bundesland_Id==synthdata$AdmUnitId[i]
                                                                    &hosp.data$Altersgruppe==x1[j]]
      i<-i+1
    }
    j<-j+1  
  }
  
  colnames(synthdata)[45:51]<-c("Hospitalization incidence 00+", "Hospitalization incidence 00-04", "Hospitalization incidence 05-14", "Hospitalization incidence 15-34", "Hospitalization incidence 35-59", "Hospitalization incidence 60-79", "Hospitalization incidence 80+")
  
  #weekly growth rate of hospitalization incidence
  
  j=0
  while (j<17) { #j <= number of regional entities observed
    i=8
    while (i<=Time) {
      synthdata[Time*j+i,52] <- 100*(synthdata$`Hospitalization incidence 00+`[Time*j+i] -synthdata$`Hospitalization incidence 00+`[Time*j+i-7])/synthdata$`Hospitalization incidence 00+`[Time*j+i-7]
      i <- i+1
    }
    synthdata[c((j*Time+1):(j*Time+7)),13] <- NA #incidence difference for first seven days is NA
    j <- j+1
  }
  colnames(synthdata)[52]<-"hospitalization inc. growth rate"
  # County type####
  #create an extra column, that indicates whether a county is a city county (Stadtkreis - SK) or a rural county (Landkreis - LK)
  loopparameter1=dim(synthdata)[2]+1
  h=which(synthdata$AdmUnitId=="01001")[1]
  while ( h<=dim(synthdata)[1] ) {
    synthdata[h,loopparameter1]<-unlist(strsplit(synthdata$Name[h],split = " "))[1] #Divides into LK, SK and other
    h<-h+1
  }
  colnames(synthdata)[dim(synthdata)[2]] <- "County type"
  
  
  
  
  # Aggregating the counties and cities of Mecklenburg-Vorpommern####
  # Here the counties (Landkreise) and cities of Mecklenburg-Vorpommern that don't border any other state are aggregated, for a respetive in analysis in subsection 4.3.
  #aggregating the counties of Mecklenburg-Vorpommern
  LKnumbers<-as.vector(synthdata[which(synthdata$StateId%in%c("13")
                                       &synthdata$DateNumeric==1
                                       &synthdata$Name%in%c("LK Vorpommern-Rügen","LK Rostock"))
                                 ,1])
  
  #direct takeover
  synthdata[(Time*417+1):(Time*418),5:6]<-synthdata[which(synthdata$UnitNumeric%in%LKnumbers)[1],5:6] #Dates
  synthdata[(Time*417+1):(Time*418),4]<-"LK MV aggregated"
  synthdata[(Time*417+1):(Time*418),1]<-418 #UniNumber
  
  i=1
  while(i<=Time){
    #summing up covid case numbers, vaccination numbers, population and unemployment numbers
    synthdata[Time*417+i,c(7:11,15:18,20:25,32:37)]<-t(colSums(synthdata[which(synthdata$UnitNumeric%in%LKnumbers
                                                                            &synthdata$DateNumeric==i),c(7:11,15:18,20:25,32:37)]))
    
    i<-i+1  
  }
  
  #Calculating the unemployment rate in relation to employed labor force
  synthdata[synthdata$UnitNumeric==418,26] <- (
    synthdata[synthdata$UnitNumeric==418,20]/
      sum(
        synthdata[which(synthdata$UnitNumeric%in%LKnumbers&synthdata$DateNumeric==1),20]/
          synthdata[which(synthdata$UnitNumeric%in%LKnumbers&synthdata$DateNumeric==1),26])
    
  )
  #Calculating the unemployment rate in relation to total labor force
  synthdata[synthdata$UnitNumeric==418,27] <- (
    synthdata[synthdata$UnitNumeric==418,20]/
      sum(
        synthdata[which(synthdata$UnitNumeric%in%LKnumbers&synthdata$DateNumeric==1),20]/
          synthdata[which(synthdata$UnitNumeric%in%LKnumbers&synthdata$DateNumeric==1),27])
    
  )
  # aggregating the cities of Mecklenburg-Vorpommern
  LKnumbers<-synthdata[which(synthdata$StateId%in%c("13")
                             &synthdata$DateNumeric==1
                             &synthdata$`County type`=="SK"),1]
  
  #direct takeover
  synthdata[(Time*418+1):(Time*419),5:6]<-synthdata[which(synthdata$UnitNumeric%in%LKnumbers[1]),5:6] #Dates
  synthdata[(Time*418+1):(Time*419),4]<-"SK MV aggregated"
  synthdata[(Time*418+1):(Time*419),1]<-419 #UnitNumber
  
  i=1
  while(i<=Time){
    #sum
    synthdata[Time*418+i,c(7:11,15:18,20:25,32:37)]<-t(colSums(synthdata[which(synthdata$UnitNumeric%in%LKnumbers
                                                                          &synthdata$DateNumeric==i),c(7:11,15:18,20:25,32:37)]))
    
    i<-i+1  
  }
  
  #Calculating the unemployment rate in relation to employed labor force
  synthdata[synthdata$UnitNumeric==419,26] <- (
    synthdata[synthdata$UnitNumeric==419,20]/
      sum(
        synthdata[which(synthdata$UnitNumeric%in%LKnumbers&synthdata$DateNumeric==1),c(20)]/
          synthdata[which(synthdata$UnitNumeric%in%LKnumbers&synthdata$DateNumeric==1),26])
    
  )
  
  #Calculating the unemployment rate in relation to total labor force
  synthdata[synthdata$UnitNumeric==419,27] <- (
    synthdata[synthdata$UnitNumeric==419,20]/
      sum(
        synthdata[which(synthdata$UnitNumeric%in%LKnumbers&synthdata$DateNumeric==1),c(20)]/
          synthdata[which(synthdata$UnitNumeric%in%LKnumbers&synthdata$DateNumeric==1),27])
    
  )
  #calculating the population density
  synthdata[which(synthdata$UnitNumeric%in%c(418,419)),19]<-synthdata[which(synthdata$UnitNumeric%in%c(418,419)),16]/synthdata[which(synthdata$UnitNumeric%in%c(418,419)),15]
  # Covid incidence####
  #This calculates the seven-days COVID incidence for all regional entities. Note that the final amount of cases reported on a day is given by "AnzFallVortag" reported in the row of the day after.
  j=0
  while (j<dim(synthdata)[1]/Time) { #j <= number of regional entities observed
    i=6
    while (i<=Time) {
      synthdata[Time*j+i,12] <- sum(synthdata$AnzFallVortag[(Time*j+i-5):(Time*j+i+1)])/synthdata$Population[Time*j+i]*100000 #calculating the covid incidence
      i <- i+1
    }
    synthdata[c((j*Time+1):(j*Time+5),((j+1)*Time)),12] <- NA #covid incidence for first six days and last day in the dataframe is NA
    j <- j+1
  }
  colnames(synthdata)[12] <- "covid incidence"
  
  # more editing####
  #dividing the aggregated vaccination numbers by the population to obtain vaccination rates
  synthdata[,32:37] <- synthdata[,32:37]/synthdata$Population
  
  #calculating the growth rate of the seven days covid incidence
  j=0
  while (j<dim(synthdata)[1]/Time) { #j <= number of regional entities observed
    i=14
    while (i<=Time) {
      synthdata[Time*j+i,13] <- 100*(synthdata$`covid incidence`[Time*j+i] -synthdata$`covid incidence`[Time*j+i-7])/synthdata$`covid incidence`[Time*j+i-7]
      i <- i+1
    }
    synthdata[c((j*Time+1):(j*Time+13)),13] <- NA #covid incidence for first six days and last day is NA
    j <- j+1
  }
  colnames(synthdata)[13] <- "incidence growth rate"
  
  
    
    
    #calculating the 14 days covid incidence
    synthdata2<-synthdata #splitting up the data frame to insert columns in the middle
    synthdata<-synthdata2[,1:13] #splitting up the data frame to insert columns in the middle
    
    j=0
    while (j<dim(synthdata)[1]/Time) { #j runs over all regional entities observed
      i=13
      while (i<Time) {
        synthdata[Time*j+i,14] <- sum(synthdata$AnzFallVortag[(Time*j+i-12):(Time*j+i+1)])/synthdata2$Population[Time*j+i]*100000 #calculating the 14 days covid incidence
        i <- i+1
      }
      synthdata[c((j*Time+1):(j*Time+12),((j+1)*Time)),14] <- NA #covid incidence for first thirteen days and last day is NA
      j <- j+1
    }
    colnames(synthdata)[14] <- "14 days covid incidence"
    
    #calculating the growth rate of the 14 days covid incidence
    j=0
    while (j<dim(synthdata)[1]/Time) { #j < number of regional entities observed
      i=27 #first day to use is January 27, as the first observation for the 14 days covid incidence is January 13
      while (i<=Time) {
        synthdata[Time*j+i,15] <- 100*(synthdata$`14 days covid incidence`[Time*j+i] -synthdata$`14 days covid incidence`[Time*j+i-14])/synthdata$`14 days covid incidence`[Time*j+i-14] #100 times incidence minus incidence two weeks ago divided by incidence two weeks ago
        i <- i+1
      }
      j <- j+1
    }
    colnames(synthdata)[15]<-"14 days covid incidence growth rate"
    
    #adding the remaining columns, except for county type
    synthdata[,16:53]<-synthdata2[,15:52]
    
    #calculating the 14 days hospitalization incidence
    j=0
    while (j<17) { #j < number of Germany + federal states, as the hospitalization incidence is available only for on federal and state level
      i=8
      while (i<=Time) {
        synthdata[j*Time+i,54] <- sum(synthdata$`Hospitalizations 00+`[j*Time+i] + synthdata$`Hospitalizations 00+`[j*Time+i-7])/synthdata$Population[[j*Time+i]]*100000 #Hospitalization 00+ contains the numbers of hospitalized covid patients within the last seven days
        i <- i+1
      }
      j <- j+1
    }
    colnames(synthdata)[54] <- "14 days hospitalization incidence"
    
    #growth rate of 14 days hospitalization incidence
    j=0
    while (j<17) { #j < number of Germany + federal states, as the hospitalization incidence is available only for them
      i=22
      while (i<=Time) {
        synthdata[Time*j+i,55] <- 100*(synthdata$`14 days hospitalization incidence`[Time*j+i] -synthdata$`14 days hospitalization incidence`[Time*j+i-14])/synthdata$`14 days hospitalization incidence`[Time*j+i-14] #100 times incidence minus incidence two weeks ago divided by incidence two weeks ago
        i <- i+1
      }
      j <- j+1
    }
    colnames(synthdata)[55] <- "growth rate of 14 days hospitalization incidence"
  
  #reattaching last column, county type
  synthdata[,56] <- synthdata2[,53]
  colnames(synthdata)[56]<-"County type"
  
  #renaming columns
  colnames(synthdata)[55]<-c("14 days hospitalization incidence growth rate")
  
#exporting the data frame####
write_xlsx(synthdata, "Data/processed data/synthdata.xlsx")