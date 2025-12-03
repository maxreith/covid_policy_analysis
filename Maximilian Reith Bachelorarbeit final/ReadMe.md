
## _Content_
This project contains the following folders and files:

- Folder: Data
- Folder:Figures and Tables
- Folder:R Packages
- Folder:Scripts
- R Project file: Maximilian Reith Bachelorarbeit.Rproj
- Final thesis: Covid Measures in German States under the Hotspot Regulation _ an Empiric Policy Evaluation.pdf
- Readme file

The folders' content is described in the following.
## _R Project file_
Open the R project file to set this folder as the R working directory. Make sure to unpack the zip file into a regular folder first.

## _Scripts_
This folder contains the R scripts used for merging the data ("Data Import") and obtaining the results. All results are reproducable using the respective script. "Data Import" does not need to be run first, as each script can be used independently of the others.
Running the script "Data Import" to reproduce the final, merged dataset "synthdata" takes about 90 minutes, as merging the data set requires substantial computing time. Reproducing the figurs showing placebo tests or robustness checks takes several minutes, as multiple synthetic controls need to be computed.

## _Figures and Tables_
Contains all figures used, as well as all tables in Excel format.


## _R packages_
This folder contains the R packages used. Should the packages not be installed already, please do so by installing them from CRAN or using the files provided.
- Synth (https://cran.r-project.org/web/packages/Synth/index.html)
- writexl (https://cran.r-project.org/web/packages/writexl/index.html)
- readxl (https://cran.r-project.org/web/packages/readxl/index.html)
- readr (https://cran.r-project.org/web/packages/readr/index.html)


## _Data_
The data folder contains the used data. Raw data contains the data obtained from the cited sources. Some of the data has been reformatted slighly to allow for a more convenient import into R, an additional sheet called "edited" has been added there. 
Processed data contains the final data set synthdata, that is obtained by running the R Script "Data Import". The synthdata file contains the following columns:
- UnitNumeric
    - A numeric ranging from 1 to 419, assigned to each regional entitiy.
- AdmUnitId
    - A character containing the regional code used in the data to identify a regional entity (like "01001" for Flensburg, for example).
- StateId
    -  A character containing the regional code used in the data to identify a state (like "01" for Schleswig-Holstein)
- Name
- DateNumeric
    - A numeric equivalent to a date in the dataset. (1 is equivalent to January 1 2022, 2 to January 2, ..., with the last date included, September 2 2022, being 245)
- AnzFallNeu
    - not used for analysis. Refers to the number of cases reported on the day of accessing the database. See RKI Datahub for more.
- AnzFallVortag
    - Main variable used for calculating COVID incidences. Indicates the final number of cases reported for the prior day, which is used for the calculation of the COVID incidence
- AnzFallErkrankung
    - not used for analysis.  Refers exclusively to the date of illness/infection. If the health authorities know when the person concerned became infected, this date is given, see RKI Datahub
- AnzFallMeldung
    - not used for analysis, see RKI Datahub
- KumFall
    - Indicates the cumulative number of cases by reporting date.
- covid incidence
    - seven-days covid incidence
- covid incidence growth rate
    - growth rate of the seven-days covid incidence
- 14 days covid incidence
- 14 days covid incidence growth rate
- Area in Square Kilometers
- Population
- Male Population
- Female population
- Population Density
- Unemployed
- Unemployed Foreigners	
- Unemployed Age 15-20	
- Unemployed Age 15-25	
- Unemployed Age 55-65	
- Long-term Unemployed	
- Unemployment rate in relation to employed labor force	
- Unemployment rate in relation to total labor force	
- Unemployment rate of men in relation to total male labor force
- Unemployment rate of women in relation to total female labor force
- Unemployment rate of foreigners in relation to total foreign labor force
- Unemployment rate of people aged 15-25 in relation to total labor force aged 15-25	
- First dose vaccinations
    - Number of people having received a first COVID-vaccination until that date divided by population. For municipalities, vaccination rates are only given until April 15, due to limited computing capacities
- Second dose vaccinations
    - ...
- Third dose vaccinations	
- Fourth dose vaccinations	
- Fifth dose vaccinations
- Sixth dose vaccinations
- Hospitalizations 00+	
    - Number of hospitalizations (all age groups) within the last seven days 
- Hospitalizations 00-04
    - Number of hospitalizations (of patients between 0 and 4 years old) within the last seven days 
- Hospitalizations 05-14
    - Number of hospitalizations (of patients between 5 and 14 years old) within the last seven days 
- Hospitalizations 15-34
    - Number of hospitalizations (of patients between 15 and 34 years old) within the last seven days 
- Hospitalizations 35-59
    - Number of hospitalizations (of patients between 35 and 59 years old) within the last seven days 
- Hospitalizations 60-79
    - Number of hospitalizations (of patients between 60 and 79 years old) within the last seven days 
- Hospitalizations 80+	
    - Number of hospitalizations (of patients above 80 years old) within the last seven days 
- Hospitalization incidence 00+	
    - hospitalizations within the last seven days per 100,000 inhabitants for all age groups   
- Hospitalization incidence 00-04	
    - hospitalizations within the last seven days per 100,000 inhabitants for the respective age group
- Hospitalization incidence 05-14	
- Hospitalization incidence 15-34	
- Hospitalization incidence 35-59	
- Hospitalization incidence 60-79	
- Hospitalization incidence 80+	
- hospitalization inc. growth rate	
- 14 days hospitalization incidence
    - hospitalizations (all age groups) within the last 14 days per 100,000 inhabitants 
- 14 days hospitalization incidence growth rate	
    - percentage change of the 14 days hospitalization incidence compared to the 14 days hospitalization incidence two weeks ago
- County type
    - Character string identifying municipality types (mostly "LK" - Landkreis or "SK" - Stadtkreis)

