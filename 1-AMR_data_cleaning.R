### Caroline Project | Cleaning script

library(dplyr) ## data manipulation and cleaning
library(arsenal)  ## tables and basic  stats
library(mosaic) ## derive factors based on criteria from multiple columns
library(mgsub) ## change variables (find replace function for many switches at a time) 

# Load data, check variable names, check structure for variable type
choose.files()

## AMR usage data
AMR_data <- read.csv("D:\\Dropbox\\Bandsma.Lab\\Random_Tasks\\Caroline_Tigoi\\5-Data\\antibio_use_data_all sites.csv",
                     na.strings = c("NA", NA, "","."))
head(AMR_data)
colnames(AMR_data)
str(AMR_data)

## daily records
drev_data <- read.csv("D:\\Dropbox\\Bandsma.Lab\\Random_Tasks\\Caroline_Tigoi\\5-Data\\antibio_use_drev_data.csv",
                     na.strings = c("NA", NA, "","."))
head(drev_data)
colnames(drev_data)
str(drev_data)


## anthro
anthro <- read.csv("C:\\Users\\cbour\\OneDrive\\Desktop\\2019_DataCuration\\CHAIN_Data\\CHAIN_Data_v9\\demographics\\anthropometry.csv",
                   na.strings = c("NA", NA, "","."))
head(anthro)
colnames(anthro)
str(anthro)

## demographics
demog <- read.csv("C:\\Users\\cbour\\OneDrive\\Desktop\\2019_DataCuration\\CHAIN_Data\\CHAIN_Data_v9\\demographics\\demographics.csv",
                  na.strings = c("NA", NA, "","."))
head(demog)
colnames(demog)
str(demog)

## outcome
outcome <- read.csv("C:\\Users\\cbour\\OneDrive\\Desktop\\2019_DataCuration\\CHAIN_Data\\CHAIN_Data_v9\\demographics\\outcome.csv",
                    na.strings = c("NA", NA, "","."))
head(outcome)
colnames(outcome)
str(outcome)

## dates
dates <- read.csv("C:\\Users\\cbour\\OneDrive\\Desktop\\2019_DataCuration\\CHAIN_Data\\CHAIN_Data_v9\\demographics\\dates.csv",
                  na.strings = c("NA", NA, "","."))
head(dates)
colnames(dates)
str(dates)



#######################
### First data sets
data <- left_join(demog, anthro, by="record_id")
data <- left_join(data, outcome, by="record_id")
data <- left_join(data, dates, by="record_id")
data <- left_join(data, AMR_data, by="record_id")


### Merge special case
### Add daily review data (convert to "wide" format to merge in)
#data <- left_join(data, drev_data, by="record_id")


### restrict to Kenyan sites
data$site.x # lists out site variable in dataset
data<-subset(data, site.x %in% c("10001","10002","10003"))


### saving full  merged dataset
paste0("AMR_Fulldata_",Sys.Date(),".csv")
write.csv(data, file=paste0("AMR_Fulldata_", Sys.Date(),".csv"))





## Make human readable

### Site
data <- data %>% mutate(site = derivedFactor(
  "Kilifi" = site.x=="10001",
  "Migori" = site.x=="10003",
  "Nairobi" = site.x=="10002",
  .method = "first",
  .default = "NA"))

summary(data$site)


### Admission group
data$group_adm
data$group_adm <- mgsub(data$group_adm, pattern=c(1,2,3,4), replacement=c("SWK","MAM", "NAM", "CP"))
data$group_adm
summary(data$group_adm)
data$group_adm <- as.factor(data$group_adm)
summary(data$group_adm)




####### Participant characteristics table
colnames(data)
dput(colnames(data))
### select variables of interest and store in mini dataset
idata <- data %>% select("record_id", "site", "group_adm","sex_adm","age_group_adm",
                         "weight_adm","height_adm", "haz_adm", "waz_adm", "whz_adm",
                         "oedema_adm", "adm_dead", "readmission", "dead") 


##### set variables as factors
str(idata)
### make list to convert as factor
list_factors<-c("record_id", "site", "group_adm","sex_adm","age_group_adm",
                "oedema_adm", "adm_dead", "readmission", "dead")

### lapply = "to that list apply" transform as factor
idata[, colnames(idata) %in% list_factors]<- lapply(idata[, colnames(idata) %in% list_factors], as.factor)
str(idata)



### Make participant characteristic table
###### Making Participant characteristic table
labels(idata)  <- c(site = 'Site', group_adm = "Nutritional group", sex_adm="Sex",
                    age_group_adm="Age group",
                    weight_adm = "Weight",height_adm="Height", 
                    haz_adm="Lenght for age, z-score", 
                    waz_adm="Weight for age, z-score", 
                    whz_adm="Weight for length, z-score",
                    oedema_adm ="Oedema",
                    adm_dead= "Died during admission",
                    readmission="Readmitted", 
                    dead= "Died during study")

mycontrols  <- tableby.control(test=FALSE, digits.p = 3, total=TRUE, digits.pct = 0, cat.simplify = F,
                               numeric.test="anova", cat.test= "fe",
                               numeric.stats=c("meansd"), digits = 1,
                               cat.stats=c("countpct"),
                               stats.labels=list(N='Count', median='Median', q1q3='Q1,Q3'))


tab1 <- tableby(group_adm ~ site+group_adm+sex_adm+age_group_adm+
                weight_adm+height_adm+haz_adm+waz_adm+whz_adm+
                oedema_adm+adm_dead+readmission+dead,
                data=idata, control=mycontrols)
summary(tab1, text=TRUE) 



### write table
write2pdf(tab1, file=paste0("Table1_Participant_Characteristic_", Sys.Date(),".pdf"))
write2word(tab1, file=paste0("Table1_Participant_Characteristic_", Sys.Date(),".docx"))

