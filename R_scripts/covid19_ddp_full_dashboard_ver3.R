# Extract data from CalREDIE DDP site. When extracting data go to:
# Data Extracts -> select "Complete UDF Data Exports" -> select "NCOV2019" ->
# select Disease: Novel Coronavirus 2019 (nCoV-2019), Date Type: Episode Date ->
# complete the request by adding a start and end date.

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Step 1- Load the necessary package(s)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readr) # Package needed for reading and manipulating tsv files.

library(readxl) # Package needed for reading and manipulating excel files.

library(dplyr) # Package needed for manipulating data structure.

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Step 2- Set the working directory:
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/fosterg/Desktop/CALREDIEAutomated-master/Data")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Step 3- Importing files to create data frames:
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Import Option 1:
calredie <- read_tsv("C:/Users/fosterg/Desktop/CALREDIEAutomated-master/Data/UDF_Disease_Data.tsv")

labs <- read_tsv("C:/Users/fosterg/Desktop/CALREDIEAutomated-master/Data/DiseaseCountyDisGrpData.tsv")

#Import Option 2: Use if Import Option 1 has issues:
#calredie <- read.csv("/Users/GrahamFoster/Desktop/FakeData/UDF_Disease_Data.tsv", na.strings = c(""))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Step 4- modify and trasnform data frames:
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dashboard_data <- calredie %>% # Data subset for needed variables.
  select(IncidentID, Age, Sex,Race, Ethnicity, City ,Zip, DtReceived, DtEpisode,
         RStatus, NCOVPUISxComorbid_NONE, NCOVPUISxComorbid_UNK, NCOVPUISxComorbid_DIAB,
         NCOVPUISxComorbid_CARD, NCOVPUISxComorbid_HYPE, NCOVPUISxComorbid_ASTH,
         NCOVPUISxComorbid_CPD, NCOVPUISxComorbid_CKD, NCOVPUISxComorbid_CLD,
         NCOVPUISxComorbid_STRK, NCOVPUISxComorbid_NEUR, NCOVPUISxComorbid_CANC,
         NCOVPUISxComorbid_IMMU, NCOVPUISxComorbid_OBES, NCOVPUISxComorbid_CSMK,
         NCOVPUISxComorbid_FSMK, NCOVPUISxComorbid_CVAP, NCOVPUISxComorbid_OTH,
         NCOVPUISxHospitalized, NCOVPUISxDie, NCOVPUIISCNTPHCLR,
         NCOVPUISxContTravImpArea, NCOVPUISxContCase, NCOVPUIEpiLinkKnownCase,
         NCOVPUISxComAcq, NCOVPUISxContCaseContType_HOUS, NCOVPUISxContCaseContType_COMM,
         NCOVPUISxContCaseContType_HLTH, NCOVPUISxHlthCareWorker) %>% # data subset for known cases only.
  filter(RStatus %in% c("Confirmed")) %>%
  rowwise() %>% 
  mutate(RStatus = case_when(
    RStatus == "Confirmed" ~ "Known Case" # Changed Resolution status to "Known Case."
    ),
    NCOVPUIISCNTPHCLR = case_when(
    NCOVPUIISCNTPHCLR == "Y" ~ "Yes", # Changed Case recovered to "Yes/No/Unknown/Under investigation."
    NCOVPUIISCNTPHCLR == "N" ~ "No",
    NCOVPUIISCNTPHCLR == "U" ~ "Unknown",
    TRUE ~ "Under investigation"
    ),
    Age = case_when(
    Age <= 17 ~ "17yrs and younger", # Change Ages to age categories.
    Age >= 18 & Age <= 34 ~ "18yrs to 34 yrs",
    Age >= 35 & Age <= 49 ~ "35yrs to 49 yrs",
    Age >= 50 & Age <= 65 ~ "50yrs to 64 yrs",
    Age >= 65 ~ "65yrs and older",
    TRUE ~ "Under investigation"
    ),
    Sex = case_when(
    Sex == "F" ~ "Female", # Changed Sex to "Female/Male/Trans/Nonbinary/Other Genders/Under investigation."
    Sex == "M" ~ "Male",
    Sex == "MTF" ~ "Other",
    Sex == "FTM" ~ "Other",
    Sex == "OTH" ~ "Other",
    Sex == "UNK" ~ "Under investigation",
    TRUE ~ "Under investigation"
    ),
    NCOVPUISxDie = case_when(
    IncidentID == 8650650 ~ "No", # Change die states for incident id 8650650 because NCOVPUISxDieCOVID is not available in the dataset.
    NCOVPUISxDie == "Y" ~ "Yes", # Changed death to "Yes/No/Unknown/Under investigation."
    NCOVPUISxDie == "N" ~ "No",
    NCOVPUISxDie == "U" ~ "Unknown",
    TRUE ~ "Under investigation"
    ),
    NCOVPUISxComorbid_NONE = case_when(
    NCOVPUISxComorbid_NONE == "NONE" ~ 0, # Changed comorbid to a value of 1 or 0.
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_UNK = case_when(
    NCOVPUISxComorbid_UNK == "UNK" ~ 0,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_DIAB = case_when(
    NCOVPUISxComorbid_DIAB == "DIAB" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_CARD = case_when(
    NCOVPUISxComorbid_CARD == "CARD" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_HYPE = case_when(
    NCOVPUISxComorbid_HYPE == "HYPE" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_CPD = case_when(
    NCOVPUISxComorbid_CPD == "CPD" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_CKD = case_when(
    NCOVPUISxComorbid_CKD == "CKD" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_CLD = case_when(
    NCOVPUISxComorbid_CLD == "CLD" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_IMMU = case_when(
    NCOVPUISxComorbid_IMMU == "IMMU" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_ASTH = case_when(
    NCOVPUISxComorbid_ASTH == "ASTH" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_NEUR = case_when(
    NCOVPUISxComorbid_NEUR == "NEUR" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_CSMK = case_when(
    NCOVPUISxComorbid_CSMK == "CSMK" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_FSMK = case_when(
    NCOVPUISxComorbid_FSMK == "FSMK" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_STRK = case_when(
    NCOVPUISxComorbid_STRK == "STRK" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_CANC = case_when(
    NCOVPUISxComorbid_CANC == "CANC" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_OBES = case_when(
    NCOVPUISxComorbid_OBES == "OBES" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_CVAP = case_when(
    NCOVPUISxComorbid_CVAP == "CVAP" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxComorbid_OTH = case_when(
    NCOVPUISxComorbid_OTH == "OTH" ~ 1,
    TRUE ~ 0
    ),
    NCOVPUISxHospitalized = case_when(
    NCOVPUISxHospitalized == "Y" ~ "Yes", # Changed hospitalization to "Yes/No/Unknown/Under investigation."
    NCOVPUISxHospitalized == "N" ~ "No",
    NCOVPUISxHospitalized == "U" ~ "Unknown",
    TRUE ~ "Under investigation"
    ),
    comorbidities = sum(NCOVPUISxComorbid_NONE + NCOVPUISxComorbid_UNK + # Created a new varaible to sum all comorbid values.
                             NCOVPUISxComorbid_DIAB + NCOVPUISxComorbid_CARD + NCOVPUISxComorbid_HYPE +
                             NCOVPUISxComorbid_CPD + NCOVPUISxComorbid_CKD + NCOVPUISxComorbid_CLD +
                             NCOVPUISxComorbid_IMMU + NCOVPUISxComorbid_ASTH + NCOVPUISxComorbid_NEUR +
                             NCOVPUISxComorbid_CSMK + NCOVPUISxComorbid_FSMK + NCOVPUISxComorbid_OTH +
                             NCOVPUISxComorbid_STRK + NCOVPUISxComorbid_CANC + NCOVPUISxComorbid_OBES +
                             NCOVPUISxComorbid_CVAP
    ),
    comorbidities = case_when(
    comorbidities == 0 ~ "None", # Changed comorbid sum to a category.
    comorbidities == 1 ~ "One",
    comorbidities >= 2 ~ "Two or more",
    TRUE ~ "Under investigation"
    )
  ) %>%
  rename(ID = IncidentID) %>% # Changed IncidentID to ID
  mutate(case_status = case_when(
    NCOVPUISxDie == "Yes" ~ "Died", # Created case status variable.
    NCOVPUIISCNTPHCLR == "Yes" ~ "Recovered",
    NCOVPUISxDie == "No" | NCOVPUISxDie == "Unknown"
    & NCOVPUIISCNTPHCLR == "No" |  NCOVPUIISCNTPHCLR == "Unknown"
    | TRUE ~ "Ongoing case"
    ),
    exposure_aggreg = case_when(
    NCOVPUISxContTravImpArea == "Y" ~ "Travel", # Created exposure_aggreg variable.
    NCOVPUISxContCase == "Y" | NCOVPUIEpiLinkKnownCase == "Y" ~ "Person-to-person",
    NCOVPUISxComAcq == "Y" ~ "Community acquired",
    TRUE ~ "Under investigation"
    ),
    exposure_detail = case_when(
    exposure_aggreg == "Travel" ~ "Travel", # Created exposure_detail variable.
    exposure_aggreg == "Community acquired" ~ "Community acquired",
    NCOVPUISxContCaseContType_HOUS == "HOUS" ~ "Household",
    NCOVPUISxContCaseContType_COMM == "COMM" ~ "Close contact",
    NCOVPUISxContCaseContType_HLTH == "HLTH" ~ "Healthcare setting",
    TRUE ~ "Under investigation"
    ),
    healthcare_worker = case_when(
    NCOVPUISxHlthCareWorker == "Y" ~ "Yes", # Changed healthcare worker varaible to have "Yes/No/Unknown/Under investigation"
    NCOVPUISxHlthCareWorker == "N" ~ "No",
    NCOVPUISxHlthCareWorker == "U" ~ "Unknown",
    TRUE ~ "Under investigation"
    ),
    tri_area = case_when(
    Zip %in% c(95060, 95065, 95067, 95066, 95018, 95033, 95006, 95007, 95005, # Changed zip code to tri-area category.
                     95041, 95064, 95061, 95017) ~ "North-county",
    Zip %in% c(95062, 95010, 95073, 95001, 95003) ~ "Mid-county",
    Zip %in% c(95076, 95077) ~ "South-county",
    TRUE  ~ "Under investigation"
    ),
    ethnicity_race = case_when(
    Ethnicity %in% c("Hispanic or Latino") ~ "Hispanic/Latino", # Change race and ethnicity.
    Ethnicity %in% c("Not Hispanic or Latino", "Unknown") & Race %in% c("White") ~ "White/Caucasian (Non-Latino/Hispanic)",
    Ethnicity %in% c("Not Hispanic or Latino", "Unknown") & Race %in% c("Asian",
                                                                        "Black or African American",
                                                                        "Multiple Races", "Other") ~ "Other and Multi-Race (categories were combined due to small counts)",
    Ethnicity %in% c("Not Hispanic or Latino", "Unknown") & Race %in% c("Unknown") ~ "White/Caucasian (Non-Latino/Hispanic)",
    TRUE ~ "Under investigation"
    ),
    cities = case_when(
    City == "Scotts Valley" ~ "Scotts Valley", # Change cities.
    City == "Santa Cruz" ~ "Santa Cruz",
    City == "Capitola" ~ "Capitola",
    City == "Watsonville" ~ "Watsonville",
    City %in% c("Amesti", "Aptos", "Aptos Hills-Larkin Valley", "Ben Lomond", "Bonny Doon",
                "Boulder Creek", "Brookdale", "Corralitos", "Davenport", "Day Valley", "Felton",
                "Interlaken", "La Selva Beach", "Live Oak", "Lompico", "Mount Hermon", "Pajaro Dunes",
                "Paradise Park", "Pasatiempo", "Pleasure Point", "Rio del Mar", "Seacliff", "Soquel",
                "Twin Lakes", "Zayante", "Los Gatos") ~ "Unincorporated",
    TRUE ~ "Under investigation" # This includes one case labaled as Madera.
    )
  ) %>% 
  select(ID, Age, Sex, DtReceived, DtEpisode, RStatus, NCOVPUISxHospitalized, NCOVPUISxDie, # Create final dataset.
         NCOVPUIISCNTPHCLR, comorbidities, case_status, exposure_aggreg, exposure_detail, 
         healthcare_worker, tri_area, ethnicity_race, cities)

labs1 <- calredie %>% 
  select(IncidentID, RStatus, NCOVLabNCoVRslt_1, DtLabResult) %>% # Data subset for needed variables.
  filter(RStatus != "Confirmed",
         DtLabResult != is.na(DtLabResult)) %>% # data subset for known cases only.
  select(IncidentID) %>%
  mutate(result_type = case_when(
    TRUE ~ "COVID-19 case condition"
    )
  )

labs2 <- labs %>%
  select(IncidentID, PersonId, RStatus, Disease, DiseaseGrp, DtReceived, DtEpisode) %>%
  filter(Disease %in% c("Contact to Novel Coronavirus (nCoV-2019)", 
                        "Coronavirus Disease 2019 - Non-positive ELR",
                        "Novel Coronavirus 2019 (nCoV-2019)",
                        "Novel Coronavirus Traveler (nCoV-2019)")
         & RStatus %in% c("Not A Case", "Not Reportable", "Suspect")) %>%
  select(IncidentID) %>%
  mutate(result_type = case_when(
    TRUE ~ "Neg ELR COVID-19 condition"
    )
  )

dashboard_labs <- rbind(labs1, labs2)

two_plus <- dashboard_data %>%
  select(comorbidities, NCOVPUISxHospitalized) %>%
  filter(comorbidities == "Two or more") %>%
  count(comorbidities)

two_plus_hospital <- dashboard_data %>%
  select(comorbidities, NCOVPUISxHospitalized) %>%
  filter(comorbidities == "Two or more" & NCOVPUISxHospitalized == "Yes") %>%
  count(comorbidities)

per_number <- c(two_plus_hospital$n / two_plus$n *100) %>%
  round(digits = 0)

per_number <- paste0(per_number, "%")


time_stamp <- paste("Last refreshed", format(Sys.time() + 1*60*60, "%m/%d/%Y, %H:%M;"),
                 "updated with data entered into CalREDIE as of", format(Sys.Date() - 1, 
                                                                         "%m/%d/%Y,"), "17:00", sep = " ")

chart_summary <- paste("Summary: ''Among Known Cases with two or more chronic disease conditions,",
                   per_number, "required hospitalization while ill with COVID-19.''", sep = " ")

dashboard_static <- data.frame(time_stamp, chart_summary)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Step 5- Pre and xport CSV file with final data:
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
write.table(dashboard_data, "dashboard_data_v2.csv", col.names = TRUE, row.names = FALSE, sep = ",")

write.table(dashboard_labs, "dashboard_lab_v2.csv", col.names = TRUE, row.names = FALSE, sep = ",")

write.table(dashboard_static, "dashboard_static_v2.csv", col.names = TRUE, row.names = FALSE, sep = ",")

