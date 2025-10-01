
#title: "MIDAS Financial 2019"
#output: html_document
#date: "July-2025"

# ########################################################################
#------------------------------------------------------------------------------------------------------#
# 1) Packages and downloading datasets
#------------------------------------------------------------------------------------------------------#
##Download packages#-######################################################################

library(tidyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(dplyr)
library(lubridate)
library(gtsummary)
library(stringr)
library(writexl)
library(data.table)
library(ggridges)
library(patchwork)
library(splines)
library(gridExtra)
library(tibble)
library(mgcv)
library(betareg)
library(ggplot2)
library(margins)
library(splines)
library(systemfit)
library(ranger)
library(forcats)
##Download datasets########
getwd()
options(scipen = 999)
# Datasets
midas_price<- read.csv(file= "/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/OneDrive_24/midas_financial_2019.csv")
midas_price$yr_2019_usd_mnf <- gsub("\\$", "", midas_price$yr_2019_usd_mnf)  # Remove dollar signs
midas_price$yr_2019_usd_mnf <- gsub(",", "", midas_price$yr_2019_usd_mnf)  # Remove commas
midas_price$yr_2019_usd_mnf <- as.numeric(midas_price$yr_2019_usd_mnf)
midas_price$PriceperDDD <- midas_price$yr_2019_usd_mnf / midas_price$ddd
midas_price$PriceperDDD<- as.numeric(midas_price$PriceperDDD)
midas_price$yr_2019_standard_units<- gsub(",", "", midas_price$yr_2019_standard_units)  # Remove commas
midas_price$yr_2019_standard_units<-  as.numeric(midas_price$yr_2019_standard_units)
midas_price$PriceperSU <- midas_price$yr_2019_usd_mnf/ midas_price$yr_2019_standard_units
midas_price$PriceperSU<- as.numeric(midas_price$PriceperSU)
midas_price<- midas_price %>%
  filter(route_of_administration %in% c("Oral", "Parenteral"))


##Conversion rates########
library(readr)
ER_ppp_19WB<- read.csv(file= "/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/OneDrive_24/ER_ppp_19WB.csv")
##Country adjustments#######
midas_price$country <- tolower(midas_price$country)
midas_price <- midas_price %>%
  mutate(country = if_else(country == "uk", "united kingdom", country))
midas_price <- midas_price[!midas_price$country %in% c("central america", "west africa"), ]
midas_price <- midas_price[!is.na(midas_price$adila_antimicrobials), ]


#### WHO AND WB INCOME GROUPS: ####
ref_dataWB_WHO_isoc <- data.frame(
  country_iso3_code = c("ARE", "ARG", "AUS", "AUT", "BEL", "BGD", "BGR", "BIH", "BLR", "BRA", 
                        "CAN", "CHE", "CHL", "CHN", "COL", "CZE", "DEU", "DOM", "DZA", "ECU", 
                        "EGY", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HKG", "HRV", "HUN", 
                        "IDN", "IND", "IRL", "ITA", "JOR", "JPN", "KAZ", "KOR", "KWT", "LBN", 
                        "LKA", "LTU", "LUX", "LVA", "MAR", "MEX", "MYS", "NLD", "NOR", "NZL", 
                        "PAK", "PER", "PHL", "POL", "PRI", "PRT", "ROU", "RUS", "SAU", "SGP", 
                        "SRB", "SVK", "SVN", "SWE", "THA", "TUN", "TUR", "TWN", "URY", "USA", 
                        "VEN", "VNM", "ZAF"),
  WHO_region = c("EMR", "AMR", "WPR", "EUR", "EUR", "SEAR", "EUR", "EUR", "EUR", "AMR",
                 "AMR", "EUR", "AMR", "WPR", "AMR", "EUR", "EUR", "AMR", "AFR", "AMR",
                 "EMR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "WPR", "EUR", "EUR",
                 "SEAR", "SEAR", "EUR", "EUR", "EMR", "WPR", "EUR", "WPR", "EMR", "EMR",
                 "SEAR", "EUR", "EUR", "EUR", "EMR", "AMR", "WPR", "EUR", "EUR", "WPR",
                 "EMR", "AMR", "WPR", "EUR", "AMR", "EUR", "EUR", "EUR", "EUR", "WPR", 
                 "EUR", "EUR", "EUR", "EUR", "SEAR", "EMR", "EUR", "WPR", "AMR", "AMR",
                 "AMR", "WPR", "AFR"),
  WB_income = c("HIC", "UMC", "HIC", "HIC", "HIC", "LMC", "UMC", "UMC", "LMC", "UMC",
                "HIC", "HIC", "HIC", "UMC", "UMC", "HIC", "HIC", "UMC", "LMC", "LMC",
                "LMC", "HIC", "HIC", "HIC", "HIC", "HIC", "HIC", "HIC", "HIC", "HIC",
                "LMC", "LMC", "HIC", "HIC", "LMC", "HIC", "UMC", "HIC", "HIC", "UMC",
                "LMC", "HIC", "HIC", "HIC", "LMC", "UMC", "HIC", "HIC", "HIC", "HIC",
                "LMC", "UMC", "LMC", "HIC", "HIC", "HIC", "UMC", "UMC", "HIC", "HIC", 
                "HIC", "UMC", "UMC", "HIC", "HIC", "LMC", "UMC", "UMC", "HIC", "HIC",
                "UMC", "LMC", "UMC")
)

ref_dataWB_WHO_isoc$WB_incomeshort <- ifelse(ref_dataWB_WHO_isoc$WB_income %in% c("HIC"), "HIC", "LMIC")
number_of_countries <- length(unique(na.omit(midas_price$country)))
number_of_countries
######
#------------------------------------------------------------------------------------------------------#
# 2) Editing midas financial:
#------------------------------------------------------------------------------------------------------#
#Editing and creating EML book variable########
View(midas_price)
table(midas_price$country)
midas_price$PriceperSU[midas_price$PriceperSU < 0] <- 0
midas_price$PriceperDDD[midas_price$PriceperDDD < 0] <- 0
midas_price$ddd[midas_price$ddd< 0] <- 0
midas_price$yr_2019_standard_units[midas_price$yr_2019_standard_units< 0] <- 0
#midas_price$EML_book <- ifelse(midas_price$on_eml_list, 1, 0) // BE CAREFUL HERE WITH PRIOR DATASET IQVIA/
#####

#------------------------------------------------------------------------------------------------------#
# (3) CONVERSION RATES FROM OECD (Exchange rates & PPP 2019)
#------------------------------------------------------------------------------------------------------#
# Merging WHO/WB data to conversion data
ppp_2019_iso <- merge(ER_ppp_19WB, ref_dataWB_WHO_isoc, by = "country_iso3_code", all = TRUE)
# Merge conversion rates to Midas financial 2019 database SET PRICES / PPP RATES.#####
midas_price <- merge(x=midas_price,y=ppp_2019_iso, 
                     by="country_iso3_code", all.y = TRUE)
midas_price$PriceperDDD <- (midas_price$PriceperDDD * midas_price$ER_rate)/ midas_price$PPP_rate
midas_price$PriceperSU <- (midas_price$PriceperSU * midas_price$ER_rate)/ midas_price$PPP_rate
midas_price <- midas_price[!is.na(midas_price$adila_antimicrobials), ]
######
#------------------------------------------------------------------------------------------------------#
# (4) Calculating original prices per DDD  (not weighted) and checking  data availability
#------------------------------------------------------------------------------------------------------#
# ########################################################################
##IDs in the dataset per country/antimicrobial/administration route, etc.####
midas_price$CAA <- paste(midas_price$country, midas_price$adila_antimicrobials, midas_price$route_of_administration, sep = "_") 
midas_price$AA <- paste(midas_price$adila_antimicrobials, midas_price$route_of_administration, sep = "_")
midas_price$AAw <- paste(midas_price$adila_antimicrobials, midas_price$route_of_administration, midas_price$WHO_region, sep = "_")
midas_price$CAw <- paste(midas_price$country, midas_price$aware_category, sep = "_")
midas_price$WAw <- paste(midas_price$WHO_region, midas_price$aware_category, sep = "_")
midas_price$WBAw <- paste(midas_price$WB_income, midas_price$aware_category, sep = "_")
midas_price$WBsAw <- paste(midas_price$WB_incomeshort, midas_price$aware_category, sep = "_")
midas_price$CA <- paste(midas_price$country, midas_price$adila_antimicrobials, sep = "_") 
midas_price$CAwR <- paste(midas_price$country, midas_price$aware_category, midas_price$sector, sep = "_")
midas_price$CRAd <- paste(midas_price$country, midas_price$sector, midas_price$adila_antimicrobials, sep = "_")
midas_price$CEAw <- paste(midas_price$country, midas_price$EML_book, midas_price$aware_category, sep = "_")
midas_price$CAw2 <- paste(midas_price$country, midas_price$derived_aware_category, sep = "_")
#remove data with 0 DDDs consumption #####
midas_price <- midas_price %>%
  filter(ddd != 0)

#Calculate below the price per DDD according to CAA or AAw from the IQVIA  MIDAS FINANCIAL dataset (consumption+price) and weighted prices from this dataset: ####
midas_pricexox <- midas_price %>%
  group_by(CAA) %>%
  mutate(
    ddd_factor = ddd / sum(ddd, na.rm = TRUE),  # Calculate ddd divided by the sum of ddd per CAA group
    avg_pricefactored = PriceperDDD * ddd_factor  # Calculate the factor-weighted price
  )
priceIQVIA_external_c <- midas_pricexox %>%
  group_by(CAA) %>%
  summarise(
    Price_iqvia_ex = sum(avg_pricefactored, na.rm = TRUE)  # Properly sum up the weighted prices to calculate the average
  )


midas_pricexox2 <- midas_price %>%
  group_by(AAw) %>%
  mutate(
    ddd_factor = ddd / sum(ddd, na.rm = TRUE),  # Calculate ddd divided by the sum of ddd per CAA group
    avg_pricefactored = PriceperDDD * ddd_factor  # Calculate the factor-weighted price
  )
priceIQVIA_external_r <- midas_pricexox2 %>%
  group_by(AAw) %>%
  summarise(
    Price_iqvia_ex = sum(avg_pricefactored, na.rm = TRUE)  # Properly sum up the weighted prices to calculate the average
  )




#IQVIA MIDAS FINANCIAL {Graph of country and antimicrobials available} ####
midas_price$available <- 1
midas_price$country <- factor(midas_price$country, levels = rev(sort(unique(midas_price$country))))

filtered_dataG59 <- midas_price[midas_price$EML_book == 1, ]
filtered_dataG60 <- midas_price[midas_price$EML_book == 0, ]

alol2<- ggplot(filtered_dataG59, aes(x = adila_antimicrobials, y = country, fill = available)) +
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90),axis.text.y = element_text(face="bold",size=7)) +
  labs(title = "",x = "Antibiotic", y = "Country") +
  theme(legend.position = "none")

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("availablemidasFin_emlbook.tiff", plot = alol2, device = "tiff", width =12, height = 8, dpi = 500, units = "in")

#Table for all antibiotics available per country #####
midas_price_uniqueg <- midas_price %>%
  group_by(adila_antimicrobials, country) %>%
  summarize(available = first(available), .groups = 'drop') 
midas_price_sorted <- midas_price_uniqueg %>%
  arrange(country)
wide_data <- pivot_wider(
  midas_price_sorted,
  names_from = country,
  values_from = available,
  values_fill = list(available = NA)  # Fill missing cases with NA or consider another appropriate value
)
write_xlsx(wide_data, "Filtered_Antibiotics_Availability_by_Country.xlsx")

#Table for antibiotics available according to EML_book and aware category #####
antibiotics_list <- midas_price %>%
  dplyr::select(adila_antimicrobials, EML_book, aware_category) %>%  # Select the necessary columns
  dplyr::distinct()
write_xlsx(antibiotics_list, "Antibiotics_List_with_Categories.xlsx")

######
#------------------------------------------------------------------------------------------------------#
# (5) Calculating weighted average prices per Country/antibiotic/administration route
#------------------------------------------------------------------------------------------------------#
# ###################################################################

#MIDAS FINANCIAL: data editing across prices and WHO regions for missing prices ######
midas_price <- midas_price[midas_price$country_iso3_code != "ALB", ]
midas_price <- midas_price %>% filter(!is.na(ddd))
num_missing <- sum(is.na(midas_price$PriceperDDD))
num_missing

median_price <- midas_price %>%
  group_by(WHO_region) %>%
  summarise(median_PriceperDDD = median(PriceperDDD, na.rm = TRUE))

midas_price <- midas_price %>%
  left_join(median_price, by = "WHO_region")

midas_price$PriceperDDD <- ifelse(is.na(midas_price$PriceperDDD), midas_price$median_PriceperDDD, midas_price$PriceperDDD)
midas_price <- dplyr::select(midas_price, -median_PriceperDDD)
midas_price <- midas_price[!is.na(midas_price$PriceperDDD), ]

num_missing <- sum(is.na(midas_price$PriceperDDD))
num_missing


median_price2 <- midas_price %>%
  group_by(WHO_region) %>%
  summarise(median_PriceperSU = median(PriceperSU, na.rm = TRUE))

midas_price <- midas_price %>%
  left_join(median_price2, by = "WHO_region")

midas_price$PriceperSU <- ifelse(is.na(midas_price$PriceperSU), midas_price$median_PriceperSU, midas_price$PriceperSU)
midas_price <- dplyr::select(midas_price, -median_PriceperSU)
num_missing <- sum(is.na(midas_price$PriceperSU))
num_missing

table(midas_price$country)

# MIDAS FINANCIAL: Total expenditure and WEIGHTED prices. ######
midas_price$total_expenditure <- midas_price$ddd*midas_price$PriceperDDD

#Weighted average price per Country/Antimicrobial/route of administration & TABLE with Antibiotics/Administration route outputs APPENDIX 1: dataset -> weighted_avg_dataMIDAS_prCAA.######
weighted_avg_dataMIDAS_prCAA <- midas_price %>%
  group_by(CAA) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditureCAA= sum(total_expenditure), total_ddd_soldCAA = sum(ddd), aware_category=first(aware_category), na.rm = TRUE)

selected_weighted_avg <- weighted_avg_dataMIDAS_prCAA %>%
  dplyr::select(CAA, PriceperDDD_w, total_expenditureCAA, total_ddd_soldCAA)

midas_priceAA <- midas_price %>%
  left_join(selected_weighted_avg, by = "CAA")

result_tableAA <- midas_priceAA %>%
  group_by(AA) %>%
  summarise(
    weighted_PriceperDDD_w = mean(PriceperDDD_w, na.rm = TRUE),  # Assuming you want the mean; adjust if another summary is intended
    P25_PriceperDDD = quantile(PriceperDDD, 0.25, na.rm = TRUE),
    Median_PriceperDDD = median(PriceperDDD, na.rm = TRUE),
    P75_PriceperDDD = quantile(PriceperDDD, 0.75, na.rm = TRUE),
    .groups = "drop"  # This will drop the grouping after summarising
  )


library(openxlsx)
write.table(result_tableAA, "result_tableAA.txt", sep = "\t", row.names = FALSE)

#Weighted average price per Country/Aware category & TABLE outputs APPENDIX 2 :dataset -> weighted_avg_dataMIDAS_prCAw.######
weighted_avg_dataMIDAS_prCAw<- midas_price %>%
  group_by(CAw) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), aware_category = first(aware_category), country_iso3_code= first(country_iso3_code), WHO_region= first(WHO_region), WB_income=first(WB_income), WB_incomeshort= first(WB_incomeshort), na.rm = TRUE)
#Checking if there is Inf prices
is_inf <- is.infinite(weighted_avg_dataMIDAS_prCAw$PriceperDDD_w)
inf_count <- sum(is.infinite(weighted_avg_dataMIDAS_prCAw$PriceperDDD_w))
print(inf_count)

selected_weighted_avg2 <- weighted_avg_dataMIDAS_prCAw %>%
  dplyr::select(CAw, PriceperDDD_w, total_expenditure, total_ddd_sold)

midas_price2CAw <- midas_price %>%
  dplyr::select(country_iso3_code, adila_antimicrobials,aware_category, PriceperDDD, who_class, EML_book, WHO_region, WB_income, WB_incomeshort, country,route_of_administration, CAw) %>%
  left_join(selected_weighted_avg2, by = "CAw")

result_tableCAw <- midas_price2CAw %>%
  group_by(country_iso3_code, aware_category) %>%
  summarise(
    weighted_PriceperDDD_w = mean(PriceperDDD_w, na.rm = TRUE),  # Assuming you want the mean; adjust if another summary is intended
    P25_PriceperDDD = quantile(PriceperDDD, 0.25, na.rm = TRUE),
    Median_PriceperDDD = median(PriceperDDD, na.rm = TRUE),
    P75_PriceperDDD = quantile(PriceperDDD, 0.75, na.rm = TRUE),
    .groups = "drop"  # This will drop the grouping after summarising
  )

library(openxlsx)
write.table(result_tableCAw, "result_tableCAw.txt", sep = "\t", row.names = FALSE)

#Weighted average price per WHO-region/Aware category & TABLE outputs APPENDIX 3 :dataset -> weighted_avg_dataMIDAS_prCAw.######
weighted_avg_dataMIDAS_prWAw<- midas_price %>%
  group_by(WAw) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), aware_category = first(aware_category), country_iso3_code= first(country_iso3_code), WHO_region= first(WHO_region),na.rm = TRUE)

selected_weighted_avg3 <- weighted_avg_dataMIDAS_prWAw %>%
  dplyr::select(WAw, PriceperDDD_w, total_expenditure, total_ddd_sold)

midas_price3WAw <- midas_price %>%
  dplyr::select(country_iso3_code, adila_antimicrobials,aware_category, PriceperDDD, who_class, EML_book, WHO_region, WB_income, WB_incomeshort, country,route_of_administration, WAw) %>%
  left_join(selected_weighted_avg3, by = "WAw")

result_tableWAw <- midas_price3WAw %>%
  group_by(WAw) %>%
  summarise(
    weighted_PriceperDDD_w = mean(PriceperDDD_w, na.rm = TRUE),  # Assuming you want the mean; adjust if another summary is intended
    P25_PriceperDDD = quantile(PriceperDDD, 0.25, na.rm = TRUE),
    Median_PriceperDDD = median(PriceperDDD, na.rm = TRUE),
    P75_PriceperDDD = quantile(PriceperDDD, 0.75, na.rm = TRUE),
    .groups = "drop"  # This will drop the grouping after summarising
  )

library(openxlsx)
write.table(result_tableWAw, "result_tableWAw.txt", sep = "\t", row.names = FALSE)


#Weighted average price per WB income/Aware category & TABLE outputs APPENDIX 4 :dataset -> weighted_avg_dataMIDAS_prCAw.######
weighted_avg_dataMIDAS_prWBAw<- midas_price %>%
  group_by(WBAw) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), WB_income=first(WB_income), WB_incomeshort= first(WB_incomeshort), aware_category=first(aware_category), na.rm = TRUE)

selected_weighted_avg4 <- weighted_avg_dataMIDAS_prWBAw %>%
  dplyr::select(WBAw, PriceperDDD_w, total_expenditure, total_ddd_sold)

midas_price4WBAw <- midas_price %>%
  dplyr::select(country_iso3_code, adila_antimicrobials,aware_category, PriceperDDD, who_class, EML_book, WHO_region, WB_income, WB_incomeshort, country,route_of_administration, WBAw) %>%
  left_join(selected_weighted_avg4, by = "WBAw")

result_tableWBAw <- midas_price4WBAw%>%
  group_by(WBAw) %>%
  summarise(
    weighted_PriceperDDD_w = mean(PriceperDDD_w, na.rm = TRUE),  # Assuming you want the mean; adjust if another summary is intended
    P25_PriceperDDD = quantile(PriceperDDD, 0.25, na.rm = TRUE),
    Median_PriceperDDD = median(PriceperDDD, na.rm = TRUE),
    P75_PriceperDDD = quantile(PriceperDDD, 0.75, na.rm = TRUE),
    .groups = "drop"  # This will drop the grouping after summarising
  )

library(openxlsx)
write.table(result_tableWBAw, "result_tableWBAw.txt", sep = "\t", row.names = FALSE)



#Weighted average price per WB income short/Aware category & TABLE outputs APPENDIX 4 :dataset -> weighted_avg_dataMIDAS_prCAw. ######
weighted_avg_dataMIDAS_prWBsAw<- midas_price %>%
  group_by(WBsAw) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), na.rm = TRUE)

selected_weighted_avg5 <- weighted_avg_dataMIDAS_prWBsAw %>%
  dplyr::select(WBsAw, PriceperDDD_w, total_expenditure, total_ddd_sold)

midas_price5WBsAw <- midas_price %>%
  dplyr::select(country_iso3_code, adila_antimicrobials,aware_category, PriceperDDD, who_class, EML_book, WHO_region, WB_income, WB_incomeshort, country,route_of_administration, WBsAw) %>%
  left_join(selected_weighted_avg5, by = "WBsAw")

result_tableWBsAw <- midas_price5WBsAw %>%
  group_by(WBsAw) %>%
  summarise(
    weighted_PriceperDDD_w = mean(PriceperDDD_w, na.rm = TRUE),  # Assuming you want the mean; adjust if another summary is intended
    P25_PriceperDDD = quantile(PriceperDDD, 0.25, na.rm = TRUE),
    Median_PriceperDDD = median(PriceperDDD, na.rm = TRUE),
    P75_PriceperDDD = quantile(PriceperDDD, 0.75, na.rm = TRUE),
    .groups = "drop"  # This will drop the grouping after summarising
  )

library(openxlsx)
write.table(result_tableWBsAw, "result_tableWBsAw.txt", sep = "\t", row.names = FALSE)




#Table of available countries with their respective WHO region and WB income group: APPENDIX 5 ######
country_table <- midas_price %>%
  dplyr::select(country_iso3_code, country,  WHO_region, WB_income) %>%
  distinct()
write.csv(country_table, "country_table.csv", row.names = FALSE)

#Table of classification of antibiotics and aware and EML list. APPENDIX 6 ######
antim_awar_eml_table <- midas_price %>%
  dplyr::select(adila_antimicrobials, aware_category, EML_book) %>%
  distinct()
write.csv(antim_awar_eml_table, "antim_awar_eml_table.csv", row.names = FALSE)
#Weighted average price per Country/Antimicrobial  APPENDIX 7: dataset -> weighted_avg_dataMIDAS_prCA.######
weighted_avg_dataMIDAS_prCA <- midas_price %>%
  group_by(CA) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditureCA= sum(total_expenditure), total_ddd_soldCA = sum(ddd), aware_category= first(aware_category), EML_book= first(EML_book), country_iso3_code= first(country_iso3_code), adila_antimicrobials= first(adila_antimicrobials), WB_incomeshort=first(WB_incomeshort), country_iso3_code= first(country_iso3_code), na.rm = TRUE)
write_xlsx(weighted_avg_dataMIDAS_prCA , "weighted_avg_dataMIDAS_prCA.xlsx")
selected_weighted_avg <- weighted_avg_dataMIDAS_prCA %>%
  dplyr::select(CA, PriceperDDD_w, total_expenditureCA, total_ddd_soldCA)

midas_priceCA <- midas_price %>%
  left_join(selected_weighted_avg, by = "CA")

result_tableCA <- midas_priceCA %>%
  group_by(CA) %>%
  summarise(
    weighted_PriceperDDD_w = mean(PriceperDDD_w, na.rm = TRUE),  # Assuming you want the mean; adjust if another summary is intended
    P25_PriceperDDD = quantile(PriceperDDD, 0.25, na.rm = TRUE),
    Median_PriceperDDD = median(PriceperDDD, na.rm = TRUE),
    P75_PriceperDDD = quantile(PriceperDDD, 0.75, na.rm = TRUE),
    .groups = "drop"  # This will drop the grouping after summarising
  )


library(openxlsx)
write.table(result_tableCA, "result_tableCA.txt", sep = "\t", row.names = FALSE)


#Weighted average price per Country/Aware category by RETAIL or HOSPITAL & TABLE outputs APPENDIX 8 :dataset -> weighted_avg_dataMIDAS_prCAwrEt.######
weighted_avg_dataMIDAS_prCAwrEt<- midas_price %>%
  group_by(CAwR) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), aware_category = first(aware_category), country_iso3_code= first(country_iso3_code), WHO_region= first(WHO_region), WB_income=first(WB_income), WB_incomeshort= first(WB_incomeshort), na.rm = TRUE)

selected_weighted_avg2e <- weighted_avg_dataMIDAS_prCAwrEt %>%
  dplyr::select(CAwR, PriceperDDD_w, total_expenditure, total_ddd_sold)

midas_price2CAwrEt <- midas_price %>%
  dplyr::select(country_iso3_code,aware_category, PriceperDDD, who_class, EML_book, WHO_region, WB_income, WB_incomeshort, country,route_of_administration, CAwR, sector) %>%
  left_join(selected_weighted_avg2e, by = "CAwR")

midas_price2CAwrEt<- midas_price2CAwrEt %>% distinct(CAwR, .keep_all = TRUE)
midas_price2CAwrEt <- midas_price2CAwrEt %>%
  filter(!(aware_category %in% c("Unclassified", "Not Recommended")))

ratio_data <- midas_price2CAwrEt %>%
  group_by(aware_category, sector) %>%
  summarise(total_price = median(PriceperDDD_w)) %>%
  ungroup()

# Print the resulting dataframe to check
print(ratio_data)


result_tableCAwR <- midas_price2CAwrEt %>%
  group_by(CAwR) %>%
  summarise(
    weighted_PriceperDDD_w = mean(PriceperDDD_w, na.rm = TRUE),  # Assuming you want the mean; adjust if another summary is intended
    P25_PriceperDDD = quantile(PriceperDDD, 0.25, na.rm = TRUE),
    Median_PriceperDDD = median(PriceperDDD, na.rm = TRUE),
    P75_PriceperDDD = quantile(PriceperDDD, 0.75, na.rm = TRUE),
    .groups = "drop"  # This will drop the grouping after summarising
  )

library(openxlsx)
write.table(result_tableCAwR, "result_tableCAwrET.txt", sep = "\t", row.names = FALSE)
#######
# Weighted average price per Country/Antimicrobial/Sector GRAPH RETAIL/ HOSPITAL differences/ APPENDIX 9. Differences between prices. #########
weighted_avg_dataMIDAS_prCAwrEtCRAd<- midas_price %>%
  group_by(CRAd) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), aware_category = first(aware_category), country_iso3_code= first(country_iso3_code), WHO_region= first(WHO_region), WB_income=first(WB_income), WB_incomeshort= first(WB_incomeshort), na.rm = TRUE)

selected_weighted_avg2eCRAd <- weighted_avg_dataMIDAS_prCAwrEtCRAd %>%
  dplyr::select(CRAd, PriceperDDD_w, total_expenditure, total_ddd_sold)

midas_price2CAwrEtCRAd <- midas_price %>%
  dplyr::select(country_iso3_code,aware_category, PriceperDDD, who_class, EML_book, WHO_region, WB_income, WB_incomeshort, country,route_of_administration, CRAd, sector, adila_antimicrobials) %>%
  left_join(selected_weighted_avg2eCRAd, by = "CRAd")

midas_price2CAwrEtCRAd<- midas_price2CAwrEtCRAd %>% distinct(CRAd, .keep_all = TRUE)
midas_price2CAwrEtCRAd <- midas_price2CAwrEtCRAd %>%
  filter(!(aware_category %in% c("Unclassified", "Not Recommended")))

ratio_data <- midas_price2CAwrEtCRAd %>%
  group_by(adila_antimicrobials, sector) %>%
  summarise(total_price = mean(PriceperDDD_w), aware_category= first(aware_category), EML_book= first(EML_book)) %>%
  ungroup()

# Print the resulting dataframe to check
print(ratio_data)
#Ratio:
filtered_data <- ratio_data %>%
  filter(EML_book == 1, aware_category == "Access")
library(RColorBrewer)
library(ggpubr)  # For more sophisticated color palettes
library(tidyverse)
library(RColorBrewer)
green_palette <- c("lightgreen", "darkgreen")  # You can specify hex codes for more precision
yellow_palette <- c("#fff200", "#ccba00")  # Vibrant yellow and a darker gold-like yellow
red_palette <- c("#ff9999", "#8b0000")  # A softer light red and a deep dark red

# Assuming data is already loaded and filtered
filtered_data <- ratio_data %>%
  filter(EML_book == 1, aware_category == "Access")
# You can also manually define your own colors if needed
colors <- brewer.pal(6, "Set2")  # 'Set2' is a qualitative palette that's visually appealing
# Create the bar graph with adila_antimicrobials on the x-axis and flip the axes
b1akkk1_accessHOsREt<- ggplot(filtered_data, aes(x = adila_antimicrobials, y = total_price, fill = sector)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' to separate bars of different sectors
  coord_flip() +  # Flips the x and y axes
  scale_fill_manual(values = green_palette) +  # Apply the defined color palette
  theme_minimal(base_size = 14) +  # Use a minimal theme with base font size suitable for figures
  theme(
    axis.title.y = element_blank(),  # Since the axes are flipped, remove the y-axis title
    axis.text.y = element_text(color = "black"),  # Ensure text is clearly readable
    panel.grid.major = element_blank(),  # Remove major grid lines for a cleaner look
    panel.grid.minor = element_blank(),  # Remove minor grid lines as well
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 12)  # Adjust text in legend for readability
  ) +
  labs(title = "",
       subtitle = "Filtered by EML Book 'Access' Category",
       y = "Volume-weighted price per DDD",
       x = "Total Price")

ggsave("b1akkk1_accessHOsREt.tiff", plot = b1akkk1_accessHOsREt, device = "tiff", width =9, height = 9, dpi = 500, units = "in")

filtered_data <- ratio_data %>%
  filter(EML_book == 1, aware_category == "Watch")
# You can also manually define your own colors if needed
colors <- brewer.pal(6, "Set2")  # 'Set2' is a qualitative palette that's visually appealing
# Create the bar graph with adila_antimicrobials on the x-axis and flip the axes
b1akkk1_watchHOsREt<- ggplot(filtered_data, aes(x = adila_antimicrobials, y = total_price, fill = sector)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' to separate bars of different sectors
  coord_flip() +  # Flips the x and y axes
  scale_fill_manual(values = yellow_palette) +  # Apply the defined color palette
  theme_minimal(base_size = 14) +  # Use a minimal theme with base font size suitable for figures
  theme(
    axis.title.y = element_blank(),  # Since the axes are flipped, remove the y-axis title
    axis.text.y = element_text(color = "black"),  # Ensure text is clearly readable
    panel.grid.major = element_blank(),  # Remove major grid lines for a cleaner look
    panel.grid.minor = element_blank(),  # Remove minor grid lines as well
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 12)  # Adjust text in legend for readability
  ) +
  labs(title = "",
       subtitle = "Filtered by EML Book 'Watch' Category",
       y = "Volume-weighted price per DDD",
       x = "Total Price")

ggsave("b1akkk1_watchHOsREt.tiff", plot = b1akkk1_watchHOsREt, device = "tiff", width =9, height = 9, dpi = 500, units = "in")

filtered_data <- ratio_data %>%
  filter(EML_book == 1, aware_category == "Reserve")
# You can also manually define your own colors if needed
colors <- brewer.pal(6, "Set2")  # 'Set2' is a qualitative palette that's visually appealing
# Create the bar graph with adila_antimicrobials on the x-axis and flip the axes
b1akkk1_reserveHOsREt<- ggplot(filtered_data, aes(x = adila_antimicrobials, y = total_price, fill = sector)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' to separate bars of different sectors
  coord_flip() +  # Flips the x and y axes
  scale_fill_manual(values = red_palette) +  # Apply the defined color palette
  theme_minimal(base_size = 14) +  # Use a minimal theme with base font size suitable for figures
  theme(
    axis.title.y = element_blank(),  # Since the axes are flipped, remove the y-axis title
    axis.text.y = element_text(color = "black"),  # Ensure text is clearly readable
    panel.grid.major = element_blank(),  # Remove major grid lines for a cleaner look
    panel.grid.minor = element_blank(),  # Remove minor grid lines as well
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 12)  # Adjust text in legend for readability
  ) +
  labs(title = "",
       subtitle = "Filtered by EML Book 'Reserve' Category",
       y = "Volume-weighted price per DDD",
       x = "Total Price")

ggsave("b1akkk1_reserveHOsREt.tiff", plot = b1akkk1_reserveHOsREt, device = "tiff", width =9, height = 9, dpi = 500, units = "in")









######
# Weighted average price per Country/EML_book/AWaRe book & TABLES for Prices and DDDs######
weighted_avg_dataMIDAS_prCAwrEt65<- midas_price %>%
  group_by(CEAw) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), aware_category = first(aware_category), country_iso3_code= first(country_iso3_code), WHO_region= first(WHO_region), WB_income=first(WB_income), WB_incomeshort= first(WB_incomeshort), EML_book= first(EML_book), na.rm = TRUE)

weighted_avg_dataMIDAS_prCAwrEt65 <- weighted_avg_dataMIDAS_prCAwrEt65 %>%
  filter(!(aware_category %in% c("Not Recommended", "Unclassified"))) %>%
  dplyr::select(country_iso3_code, aware_category, EML_book, PriceperDDD_w)

pivot_dataxol <- weighted_avg_dataMIDAS_prCAwrEt65  %>%
  pivot_wider(names_from = c(EML_book, aware_category), values_from = PriceperDDD_w, values_fill = NA)

# Save to Excel
write.xlsx(pivot_dataxol, file = "EMLvsNonEML_pricesAware.xlsx")

weighted_avg_dataMIDAS_prCAwrEt65<- midas_price %>%
  group_by(CEAw) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), aware_category = first(aware_category), country_iso3_code= first(country_iso3_code), WHO_region= first(WHO_region), WB_income=first(WB_income), WB_incomeshort= first(WB_incomeshort), EML_book= first(EML_book), na.rm = TRUE)

weighted_avg_dataMIDAS_prCAwrEt65s <- weighted_avg_dataMIDAS_prCAwrEt65 %>%
  filter(!(aware_category %in% c("Not Recommended", "Unclassified"))) %>%
  dplyr::select(country_iso3_code, aware_category, EML_book, total_ddd_sold)

pivot_dataxol2 <- weighted_avg_dataMIDAS_prCAwrEt65s  %>%
  pivot_wider(names_from = c(EML_book, aware_category), values_from = total_ddd_sold, values_fill = NA)

# Save to Excel
write.xlsx(pivot_dataxol2, file = "EMLvsNonEML_DDDspAware.xlsx")



######

#-----------####
##DESCRIPTION OF THE DATASETS BELOW:###
#-----------####
#midas_priceAA = Country/ antimicrobial / route of administration ID
#midas_price2CAw = Country aware_category ID
#midas_price3WAw = WHO region and aware_category ID
#midas_price4WBAw = WBank income group and aware_category ID
#midas_price5WBsAw = WBank income group short and aware_category ID
##midas_priceCA = Country/ antimicrobial
write_xlsx(midas_priceAA, "midas_priceAA.xlsx")

#Country antimicrobial aware: #######
weighted_avg_dataMIDAS_prCAwrEt65kkk <- midas_price %>%
  group_by(country_iso3_code, adila_antimicrobials) %>%
  summarise(
    PriceperDDD_w = sum(total_expenditure) / sum(ddd),
    total_expenditure = sum(total_expenditure, na.rm = TRUE),
    total_ddd_sold = sum(ddd, na.rm = TRUE),
    adila_antimicrobials=first(adila_antimicrobials),
    aware_category=first(aware_category),
    WHO_region = first(WHO_region),
    WB_income = first(WB_income),
    WB_incomeshort = first(WB_incomeshort),
    EML_book = first(EML_book)
  )
weighted_avg_dataMIDAS_prCAwrEt65kkk <- weighted_avg_dataMIDAS_prCAwrEt65kkk %>%
  filter(country_iso3_code %in% c("MYS", "PHL"))

#######

# --------------------------------------------------------------------------------------------------#
# --------------------------------------------------------------------------------------------------#
######################################################################################################
######################################################################################################
######################################################################################################
#------------------------------------------------------------------------------------------------------#
# (6) ANALYSES
#------------------------------------------------------------------------------------------------------#
######################################################################################################
######################################################################################################
######################################################################################################
#Decomposition mix-abx case########

# Recreate the unique ID and keep molecule name separately
midas_price$CAwAb <- paste(midas_price$country, midas_price$aware_category, midas_price$adila_antimicrobials, sep = "_")

# Group and preserve adila_antimicrobials
weighted_avg_dataMIDAS_prCAwAb <- midas_price %>%
  group_by(CAwAb) %>%
  summarise(
    PriceperDDD_w = sum(total_expenditure, na.rm = TRUE) / sum(ddd, na.rm = TRUE),
    total_expenditure = sum(total_expenditure, na.rm = TRUE),
    total_ddd_sold = sum(ddd, na.rm = TRUE),
    aware_category = first(aware_category),
    adila_antimicrobials = first(adila_antimicrobials),  # <- ADD THIS LINE
    country_iso3_code = first(country_iso3_code),
    WHO_region = first(WHO_region),
    WB_income = first(WB_income),
    WB_incomeshort = first(WB_incomeshort)
  )
# Filter to valid AWaRe classes
df_decomp <- weighted_avg_dataMIDAS_prCAwAb %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve"))

# --
# Level 1: Within AWaRe categories (molecule-level)
# -
df_level1 <- midas_price %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve")) %>%
  group_by(country_iso3_code, aware_category, adila_antimicrobials) %>%
  summarise(
    total_expenditure = sum(total_expenditure, na.rm = TRUE),
    total_ddd = sum(ddd, na.rm = TRUE),
    price_per_ddd = total_expenditure / total_ddd,
    WHO_region = first(WHO_region),
    WB_incomeshort = first(WB_incomeshort),
    .groups = "drop"
  ) %>%
  mutate(
    log_ddd = log(total_ddd),
    molecule = as.factor(adila_antimicrobials)
  )

model_level1 <- ranger(
  price_per_ddd ~ log_ddd + molecule,
  data = df_level1,
  importance = "impurity"
)

imp1 <- model_level1$variable.importance
imp1_df <- data.frame(
  Level = "Within AWaRe category",
  Component = c("Antibiotic mix volume (within AWaRe)", "Antibiotic mix (within AWaRe)"),
  Contribution = as.numeric(imp1[c("log_ddd", "molecule")])
)

# ---
# Level 2: Across AWaRe categories (AWaRe-level)
# ---
df_level2 <- midas_price %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve")) %>%
  group_by(country_iso3_code, aware_category) %>%
  summarise(
    avg_price = sum(total_expenditure, na.rm = TRUE) / sum(ddd, na.rm = TRUE),
    total_ddd = sum(ddd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_total_ddd = log(total_ddd),
    category = as.factor(aware_category)
  )

model_level2 <- ranger(
  avg_price ~ log_total_ddd + category,
  data = df_level2,
  importance = "impurity"
)

imp2 <- model_level2$variable.importance
imp2_df <- data.frame(
  Level = "Across AWaRe categories",
  Component = c("AWaRe volume (across AWaRe)", "AWaRe category (across AWaRe)"),
  Contribution = as.numeric(imp2[c("log_total_ddd", "category")])
)

# ---
# Combine and normalise
# ---
combined_df <- bind_rows(imp1_df, imp2_df) %>%
  group_by(Level) %>%
  mutate(
    Percent = 100 * Contribution / sum(Contribution)
  ) %>%
  ungroup()

# ---
# Plot
# ---
b1eee<-ggplot(combined_df, aes(x = Percent, y = Level, fill = Component)) +
  geom_col(position = "stack") +
  theme_minimal() +
  scale_fill_manual(
    values = c("AWaRe category (across AWaRe)" = "#D55E00",
               "Antibiotic mix (within AWaRe)" = "#F8766D",
               "Antibiotic mix volume (within AWaRe)" = "#00BFC4",
               "AWaRe volume (across AWaRe)" = "#009E73"),
    name = "Driver"
  ) +
  labs(
    title = "Two-level decomposition of price per DDD variation",
    x = "Percent contribution to variance in price per DDD (impurity reduction)",
    y = "Decomposition level"
  ) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "darkred", size = 1) +
  scale_x_continuous(breaks = seq(0, 100, by = 10))


ggsave("Decomposition_analy.tiff", plot = b1eee, device = "tiff", width =10, height = 6, dpi = 500, units = "in")






#####
#--------------------------------------------------------------------------------------#
# Separate Access//Watch//Reserve and see ATB plots per category and their medians [x-axis:  antibiotics]
#--------------------------------------------------------------------------------------#
######
access_box <- filter(midas_priceCA, aware_category == "Access")

watch_box <- filter(midas_priceCA, aware_category == "Watch")

reserve_box <- filter(midas_priceCA, aware_category == "Reserve")

mean_price_a <- mean(access_box$PriceperDDD_w, na.rm = TRUE)
median_price_a <- median(access_box$PriceperDDD_w, na.rm = TRUE)
b1<- ggplot(data = access_box %>%
              filter(!is.na(PriceperDDD_w )),  # Only filter NA values out
            aes(x = adila_antimicrobials, y = PriceperDDD_w , fill = aware_category)) +
  stat_boxplot(geom = "errorbar", width = 0.15, color = "black") +
  geom_boxplot(alpha = 0.6, width = 0.8, outlier.shape = NA) +  # Removing outliers
  #scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 2)) + 
  scale_y_continuous(breaks = seq(0, 60, by = 5)) +
  coord_cartesian(ylim = c(0, 60)) + # Setting y-axis limits and breaks
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", size=8, angle = 90),
        axis.text.y = element_text(face="bold", size=10)) +
  geom_hline(yintercept = median_price_a, color = "red", size = 0.5,linetype = "dashed") + 
  geom_hline(yintercept = mean_price_a, color = "red", size = 0.5) +# Add median line
  labs(x = "Antibiotic name", y = "Volume-weighted price per DDD (Int$)", fill = "Antibiotics") +
  scale_fill_manual(values = c("#7fc97f")) +
  guides(fill = FALSE)

mean_price_aa <- access_box %>%
  filter(EML_book == 1) %>%
  summarize(mean_price = mean(PriceperDDD_w, na.rm = TRUE)) %>%
  pull(mean_price)
median_price_aa <- access_box %>%
  filter(EML_book == 1) %>%
  summarize(median_price = median(PriceperDDD_w, na.rm = TRUE)) %>%
  pull(median_price)
b1a<- ggplot(data = access_box %>%
              filter(EML_book == 1,!is.na(PriceperDDD_w )),  # Only filter NA values out
            aes(x = adila_antimicrobials, y = PriceperDDD_w , fill = aware_category)) +
  stat_boxplot(geom = "errorbar", width = 0.15, color = "black") +
  geom_boxplot(alpha = 0.6, width = 0.8, outlier.shape = NA) +  # Removing outliers
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  coord_cartesian(ylim = c(0, 40)) + # Setting y-axis limits and breaks
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", size=8, angle = 90),
        axis.text.y = element_text(face="bold", size=10)) +
  geom_hline(yintercept = median_price_aa, color = "red", size = 0.5, linetype = "dashed") +
  geom_hline(yintercept = mean_price_aa, color = "red", size = 0.5) +# Add median line
  labs(x = "Antibiotic name", y = "Volume-weighted price per DDD (Int$)", fill = "Antibiotics") +
  scale_fill_manual(values = c("#7fc97f")) +
  guides(fill = FALSE)

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("Access_ATB_DDDavf.tiff", plot = b1, device = "tiff", width =10, height = 6, dpi = 500, units = "in")
ggsave("Access_ATB_DDDavf_EML.tiff", plot = b1a, device = "tiff", width =10, height = 6, dpi = 500, units = "in")


median_price_w <- median(watch_box$PriceperDDD_w, na.rm = TRUE)
mean_price_w <- mean(watch_box$PriceperDDD_w, na.rm = TRUE)

b2<-ggplot(watch_box %>% 
             filter(PriceperDDD_w != "" & !is.na(PriceperDDD_w)), 
           aes(x = adila_antimicrobials, y = PriceperDDD_w, fill = aware_category)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15,
               color = 1) +
  geom_boxplot(alpha = 0.6,
               width = 0.8,
               outlier.shape = NA) + 
  theme_minimal() +
  theme(axis.text.x = element_text(face = "bold", size = 8, angle = 90),
        axis.text.y = element_text(face = "bold", size = 10)) +
  geom_hline(yintercept = median_price_w, color = "red", size = 0.8, linetype = "dashed") +
  geom_hline(yintercept = mean_price_w, color = "red", size = 0.8) + # Add median line
  labs(x = "Antibiotic name", y = "Volume-weighted price per DDD (Int$)", fill = "Antibiotics") +
  #scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 350, by = 25)) +
  coord_cartesian(ylim = c(0, 350)) +# Setting y-axis limits and breaks
  scale_fill_manual(values = c("#FFFF99")) +
  guides(fill = FALSE)

median_price_wa <- watch_box %>%
  filter(EML_book == 1) %>%
  summarize(median_price = median(PriceperDDD_w, na.rm = TRUE)) %>%
  pull(median_price)

mean_price_wa <- watch_box %>%
  filter(EML_book == 1) %>%
  summarize(mean_price = mean(PriceperDDD_w, na.rm = TRUE)) %>%
  pull(mean_price)

b2a<-ggplot(watch_box %>% 
             filter(EML_book == 1,PriceperDDD_w != "" & !is.na(PriceperDDD_w)), 
           aes(x = adila_antimicrobials, y = PriceperDDD_w, fill = aware_category)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15,
               color = 1) +
  geom_boxplot(alpha = 0.6,
               width = 0.8,
               outlier.shape = NA) + 
  theme_minimal() +
  theme(axis.text.x = element_text(face = "bold", size = 8, angle = 90),
        axis.text.y = element_text(face = "bold", size = 10)) +
  geom_hline(yintercept = median_price_wa, color = "red", size = 0.8, linetype = "dashed") +
  geom_hline(yintercept = mean_price_wa, color = "red", size = 0.8 ) +# Add median line
  labs(x = "Antibiotic name", y = "Volume-weighted price per DDD (Int$)", fill = "Antibiotics") +
  #scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 200, by = 20)) +
  coord_cartesian(ylim = c(0, 200)) +# Setting y-axis limits and breaks
  scale_fill_manual(values = c("#FFFF99")) +
  guides(fill = FALSE)

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("Watch_ATB_DDDavf.tiff", plot = b2, device = "tiff", width =10, height = 6, dpi = 500, units = "in")
ggsave("Watch_ATB_DDDavf_EML.tiff", plot = b2a, device = "tiff", width =10, height = 6, dpi = 500, units = "in")


median_price_r <- median(reserve_box$PriceperDDD_w, na.rm = TRUE)
mean_price_r <- mean(reserve_box$PriceperDDD_w, na.rm = TRUE)

b3<-ggplot(reserve_box %>% 
             filter(PriceperDDD_w != "" & !is.na(PriceperDDD_w)), aes(x = adila_antimicrobials, y = PriceperDDD_w,fill = aware_category)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15,
               color = 1) +
  geom_boxplot(alpha = 0.6,
               width = 0.8,
               outlier.shape = NA) + 
  #scale_y_continuous(limits = c(0, 222), breaks = seq(0, 220, by = 20)) +  # Setting y-axis limits and breaks
  scale_y_continuous(breaks = seq(0, 4000, by = 500)) +
  coord_cartesian(ylim = c(0, 4000)) +# Setting y-axis limits and breaks
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", size=8, angle=90),axis.text.y = element_text(face="bold",size=10)) +
  geom_hline(yintercept = median_price_r, color = "red", size = 0.5, linetype = "dashed") + 
  geom_hline(yintercept = mean_price_r, color = "red", size = 0.5) + # Add median line
  labs(x = "Antibiotic name", y = "Volume-weighted price per DDD (Int$)", fill = "Antibiotics") +
  scale_fill_manual(values = c("#FF9999")) +
  guides(fill = FALSE)

median_price_ra <- reserve_box %>%
  filter(EML_book == 1) %>%
  summarize(median_price = median(PriceperDDD_w, na.rm = TRUE)) %>%
  pull(median_price)

mean_price_ra <- reserve_box %>%
  filter(EML_book == 1) %>%
  summarize(mean_price = mean(PriceperDDD_w, na.rm = TRUE)) %>%
  pull(mean_price)

b3a<-ggplot(reserve_box %>% 
             filter(EML_book == 1,PriceperDDD_w != "" & !is.na(PriceperDDD_w)), aes(x = adila_antimicrobials, y = PriceperDDD_w,fill = aware_category)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15,
               color = 1) +
  geom_boxplot(alpha = 0.6,
               width = 0.8,
               outlier.shape = NA) + 
  #scale_y_continuous(limits = c(0, 222), breaks = seq(0, 220, by = 20)) +  # Setting y-axis limits and breaks
  scale_y_continuous(breaks = seq(0, 1500, by = 100)) +
  coord_cartesian(ylim = c(0, 1500)) +# Setting y-axis limits and breaks
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", size=8, angle=90),axis.text.y = element_text(face="bold",size=10)) +
  geom_hline(yintercept = median_price_ra, color = "red", size = 0.5, linetype = "dashed") + 
  geom_hline(yintercept = mean_price_ra, color = "red", size = 0.5) + # Add median line
  labs(x = "Antibiotic name", y = "Volume-weighted price per DDD (Int$)", fill = "Antibiotics") +
  scale_fill_manual(values = c("#FF9999")) +
  guides(fill = FALSE)

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("Reserve_ATB_DDDavf.tiff", plot = b3, device = "tiff", width =10, height = 6, dpi = 500, units = "in")
ggsave("Reserve_ATB_DDDavf_EML.tiff", plot = b3a, device = "tiff", width =10, height = 6, dpi = 500, units = "in")


#####
#--------------------------------------------------------------------------------------#
# Separate Access//Watch//Reserve  and see plots per category and their medians [x-axis:  countries] Figure 1
#--------------------------------------------------------------------------------------#
######
merged_data3_caware_w_a <- weighted_avg_dataMIDAS_prCAw %>%
  filter(aware_category == "Access")
merged_data3_caware_w_w <- weighted_avg_dataMIDAS_prCAw %>%
  filter(aware_category == "Watch")
merged_data3_caware_w_r <- weighted_avg_dataMIDAS_prCAw%>%
  filter(aware_category == "Reserve")

#View(merged_data3_caware_w_a)
#View(merged_data3_caware_w_w)


price_list_access <- merged_data3_caware_w_a %>%
  dplyr::select(country_iso3_code, PriceperDDD_w) %>%  # Select the necessary columns
  arrange(country_iso3_code) %>%  # Sort the data by country ISO code
  pull(PriceperDDD_w)
print(price_list_access)
prices_character <- formatC(price_list_access, format = "f", digits = 10)
# Create a string that formats it as an R vector
prices_codea <- paste("c(", paste(prices_character, collapse = ", "), ")")
print(prices_codea)

price_list_watch <- merged_data3_caware_w_w %>%
  dplyr::select(country_iso3_code, PriceperDDD_w) %>%  # Select the necessary columns
  arrange(country_iso3_code) %>%  # Sort the data by country ISO code
  pull(PriceperDDD_w)
print(price_list_watch)
prices_character <- formatC(price_list_watch, format = "f", digits = 10)
# Create a string that formats it as an R vector
prices_codew <- paste("c(", paste(prices_character, collapse = ", "), ")")
print(prices_codew)


library(dplyr)
merged_data3_caware_w_a <- merged_data3_caware_w_a %>%
  arrange(desc(PriceperDDD_w)) %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = unique(country_iso3_code)))

median_price_wa <- median(merged_data3_caware_w_a$PriceperDDD_w  , na.rm = TRUE)

a1<-ggplot(merged_data3_caware_w_a, aes(x = reorder(country_iso3_code, PriceperDDD_w ), y = PriceperDDD_w )) +
  geom_hline(aes(yintercept = median_price_wa, color = "Median price (Access)"), linetype = "solid", size = 0.6) +  # Median line
  geom_segment(aes(xend = country_iso3_code, y = 0, yend = PriceperDDD_w ), color = "black") +  # Stems in black
  geom_point(shape = 21, color = "black", fill = "#7fc97f", size = 3, stroke = 0.5) +  # Heads
  coord_flip() +  # Flip the axes
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5)) +
  #scale_color_manual(values = c("Median price" = "#D3D3D3"), labels = "Median price (Access)") +  # Set legend label without title
  labs(x = "Country code (ISO-3)", y = "Average Price (Int$)", title = "A. Access") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust country code labels
    axis.text.x = element_text(angle = 0, hjust = 1),  # Adjust price labels
    plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),  # Bold title
    legend.position = c(0.65, 0.15),  # Adjusted legend position inside the plot area
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray", size = 0.6, fill = NA)  # Add border around the plotting area
  )


# Split the data into above and below the median
data_above_median <- merged_data3_caware_w_a %>%
  filter(PriceperDDD_w >= median_price_wa)

data_below_median <- merged_data3_caware_w_a %>%
  filter(PriceperDDD_w < median_price_wa)

data_above_median$PriceperDDD_w[data_above_median$country_iso3_code == "HRV"] <- 5

# Plot for values equal or above the median
a1_above <- ggplot(data_above_median, aes(x = reorder(country_iso3_code, PriceperDDD_w), y = PriceperDDD_w)) +
  geom_hline(aes(yintercept = median_price_wa, color = "Median price (Access)"), linetype = "solid", size = 0.6) +  # Median line
  geom_segment(aes(xend = country_iso3_code, y = 0, yend = PriceperDDD_w), color = "black") +  # Stems in black
  geom_point(shape = 21, color = "black", fill = "#7fc97f", size = 3, stroke = 0.5) +  # Heads
  coord_flip() +  # Flip the axes
  scale_y_continuous(limits = c(0, 5.2), breaks = seq(0, 5, by = 0.5)) +
  labs(x = "Country code (ISO-3)", y = "Average Price (Int$)", title = "A. Access, above median price") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust country code labels
    axis.text.x = element_text(angle = 0, hjust = 1),  # Adjust price labels
    plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),  # Bold title
    legend.position = c(0.65, 0.15),  # Adjusted legend position inside the plot area
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray", size = 0.6, fill = NA)  # Add border around the plotting area
  )+
  annotate("text", x = "HRV", y = 5.2, label = "+", size = 6, color = "black")

# Plot for values below the median
a1_below <- ggplot(data_below_median, aes(x = reorder(country_iso3_code, PriceperDDD_w), y = PriceperDDD_w)) +
  geom_hline(aes(yintercept = median_price_wa, color = "Median price (Access)"), linetype = "solid", size = 0.6) +  # Median line
  geom_segment(aes(xend = country_iso3_code, y = 0, yend = PriceperDDD_w), color = "black") +  # Stems in black
  geom_point(shape = 21, color = "black", fill = "#7fc97f", size = 3, stroke = 0.5) +  # Heads
  coord_flip() +  # Flip the axes
  scale_y_continuous(limits = c(0, median_price_wa), breaks = seq(0, median_price_wa, by = 0.2)) +
  labs(x = "Country code (ISO-3)", y = "Average Price (Int$)", title = "D. Access, below median price") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust country code labels
    axis.text.x = element_text(angle = 0, hjust = 1),  # Adjust price labels
    plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),  # Bold title
    legend.position = c(0.75, 0.15),  # Adjusted legend position inside the plot area
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray", size = 0.6, fill = NA)  # Add border around the plotting area
  )





merged_data3_caware_w_w <- merged_data3_caware_w_w %>%
  arrange(desc(PriceperDDD_w )) %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = unique(country_iso3_code)))
merged_data3_caware_w_w$PriceperDDD_w[merged_data3_caware_w_w$country_iso3_code == "HRV"] <- 14

median_price_ww <- median(merged_data3_caware_w_w$PriceperDDD_w , na.rm = TRUE)
a2<-ggplot(merged_data3_caware_w_w, aes(x = reorder(country_iso3_code, PriceperDDD_w ), y = PriceperDDD_w )) +
  geom_hline(aes(yintercept = median_price_ww, color = "Median price (Watch)"), linetype = "solid", size = 0.6) +  # Median line
  geom_segment(aes(xend = country_iso3_code, y = 0, yend = PriceperDDD_w ), color = "black") +  # Stems in black
  geom_point(shape = 21, color = "black", fill = "#FFFF99", size = 3, stroke = 0.5) +  # Heads
  coord_flip() +  # Flip the axes
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 1)) +
  #scale_color_manual(values = c("Median price" = "#D3D3D3"), labels = "Median price") +  # Set legend label without title
  labs(x = "Country code (ISO-3)", y = "Volume-weighted price per DDD ($int)", title = "B. Watch") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust country code labels
    axis.text.x = element_text(angle = 0, hjust = 1),  # Adjust price labels
    plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),  # Bold title
    legend.position = c(0.65, 0.15),  # Adjusted legend position inside the plot area
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray", size = 0.6, fill = NA)  # Add border around the plotting area
  )


# Split the data into above and below the median
data_above_median_ww <- merged_data3_caware_w_w %>%
  filter(PriceperDDD_w >= median_price_ww)

data_below_median_ww <- merged_data3_caware_w_w %>%
  filter(PriceperDDD_w < median_price_ww)

# Plot for values equal or above the median
a2_above <- ggplot(data_above_median_ww, aes(x = reorder(country_iso3_code, PriceperDDD_w), y = PriceperDDD_w)) +
  geom_hline(aes(yintercept = median_price_ww, color = "Median price (Watch)"), linetype = "solid", size = 0.6) +  # Median line
  geom_segment(aes(xend = country_iso3_code, y = 0, yend = PriceperDDD_w), color = "black") +  # Stems in black
  geom_point(shape = 21, color = "black", fill = "#FFFF99", size = 3, stroke = 0.5) +  # Heads
  coord_flip() +  # Flip the axes
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 14, by = 1)) +
  labs(x = "Country code (ISO-3)", y = "Volume-weighted price per DDD ($int)", title = "B. Watch, above median price") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust country code labels
    axis.text.x = element_text(angle = 0, hjust = 1),  # Adjust price labels
    plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),  # Bold title
    legend.position = c(0.65, 0.15),  # Adjusted legend position inside the plot area
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray", size = 0.6, fill = NA)  # Add border around the plotting area
  ) +
  annotate("text", x = "HRV", y = 15, label = "+", size = 6, color = "black")

# Plot for values below the median
a2_below <- ggplot(data_below_median_ww, aes(x = reorder(country_iso3_code, PriceperDDD_w), y = PriceperDDD_w)) +
  geom_hline(aes(yintercept = median_price_ww, color = "Median price (Watch)"), linetype = "solid", size = 0.6) +  # Median line
  geom_segment(aes(xend = country_iso3_code, y = 0, yend = PriceperDDD_w), color = "black") +  # Stems in black
  geom_point(shape = 21, color = "black", fill = "#FFFF99", size = 3, stroke = 0.5) +  # Heads
  coord_flip() +  # Flip the axes
  scale_y_continuous(limits = c(0, median_price_ww), breaks = seq(0, median_price_ww, by = 0.3)) +
  labs(x = "Country code (ISO-3)", y = "Volume-weighted price per DDD ($int)", title = "E. Watch, below median price") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust country code labels
    axis.text.x = element_text(angle = 0, hjust = 1),  # Adjust price labels
    plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),  # Bold title
    legend.position = c(0.65, 0.15),  # Adjusted legend position inside the plot area
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray", size = 0.6, fill = NA)  # Add border around the plotting area
  )





##
merged_data3_caware_w_w$PriceperDDD_w 
sau_data <- merged_data3_caware_w_w %>%
  filter(country_iso3_code == "SAU") %>%
  dplyr::select(PriceperDDD_w )

merged_data3_caware_w_r <- merged_data3_caware_w_r %>%
  arrange(desc(PriceperDDD_w )) %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = unique(country_iso3_code)))

merged_data3_caware_w_r <- merged_data3_caware_w_r %>%
  mutate(PriceperDDD_w = if_else(country_iso3_code == "HRV", 403, PriceperDDD_w))
merged_data3_caware_w_r <- merged_data3_caware_w_r %>%
  mutate(PriceperDDD_w = if_else(country_iso3_code == "KWT", 401, PriceperDDD_w))
median_price_wr <- median(merged_data3_caware_w_r$PriceperDDD_w  , na.rm = TRUE)
a3<-ggplot(merged_data3_caware_w_r, aes(x = reorder(country_iso3_code, PriceperDDD_w ), y = PriceperDDD_w )) +
  geom_hline(aes(yintercept = median_price_wr, color = "Median price (Reserve)"), linetype = "solid", size = 0.6) +  # Median line
  geom_segment(aes(xend = country_iso3_code, y = 0, yend = PriceperDDD_w), color = "black") +  # Stems in black
  geom_point(shape = 21, color = "black", fill = "#FF9999", size = 3, stroke = 0.5) +  # Heads
  coord_flip() +  # Flip the axes
  labs(x = "Country code (ISO-3)", y = "Average Price (Int$)", title= "C. Reserve") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust country code labels
    axis.text.x = element_text(angle = 0, hjust = 1),  # Adjust price labels
    plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),  # Bold title
    legend.position = c(0.65, 0.15),  # Adjusted legend position inside the plot area
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray", size = 0.6, fill = NA)  # Add border around the plotting area
  ) +
  scale_y_continuous(limits = c(0, 410), breaks = seq(0, 410, by = 30)) +
  annotate("text", x = "HRV", y = 410, label = "+", size = 6, color = "black")+
  annotate("text", x = "KWT", y = 409, label = "+", size = 6, color = "black")

  #scale_color_manual(values = c("Median price" = "gray"), labels = " ")   # Set legend label without title

# Split the data into above and below the median
data_above_median_wr <- merged_data3_caware_w_r %>%
  filter(PriceperDDD_w >= median_price_wr)

data_below_median_wr <- merged_data3_caware_w_r %>%
  filter(PriceperDDD_w < median_price_wr)

# Plot for values equal or above the median
a3_above <- ggplot(data_above_median_wr, aes(x = reorder(country_iso3_code, PriceperDDD_w), y = PriceperDDD_w)) +
  geom_hline(aes(yintercept = median_price_wr, color = "Median price (Reserve)"), linetype = "solid", size = 0.6) +  # Median line
  geom_segment(aes(xend = country_iso3_code, y = 0, yend = PriceperDDD_w), color = "black") +  # Stems in black
  geom_point(shape = 21, color = "black", fill = "#FF9999", size = 3, stroke = 0.5) +  # Heads
  coord_flip() +  # Flip the axes
  labs(x = "Country code (ISO-3)", y = "Average Price (Int$)", title = "C. Reserve, above median price") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust country code labels
    axis.text.x = element_text(angle = 0, hjust = 1),  # Adjust price labels
    plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),  # Bold title
    legend.position = c(0.65, 0.15),  # Adjusted legend position inside the plot area
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray", size = 0.6, fill = NA)  # Add border around the plotting area
  ) +
  scale_y_continuous(limits = c(0, 419), breaks = seq(0, 400, by = 50)) +
  annotate("text", x = "HRV", y = 415, label = "+", size = 6, color = "black")+
  annotate("text", x = "KWT", y = 419, label = "+", size = 6, color = "black")

# Plot for values below the median
a3_below <- ggplot(data_below_median_wr, aes(x = reorder(country_iso3_code, PriceperDDD_w), y = PriceperDDD_w)) +
  geom_hline(aes(yintercept = median_price_wr, color = "Median price (Reserve)"), linetype = "solid", size = 0.6) +  # Median line
  geom_segment(aes(xend = country_iso3_code, y = 0, yend = PriceperDDD_w), color = "black") +  # Stems in black
  geom_point(shape = 21, color = "black", fill = "#FF9999", size = 3, stroke = 0.5) +  # Heads
  coord_flip() +  # Flip the axes
  labs(x = "Country code (ISO-3)", y = "Average Price (Int$)", title = "F. Reserve, below median price") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust country code labels
    axis.text.x = element_text(angle = 0, hjust = 1),  # Adjust price labels
    plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),  # Bold title
    legend.position = c(0.65, 0.15),  # Adjusted legend position inside the plot area
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray", size = 0.6, fill = NA)  # Add border around the plotting area
  ) +
  scale_y_continuous(limits = c(0, median_price_wr), breaks = seq(0, median_price_wr, by = 10)) 




#mixing and fixing:
a1_above <- a1_above+ labs( y = "")
a1_below <- a1_below+ labs( y = "")
a2_above <- a2_above + labs(x = "", y = "")
a2_below <- a2_below + labs(x = "", y = "Volume-weighted price per DDD (I$)")
a3_above <- a3_above+ labs(x = "", y = "")
a3_below <- a3_below+ labs(x = "", y = "")

library(ggplot2)
library(patchwork)
library(gridExtra)
combined_plot <- gridExtra::grid.arrange(a1, a2, a3, nrow = 1)

combined_plot <- gridExtra::grid.arrange(a1_above, a2_above, a3_above, a1_below, a2_below, a3_below, nrow = 2)

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("Figure1.tiff", plot = combined_plot, device = "tiff", width =14, height = 9, dpi = 500, units = "in")

######
#.
#------------------------------------------------------------------------------------------------#
# Figure of antimicrobials sold (quantity) by country, according to IQVIA and by Aware category
#------------------------------------------------------------------------------------------------#
#####Antimicrobials sold as quantities by country and AWaRe category #####

library(dplyr)
library(ggplot2)

# Step 1: Filter out "Not Recommended" and "Unclassified" categories
weighted_avg_dataMIDAS_prCA2 <- weighted_avg_dataMIDAS_prCA %>%
  filter(!(aware_category %in% c("Not Recommended", "Unclassified")))

# Step 2: Aggregate the data
data_aggregated <- weighted_avg_dataMIDAS_prCA2 %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve")) %>%
  group_by(country_iso3_code, aware_category, EML_book) %>%
  summarise(count = n(), .groups = 'drop')

# Step 3: Order countries by total count
country_order <- data_aggregated %>%
  group_by(country_iso3_code) %>%
  summarise(total_count = sum(count)) %>%
  arrange(total_count) %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = country_iso3_code))

# Step 4: Join to maintain order in the aggregated data
data_aggregated <- data_aggregated %>%
  left_join(country_order, by = "country_iso3_code") %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = country_order$country_iso3_code))

# Step 5: Define colors for EML_book with updated labels
colors <- c("Access not EML" = "#398639", "Access EML" = "#7fc97f", 
            "Watch not EML" =  "#ffe699", "Watch EML" = "#ffff99", 
            "Reserve not EML" = "#c24c4b", "Reserve EML" =  "#ff9999")

# Step 6: Create a new variable for coloring based on aware_category and EML_book with updated labels
data_aggregated <- data_aggregated %>%
  mutate(color_group = case_when(
    aware_category == "Access" & EML_book == 0 ~ "Access not EML",
    aware_category == "Access" & EML_book == 1 ~ "Access EML",
    aware_category == "Watch" & EML_book == 0 ~ "Watch not EML",
    aware_category == "Watch" & EML_book == 1 ~ "Watch EML",
    aware_category == "Reserve" & EML_book == 0 ~ "Reserve not EML",
    aware_category == "Reserve" & EML_book == 1 ~ "Reserve EML"
  ))

# Debug: Check the data with color group
print("Data with Color Group:")
print(head(data_aggregated))

# Step 7: Create the plot with updated legend labels
bar_graph <- ggplot(data_aggregated, aes(x = country_iso3_code, y = count, fill = color_group)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +  # Added border to bars
  scale_fill_manual(values = colors) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 200, by = 10)) +
  labs(
    x = "Country ISO-3 code",
    y = "Count of different types of antibiotics sold (n)",
    fill = "AWaRe categories and EML Book",
    title = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    text = element_text(family = "Arial", color = "#333333"),
    legend.position = c(0.32, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.box.background = element_rect(fill = NA, colour = NA),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.box.margin = margin(0, 0, 0, 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

print(bar_graph)


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("bar_graph_atbsoldCounts.tiff", plot = bar_graph, device = "tiff", width =12, height = 6, dpi = 500, units = "in")
######
###Not recommended/unclassified counts#########
# Step 1: Aggregate the data
data_aggregatedxy <- weighted_avg_dataMIDAS_prCA %>%
  filter(aware_category %in% c("Not Recommended", "Unclassified")) %>%
  group_by(country_iso3_code, aware_category) %>%
  summarise(count = n(), .groups = 'drop')

# Debug: Check the aggregated data
print("Aggregated Data:")
print(head(data_aggregatedxy))

# Step 2: Order countries by total count
country_order <- data_aggregatedxy %>%
  group_by(country_iso3_code) %>%
  summarise(total_count = sum(count), .groups = 'drop') %>%
  arrange(total_count) %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = country_iso3_code))

# Debug: Check the country order
print("Country Order:")
print(country_order)

# Step 3: Join to maintain order in the aggregated data
data_aggregatedxy <- data_aggregatedxy %>%
  left_join(country_order, by = "country_iso3_code") %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = country_order$country_iso3_code))

# Debug: Check the data after join
print("Data After Join:")
print(head(data_aggregatedxy))

# Step 4: Plotting
bar_graph2 <- ggplot(data_aggregatedxy, aes(x = country_iso3_code, y = count, fill = aware_category)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +  # Added border to bars
  scale_fill_manual(values = c("Not Recommended" = "gray", "Unclassified" = "black")) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(
    x = "Country ISO-3 code",
    y = "Count of different types of antimicrobials sold (n)",
    fill = "AWaRe classification",
    title = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    text = element_text(family = "Arial", color = "#333333"), # Improve x-axis label readability
    legend.position = c(0.18, 0.95),  # Position the legend inside the top right corner of the plot area
    legend.justification = c("right", "top"),  # Anchor the legend at the top right
    legend.background = element_rect(fill = NA, colour = NA),  # Ensure legend background is fully transparent
    legend.box.background = element_rect(fill = NA, colour = NA),  # Ensure legend box is also transparent
    legend.key = element_rect(fill = NA, colour = NA),  # Ensure background of each legend key is transparent
    legend.box.margin = margin(0, 0, 0, 0),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),  # X axis title with top margin
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # Y axis title with right margin
    axis.text.y = element_text(size = 12), 
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)  # Y axis labels with adjusted size
  )

print(bar_graph2)

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("bar_graph_atbsoldCountsUNCnotR.tiff", plot = bar_graph2, device = "tiff", width =12, height = 6, dpi = 500, units = "in")
######
##### TOTAL DDD sold per AWaRe category and EML book by country (percentages) #####
###
library(dplyr)
library(ggplot2)

# Step 1: Filter out "Not Recommended" and "Unclassified" categories
weighted_avg_dataMIDAS_prCA2 <- weighted_avg_dataMIDAS_prCA %>%
  filter(!(aware_category %in% c("Not Recommended", "Unclassified")))

# Step 2: Aggregate the data by summing total_ddd_soldCA
data_aggregated2 <- weighted_avg_dataMIDAS_prCA2 %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve")) %>%
  group_by(country_iso3_code, aware_category, EML_book) %>%
  summarise(total_ddd_sold = sum(total_ddd_soldCA, na.rm = TRUE), .groups = 'drop')

# Step 3: Calculate the total DDD sold for each country
total_ddd_by_country <- data_aggregated2 %>%
  group_by(country_iso3_code) %>%
  summarise(total_ddd_soldT = sum(total_ddd_sold, na.rm = TRUE))

# Step 4: Calculate the percentage of total DDD sold for each category within each country
data_aggregated2 <- data_aggregated2 %>%
  left_join(total_ddd_by_country, by = "country_iso3_code") %>%
  mutate(percentage_ddd_sold = (total_ddd_sold / total_ddd_soldT) * 100)

# Step 5: Calculate total ACCESS percentage for each country
access_percentage <- data_aggregated2 %>%
  filter(aware_category == "Access") %>%
  group_by(country_iso3_code) %>%
  summarise(access_percentage = sum(percentage_ddd_sold, na.rm = TRUE)) %>%
  arrange(access_percentage)

# Step 5: Define colors for EML_book with updated labels
colors <- c("Access not EML" = "#398639", "Access EML" = "#7fc97f", 
            "Watch not EML" =  "#ffe699", "Watch EML" = "#ffff99", 
            "Reserve not EML" = "#c24c4b", "Reserve EML" =  "#ff9999")

# Step 7: Create a new variable for coloring based on aware_category and EML_book with updated labels
data_aggregated2 <- data_aggregated2 %>%
  mutate(color_group = case_when(
    aware_category == "Access" & EML_book == 0 ~ "Access not EML",
    aware_category == "Access" & EML_book == 1 ~ "Access EML",
    aware_category == "Watch" & EML_book == 0 ~ "Watch not EML",
    aware_category == "Watch" & EML_book == 1 ~ "Watch EML",
    aware_category == "Reserve" & EML_book == 0 ~ "Reserve not EML",
    aware_category == "Reserve" & EML_book == 1 ~ "Reserve EML"
  ))

# Step 8: Order countries by ACCESS percentage and adjust factor levels
data_aggregated2 <- data_aggregated2 %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = access_percentage$country_iso3_code))

# Step 9: Create the plot with updated legend labels and move legend to the top
bar_graph2 <- ggplot(data_aggregated2, aes(x = country_iso3_code, y = percentage_ddd_sold, fill = color_group)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.25) +  # Added border to bars
  scale_fill_manual(values = colors) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    x = "Country ISO-3 code",
    y = "Percentage of total DDD sold (%)",
    fill = "AWaRe categories and EML Book",
    title = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    text = element_text(family = "Arial", color = "#333333"),
    legend.position = "top",
    legend.justification = "center",
    legend.background = element_rect(fill = NA, colour = NA),
    legend.box.background = element_rect(fill = NA, colour = NA),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)  # Set plot margins to zero
  )

print(bar_graph2)

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("bar_totalDDDAwareEMLbookCountry.tiff", plot = bar_graph2, device = "tiff", width =12, height = 8, dpi = 500, units = "in")


#####
#------------------------------------------------------------------------------------------------#
# MAPS per total expenses by country
#------------------------------------------------------------------------------------------------#
#####
total_expenses_per_country <-  weighted_avg_dataMIDAS_prCA %>%
  group_by(country_iso3_code) %>%
  summarise(total_expenditure = sum(total_expenditureCA, na.rm = TRUE))  # na.rm = TRUE to ignore NAs

if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("rnaturalearth", quietly = TRUE)) install.packages("rnaturalearth")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)

world <- ne_countries(scale = "medium", returnclass = "sf")
# Ensure country_iso3_code is character for accurate merging
total_expenses_per_country <- total_expenses_per_country %>%
  mutate(country_iso3_code = as.character(country_iso3_code))
# Load country geometries

world_data <- world %>%
  left_join(total_expenses_per_country, by = c("adm0_a3" = "country_iso3_code"))

world_data$total_expenditure_pc <- world_data$total_expenditure / world_data$pop_est

# Define a function to generate custom breaks based on the data range
generate_breaks <- function(data, num_breaks = 5) {
  valid_expenditures <- na.omit(as.numeric(data$total_expenditure_pc))  # Convert to numeric and omit NAs
  range_vals <- range(valid_expenditures, na.rm = TRUE)
  seq(from = range_vals[1], to = range_vals[2], length.out = num_breaks)  # Generate sequence
}

# Create custom breaks for the legend [see the data]
summary(world_data$total_expenditure_pc)

quartiles <- quantile(world_data$total_expenditure_pc, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
quartile_colors <- c("#FFE4B5", "#FFA07A", "#CD853F", "#8B4513")

library(scales)
# Creating the map

expenditure_map <- ggplot(world_data) +
  geom_sf(aes(fill = cut(total_expenditure_pc, breaks = c(0, quartiles), include.lowest = TRUE)), color = "black", size = 0.1) +
  scale_fill_manual(
    name = "Quartiles of antibiotic\nexpenditure per capita",
    values = quartile_colors,
    labels = c("1st (I$0.65-I$7.86)", "2nd (I$7.88-I$11.72)", "3rd (I$11.73-I$17.55)", "4th (I$17.56-I$152.92)","Not available"),  # Labels for quartiles
    na.value = "lightgray"
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "Arial"),
    legend.position = c(0.1, 0.5),  # Position legend in the middle left
    legend.direction = "vertical",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(5, 5, 5, 5, "mm")
  )

# Display the map and save
print(expenditure_map)
setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("bar_mapExpenditures.tiff", plot = expenditure_map, device = "tiff", width =12, height = 6, dpi = 500, units = "in")
getwd()

library(patchwork)
# Customize the annotation text
expenditure_map <- expenditure_map + plot_annotation(tag_levels = 'A')
bar_graph <- bar_graph + plot_annotation(tag_levels = 'B')
# Combine the two plots into one column with custom annotations
combined_plotx2 <- expenditure_map / bar_graph + 
  plot_layout(ncol = 1) & 
  plot_annotation(tag_levels = 'A', tag_suffix = '.') &
  theme(plot.tag = element_text(size = 20, face = "bold"))
# Display the combined plot
print(combined_plotx2)
setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("Figure3.tiff", plot = combined_plotx2, device = "tiff", width =13, height = 13, dpi = 1000, units = "in")



######
# ----------------------------------------------------------------------------------------------####
#Figure of Oral & Parenteral quantities and percentages of their total DDD consumption per country.
# ----------------------------------------------------------------------------------------------####
######
midas_price$CAnt <- paste(midas_price$country, midas_price$route_of_administration, sep = "_")

weighted_avg_dataMIDAS_prCAnt<- midas_price %>%
  group_by(CAnt) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), aware_category = first(aware_category), route_of_administration = first(route_of_administration),  country_iso3_code= first(country_iso3_code), na.rm = TRUE)


weighted_avg_dataMIDAS_prCAnt <- weighted_avg_dataMIDAS_prCAnt %>%
  group_by(country_iso3_code) %>%
  mutate(percentage = total_ddd_sold / sum(total_ddd_sold) * 100)




# Calculate the proportion of parenteral antibiotics for each country
parenteral_proportions <- weighted_avg_dataMIDAS_prCAnt %>%
  filter(route_of_administration == "Parenteral") %>%
  arrange(percentage)

# Reorder the countries based on parenteral antibiotic proportion
weighted_avg_dataMIDAS_prCAnt <- weighted_avg_dataMIDAS_prCAnt %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = parenteral_proportions$country_iso3_code))



# Create a stacked bar plot with rotated axes and specified y-axis labels
abhj<- ggplot(weighted_avg_dataMIDAS_prCAnt, aes(x = country_iso3_code, y = percentage, fill = route_of_administration)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Oral" = "#fdcdac", "Parenteral" = "#e41a1c")) + # Refined Lancet style colors
  labs(title = " ",
       x = "Country ISO-3 code",
       y = "Proportion of total antibiotic consumption (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) + # Specify y-axis labels
  theme_minimal() +
  coord_flip() + # Rotate axes
  theme(text = element_text(size = 12),
        axis.text.y = element_text(angle = 360, hjust = 1)) +
  guides(fill = guide_legend(title = "Route of\nAdministration"))

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("parenteral_oralconsumptDDD.tiff", plot = abhj, device = "tiff", width =8, height = 10, dpi = 500, units = "in")
#####

#-------------------------------------------------------------------------------------------------#
# ATBs per country graph, descriptive and by WHO AWARE Category
#-------------------------------------------------------------------------------------------------#
#ACCESS ATB AVAILABiLITY
#####
merged_data3_catb_w <- weighted_avg_dataMIDAS_prCA

filtered_data <- merged_data3_catb_w %>%
  filter(aware_category == "Access")
# Create a binary matrix indicating presence (1) or absence (0) of antibiotics for each country
binary_matrix <- filtered_data %>%
  dplyr::select(country_iso3_code, adila_antimicrobials) %>%
  distinct() %>%
  mutate(presence = 1) %>%
  spread(key = country_iso3_code, value = presence, fill = 0)
# Convert to long format for ggplot
binary_matrix_long <- binary_matrix %>%
  gather(key = "country_iso3_code", value = "presence", -adila_antimicrobials)
# Plot the matrix
matrix_plot <- ggplot(binary_matrix_long, aes(x = country_iso3_code, y = fct_rev(adila_antimicrobials))) +
  geom_tile(aes(fill = factor(presence)), color = "white") +
  scale_fill_manual(values = c("0" = "darkgray", "1" = "#7fc97f"), 
                    name = "Does the country present the specific antibiotic?",
                    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Country ISO Code", y = "Antibiotic", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("matrix_plot_access.tiff", plot = matrix_plot, device = "tiff", width =12, height = 7, dpi = 500, units = "in")
getwd()

#WATCH ATB AVAILABiLITY
filtered_data2 <- merged_data3_catb_w %>%
  filter(aware_category == "Watch")
# Create a binary matrix indicating presence (1) or absence (0) of antibiotics for each country
binary_matrix2 <- filtered_data2 %>%
  dplyr::select(country_iso3_code, adila_antimicrobials) %>%
  distinct() %>%
  mutate(presence = 1) %>%
  spread(key = country_iso3_code, value = presence, fill = 0)
# Convert to long format for ggplot
binary_matrix_long2 <- binary_matrix2 %>%
  gather(key = "country_iso3_code", value = "presence", -adila_antimicrobials)
# Plot the matrix
matrix_plot2 <- ggplot(binary_matrix_long2, aes(x = country_iso3_code, y = fct_rev(adila_antimicrobials))) +
  geom_tile(aes(fill = factor(presence)), color = "white") +
  scale_fill_manual(values = c("0" = "darkgray", "1" = "#FFFF99"), 
                    name = "Does the country present the specific antibiotic?",
                    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Country ISO Code", y = "Antibiotic", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size=7),
    axis.text.y = element_text(size = 5),
    legend.position = "bottom"
  )

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("matrix_plot_watch.tiff", plot = matrix_plot2, device = "tiff", width =12, height = 7, dpi = 500, units = "in")
getwd()



#Reserve ATB AVAILABiLITY
filtered_data3 <- merged_data3_catb_w %>%
  filter(aware_category == "Reserve")
# Create a binary matrix indicating presence (1) or absence (0) of antibiotics for each country
binary_matrix3 <- filtered_data3 %>%
  dplyr::select(country_iso3_code, adila_antimicrobials) %>%
  distinct() %>%
  mutate(presence = 1) %>%
  spread(key = country_iso3_code, value = presence, fill = 0)
# Convert to long format for ggplot
binary_matrix_long3 <- binary_matrix3 %>%
  gather(key = "country_iso3_code", value = "presence", -adila_antimicrobials)
# Plot the matrix
matrix_plot3 <- ggplot(binary_matrix_long3, aes(x = country_iso3_code, y = fct_rev(adila_antimicrobials))) +
  geom_tile(aes(fill = factor(presence)), color = "white") +
  scale_fill_manual(values = c("0" = "darkgray", "1" = "#FF9999"), 
                    name = "Does the country present the specific antibiotic?",
                    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Country ISO Code", y = "Antibiotic", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("matrix_plot_reserve.tiff", plot = matrix_plot3, device = "tiff", width =12, height = 7, dpi = 500, units = "in")
getwd()

#####
#.
#.
#-------------------------------------------------------------------------------------------------#
#.BY WHO REGION
#-------------------------------------------------------------------------------------------------#
#####
# Filter the data for each aware_category and ensure avg_price_caware_w is >= 0

merged_data3_caware_w <- weighted_avg_dataMIDAS_prCAw

# Filter data for each category
data_access <- merged_data3_caware_w %>% filter(aware_category == "Access" & PriceperDDD_w >= 0)
data_watch <- merged_data3_caware_w %>% filter(aware_category == "Watch" & PriceperDDD_w >= 0)
data_reserve <- merged_data3_caware_w %>% filter(aware_category == "Reserve" & PriceperDDD_w >= 0)

# Combine the data into one dataframe with an additional column indicating the category
combined_data <- bind_rows(
  mutate(data_access, category = "Access"),
  mutate(data_watch, category = "Watch"),
  mutate(data_reserve, category = "Reserve")
)

# Ensure there are no missing values in 'category' or 'WHO_region'
combined_data <- combined_data %>% 
  filter(!is.na(category) & !is.na(WHO_region))

# Convert category to a factor to control the order in the plot
combined_data$category <- factor(combined_data$category, levels = c("Access", "Watch", "Reserve"))

# Define lighter color schemes for each WHO region
colors_who <- c(
  "AFR" = "#a6cee3", "AMR" = "#b2df8a", "EMR" = "#cab2d6", 
  "EUR" = "#8da0cb", "SEAR" = "#fb9a99", "WPR" = "#fdbf6f", 
  "Other" = "#ff7f00", "Global" = "#6a3d9a"
)

# Create the plot with specific colors for each WHO region
plot <- ggplot(combined_data, aes(x = WHO_region, y = PriceperDDD_w, fill = WHO_region)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  facet_wrap(~ category, scales = "free_y", labeller = label_both) +
  labs(
    x = "WHO Region",
    y = "Average Price (Int$)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", size = 0.5, fill = NA),
    legend.position = "none"
  ) +
  scale_fill_manual(values = colors_who)

# Create separate plots for each category with custom y-axis limits
library(ggplot2)
library(gridExtra)

# Assuming combined_data and colors_who are already defined



library(ggplot2)
library(cowplot)

# Assuming combined_data and colors_who are already defined

# Create separate plots for each category with specific y-axis limits
plot_access <- ggplot(subset(combined_data, category == "Access"), aes(x = WHO_region, y = PriceperDDD_w, fill = WHO_region)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  labs(
    x = "WHO Region",
    y = "Average Price (Int$)",
    title = "Access"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", size = 0.5, fill = NA),
    legend.position = "none"
  ) +
  scale_fill_manual(values = colors_who) +
  coord_cartesian(ylim = c(0, 3.5))

plot_watch <- ggplot(subset(combined_data, category == "Watch"), aes(x = WHO_region, y = PriceperDDD_w, fill = WHO_region)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  labs(
    x = "WHO Region",
    y = "Average Price (Int$)",
    title = "Watch"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", size = 0.5, fill = NA),
    legend.position = "none"
  ) +
  scale_fill_manual(values = colors_who) +
  coord_cartesian(ylim = c(0, 9))

plot_reserve <- ggplot(subset(combined_data, category == "Reserve"), aes(x = WHO_region, y = PriceperDDD_w, fill = WHO_region)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  labs(
    x = "WHO Region",
    y = "Average Price (Int$)",
    title = "Reserve"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", size = 0.5, fill = NA),
    legend.position = "none"
  ) +
  scale_fill_manual(values = colors_who) +
  coord_cartesian(ylim = c(0, 400))

# Combine plots in a row
combined_plotWHO <- plot_grid(plot_access, plot_watch, plot_reserve, nrow = 1)


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("plot_WHOr_aware.tiff", plot = combined_plotWHO, device = "tiff", width =10, height = 7, dpi = 500, units = "in")
getwd()
######

#.
#-------------------------------------------------------------------------------------------------#
# BY WB INCOME group, high income or LMIC
#-------------------------------------------------------------------------------------------------#
#####
#https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
library(ggthemes)
merged_data3_caware_w <- weighted_avg_dataMIDAS_prCAw


# Filter the data for each aware_category and ensure avg_price_caware_w is >= 0
data_access <- merged_data3_caware_w %>% filter(aware_category == "Access" & PriceperDDD_w >= 0)
data_watch <- merged_data3_caware_w %>% filter(aware_category == "Watch" & PriceperDDD_w >= 0)
data_reserve <- merged_data3_caware_w %>% filter(aware_category == "Reserve" & PriceperDDD_w>= 0)

# Combine the data into one dataframe with an additional column indicating the category
combined_data <- bind_rows(
  mutate(data_access, category = "Access"),
  mutate(data_watch, category = "Watch"),
  mutate(data_reserve, category = "Reserve")
)

# Ensure there are no missing values in 'category' or 'wb_lmics'
combined_data <- combined_data %>% 
  filter(!is.na(category) & !is.na(WB_incomeshort))

# Convert category to a factor to control the order in the plot
combined_data$category <- factor(combined_data$category, levels = c("Access", "Watch", "Reserve"))

# Determine plot type based on number of observations
combined_data <- combined_data %>%
  group_by(category, WB_incomeshort) %>%
  mutate(plot_type = ifelse(n() > 3, "boxplot", "point"))

# Define lighter color schemes for HIC and LMICs
colors_wb <- c(
  "HIC" = "#a6cee3", "LMICs" = "#b2df8a"
)

upper_limits <- combined_data %>%
  group_by(category) %>%
  summarize(upper_limit = max(quantile(PriceperDDD_w, 0.75) + 1.5 * IQR(PriceperDDD_w), na.rm = TRUE))

combined_data <- combined_data %>%
  left_join(upper_limits, by = "category")

# Create the plot with specific colors for each wb_lmics category
plot1 <- ggplot(combined_data, aes(x = WB_incomeshort, y = PriceperDDD_w)) +
  geom_boxplot(data = combined_data %>% filter(plot_type == "boxplot"), aes(fill = WB_incomeshort), width = 0.1, outlier.shape = NA) +
  geom_point(data = combined_data %>% filter(plot_type == "point"), aes(color = WB_incomeshort), size = 2) +
  geom_text(data = combined_data %>% filter(plot_type == "point"), aes(label = country_iso3_code), vjust = -1, size = 3, color = "black") +  # Adjusted label position
  facet_wrap(~ category, scales = "free_y", labeller = label_both) +
  labs(
    x = "World bank income group",
    y = "Average Price (Int$)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", size = 0.5, fill = NA),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) 





library(ggplot2)
library(dplyr)
library(cowplot)

# Create separate plots for each category with specific y-axis limits and WB_incomeshort on the x-axis

library(ggplot2)
library(dplyr)
library(cowplot)

# Define colors for WB income groups
colors_wb <- c("HIC" = "#a6cee3", "LMICs" = "#b2df8a")

# Create separate plots for each category with specific y-axis limits and WB_incomeshort on the x-axis

plot_access <- ggplot(combined_data %>% filter(category == "Access"), aes(x = WB_incomeshort, y = PriceperDDD_w)) +
  geom_boxplot(data = . %>% filter(plot_type == "boxplot"), aes(fill = WB_incomeshort), width = 0.1, outlier.shape = NA) +
  geom_point(data = . %>% filter(plot_type == "point"), aes(color = WB_incomeshort), size = 2) +
  geom_text(data = . %>% filter(plot_type == "point"), aes(label = country_iso3_code), vjust = -1, size = 3, color = "black") +
  labs(
    x = "World Bank Income Group",
    y = "Average Price (Int$)",
    title = "Access"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", size = 0.5, fill = NA),
    legend.position = "none"
  ) +
  scale_fill_manual(values = colors_wb) +
  scale_color_manual(values = colors_wb) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3))

plot_watch <- ggplot(combined_data %>% filter(category == "Watch"), aes(x = WB_incomeshort, y = PriceperDDD_w)) +
  geom_boxplot(data = . %>% filter(plot_type == "boxplot"), aes(fill = WB_incomeshort), width = 0.1, outlier.shape = NA) +
  geom_point(data = . %>% filter(plot_type == "point"), aes(color = WB_incomeshort), size = 2) +
  geom_text(data = . %>% filter(plot_type == "point"), aes(label = country_iso3_code), vjust = -1, size = 3, color = "black") +
  labs(
    x = "World Bank Income Group",
    y = "Average Price (Int$)",
    title = "Watch"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", size = 0.5, fill = NA),
    legend.position = "none"
  ) +
  scale_fill_manual(values = colors_wb) +
  scale_color_manual(values = colors_wb) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7.5))

plot_reserve <- ggplot(combined_data %>% filter(category == "Reserve"), aes(x = WB_incomeshort, y = PriceperDDD_w)) +
  geom_boxplot(data = . %>% filter(plot_type == "boxplot"), aes(fill = WB_incomeshort), width = 0.1, outlier.shape = NA) +
  geom_point(data = . %>% filter(plot_type == "point"), aes(color = WB_incomeshort), size = 2) +
  geom_text(data = . %>% filter(plot_type == "point"), aes(label = country_iso3_code), vjust = -1, size = 3, color = "black") +
  labs(
    x = "World Bank Income Group",
    y = "Average Price (Int$)",
    title = "Reserve"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", size = 0.5, fill = NA),
    legend.position = "none"
  ) +
  scale_fill_manual(values = colors_wb) +
  scale_color_manual(values = colors_wb) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250))

# Combine plots in a row
combined_plot <- plot_grid(plot_access, plot_watch, plot_reserve, nrow = 1)
combined_plot


# Combine plots in a row
combined_plotWB <- plot_grid(plot_access, plot_watch, plot_reserve, nrow = 1)
combined_plotWB



# Print the plot
print(plot1)
setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("plot_aware_wb.tiff", plot = plot1, device = "tiff", width =10, height = 7, dpi = 500, units = "in")
getwd()

library(gridExtra)
# Combine the plots into one figure with one column and two rows
combined_plot <- grid.arrange(combined_plotWHO, combined_plotWB, ncol = 1)
setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("plot_WB&WHO_combo.tiff", plot = combined_plot, device = "tiff", width =11, height = 7, dpi = 500, units = "in")
getwd()
#####
#.
#--------------------------------------------------------------------#
# TABLE OF SUMMARY STATISTICS USING PHARMA14 + IQVIA not financial
#--------------------------------------------------------------------#
# I. Function to compute summary statistics ######
compute_summary_statistics <- function(data, group_vars) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      median = median(PriceperDDD_w, na.rm = TRUE),
      p25 = quantile(PriceperDDD_w, 0.25, na.rm = TRUE),
      p75 = quantile(PriceperDDD_w, 0.75, na.rm = TRUE),
      IQR = IQR(PriceperDDD_w, na.rm = TRUE)
    ) %>%
    ungroup()
}
#####
merged_data3_caware_w <- weighted_avg_dataMIDAS_prCAw
merged_data3_caware_w <- merged_data3_caware_w %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve") & PriceperDDD_w >= 0)
# (1) Summary statistics by aware_category
summary_aware_category <- compute_summary_statistics(merged_data3_caware_w, "aware_category")
# (2) Summary statistics by who_regional_office_code and aware_category
summary_who_regional <- compute_summary_statistics(merged_data3_caware_w, c("aware_category", "WHO_region"))
# (3) Summary statistics by wb_lmics and aware_category
summary_wb_lmic <- compute_summary_statistics(merged_data3_caware_w, c("aware_category", "WB_income"))
# Overall summary statistics
overall_summary <- merged_data3_caware_w %>%
  group_by(aware_category) %>%
  summarise(
    median = median(PriceperDDD_w, na.rm = TRUE),
    p25 = quantile(PriceperDDD_w, 0.25, na.rm = TRUE),
    p75 = quantile(PriceperDDD_w, 0.75, na.rm = TRUE),
    IQR = IQR(PriceperDDD_w, na.rm = TRUE)
  ) %>%
  mutate(group = "Overall")
# Combine all summaries into one table
combined_summary <- bind_rows(
  overall_summary,
  summary_who_regional %>% mutate(group = paste0("WHO Region: ", WHO_region)) %>% dplyr::select(-WHO_region),
  summary_wb_lmic %>% mutate(group = paste0("WB Income: ", WB_income)) %>% dplyr::select(-WB_income)
)
# Pivot to make the table more readable
combined_summary_pivoted <- combined_summary %>%
  pivot_longer(cols = c(median, p25, p75, IQR), names_to = "statistic", values_to = "value") %>%
  unite("stat_category", statistic, aware_category, sep = "_") %>%
  pivot_wider(names_from = stat_category, values_from = value)
# Add overall summary to each group
overall_summary_pivoted <- overall_summary %>%
  pivot_longer(cols = c(median, p25, p75, IQR), names_to = "statistic", values_to = "value") %>%
  unite("stat_category", statistic, aware_category, sep = "_") %>%
  pivot_wider(names_from = stat_category, values_from = value) %>%
  mutate(group = "Total") %>%
  dplyr::select(group, everything())
combined_summary_pivoted <- bind_rows(combined_summary_pivoted, overall_summary_pivoted)

# Reorder the columns
ordered_columns <- c(
  "group",
  "median_Access", "p25_Access", "p75_Access", "IQR_Access",
  "median_Watch", "p25_Watch", "p75_Watch", "IQR_Watch",
  "median_Reserve", "p25_Reserve", "p75_Reserve", "IQR_Reserve"
)
combined_summary_pivoted <- combined_summary_pivoted %>%
  dplyr::select(all_of(ordered_columns))
# Transpose the table
combined_summary_transposed <- as.data.frame(t(combined_summary_pivoted))
colnames(combined_summary_transposed) <- combined_summary_pivoted$group
combined_summary_transposed <- combined_summary_transposed[-1, ]
# Display the transposed combined summary table
print(combined_summary_transposed)
View(combined_summary_transposed)
write_xlsx(combined_summary_transposed, "summarystats1.xlsx")


#Procurement prices: #------------#
merged_data3_caware_w <- weighted_avg_dataMIDAS_prCAw
merged_data3_caware_w <- merged_data3_caware_w %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve") & PriceperDDD_w >= 0)


dataX2 <- data.frame(
  country_iso3_code = c('AUS', 'AUS', 'AUT', 'AUT', 'BEL', 'BEL', 'BGR', 'BGR', 'BRA', 'BRA',
                        'CAN', 'CAN', 'CZE', 'CZE', 'DEU', 'DEU', 'EST', 'EST', 'FIN', 'FIN',
                        'GBR', 'GBR', 'GRC', 'GRC', 'HRV', 'HRV', 'HUN', 'HUN', 'IRL', 'IRL',
                        'LTU', 'LTU', 'LUX', 'LUX', 'LVA', 'LVA', 'MEX', 'MEX', 'POL', 'POL',
                        'ROU', 'ROU', 'RUS', 'RUS', 'SAU', 'SAU', 'SVK', 'SVK', 'SVN', 'SVN',
                        'SWE', 'SWE', 'TUR', 'TUR', 'ZAF', 'ZAF'),
  Category = c('Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch'),
  Value = c(1.074, 1.075, 1.146, 1.132, 1.159, 1.143, 1.063, 1.056, 1.253, 1.252,
            1.080, 1.080, 1.050, 1.050, 1.162, 1.179, 1.131, 1.131, 1.040, 1.040,
            1.067, 1.078, 1.150, 1.150, 1.063, 1.055, 1.059, 1.048, 1.080, 1.080,
            1.089, 1.080, 1.175, 1.157, 1.241, 1.240, 1.161, 1.150, 1.080, 1.084,
            1.136, 1.134, 1.262, 1.254, 1.149, 1.145, 1.080, 1.099, 1.065, 1.060,
            1.027, 1.027, 1.087, 1.069, 1.300, 1.294)
)

# Calculate the average value per country
average_per_country <- dataX2 %>%
  group_by(country_iso3_code) %>%
  summarize(average_value = mean(Value))

merged_data3_caware_w <- merged_data3_caware_w %>%
  left_join(average_per_country, by = "country_iso3_code")
merged_data3_caware_w <- merged_data3_caware_w %>%
  mutate(average_value = if_else(is.na(average_value),
                                 if_else(WB_incomeshort == "HIC", 1.12, 
                                         if_else(WB_incomeshort == "LMIC", 1.21, NA_real_)),
                                 average_value))

merged_data3_caware_w$PriceperDDD_w_proc<- merged_data3_caware_w$PriceperDDD_w /merged_data3_caware_w$average_value



# (1) Summary statistics by aware_category
summary_aware_category <- compute_summary_statistics(merged_data3_caware_w, "aware_category")
# (2) Summary statistics by who_regional_office_code and aware_category
summary_who_regional <- compute_summary_statistics(merged_data3_caware_w, c("aware_category", "WHO_region"))
# (3) Summary statistics by wb_lmics and aware_category
summary_wb_lmic <- compute_summary_statistics(merged_data3_caware_w, c("aware_category", "WB_income"))
# Overall summary statistics
overall_summary <- merged_data3_caware_w %>%
  group_by(aware_category) %>%
  summarise(
    median = median(PriceperDDD_w_proc, na.rm = TRUE),
    p25 = quantile(PriceperDDD_w_proc, 0.25, na.rm = TRUE),
    p75 = quantile(PriceperDDD_w_proc, 0.75, na.rm = TRUE),
    IQR = IQR(PriceperDDD_w_proc, na.rm = TRUE)
  ) %>%
  mutate(group = "Overall")
# Combine all summaries into one table
combined_summary <- bind_rows(
  overall_summary,
  summary_who_regional %>% mutate(group = paste0("WHO Region: ", WHO_region)) %>% dplyr::select(-WHO_region),
  summary_wb_lmic %>% mutate(group = paste0("WB Income: ", WB_income)) %>% dplyr::select(-WB_income)
)
# Pivot to make the table more readable
combined_summary_pivoted <- combined_summary %>%
  pivot_longer(cols = c(median, p25, p75, IQR), names_to = "statistic", values_to = "value") %>%
  unite("stat_category", statistic, aware_category, sep = "_") %>%
  pivot_wider(names_from = stat_category, values_from = value)
# Add overall summary to each group
overall_summary_pivoted <- overall_summary %>%
  pivot_longer(cols = c(median, p25, p75, IQR), names_to = "statistic", values_to = "value") %>%
  unite("stat_category", statistic, aware_category, sep = "_") %>%
  pivot_wider(names_from = stat_category, values_from = value) %>%
  mutate(group = "Total") %>%
  dplyr::select(group, everything())
combined_summary_pivoted <- bind_rows(combined_summary_pivoted, overall_summary_pivoted)

# Reorder the columns
ordered_columns <- c(
  "group",
  "median_Access", "p25_Access", "p75_Access", "IQR_Access",
  "median_Watch", "p25_Watch", "p75_Watch", "IQR_Watch",
  "median_Reserve", "p25_Reserve", "p75_Reserve", "IQR_Reserve"
)
combined_summary_pivoted <- combined_summary_pivoted %>%
  dplyr::select(all_of(ordered_columns))
# Transpose the table
combined_summary_transposed <- as.data.frame(t(combined_summary_pivoted))
colnames(combined_summary_transposed) <- combined_summary_pivoted$group
combined_summary_transposed <- combined_summary_transposed[-1, ]
# Display the transposed combined summary table
print(combined_summary_transposed)
View(combined_summary_transposed)
write_xlsx(combined_summary_transposed, "summarystatsPROC.xlsx")









#.
# II. CALCULATIONS COUNTRY LEVEL.
# Calculate the median from merged_data3_caware_w
median_summary <- merged_data3_caware_w %>%
  group_by(country_iso3_code, aware_category) %>%
  summarise(median = median(PriceperDDD_w, na.rm = TRUE)) %>%
  ungroup()
# Calculate p25, p75, and IQR from merged_data3_catb_w
summary_catb <- merged_data3_catb_w %>%
  group_by(country_iso3_code, aware_category) %>%
  summarise(
    p25 = quantile(PriceperDDD_w, 0.25, na.rm = TRUE),
    p75 = quantile(PriceperDDD_w, 0.75, na.rm = TRUE),
    IQR = IQR(PriceperDDD_w, na.rm = TRUE)
  ) %>%
  ungroup()
# Merge the two summaries
combined_summary <- left_join(median_summary, summary_catb, by = c("country_iso3_code", "aware_category"))
# Pivot to make the table more readable
combined_summary_pivoted <- combined_summary %>%
  pivot_longer(cols = c(median, p25, p75, IQR), names_to = "statistic", values_to = "value") %>%
  unite("stat_category", statistic, aware_category, sep = "_") %>%
  pivot_wider(names_from = stat_category, values_from = value)
# Display the combined summary table
print(combined_summary_pivoted)
# Write the combined summary table to an Excel file
write_xlsx(combined_summary_pivoted, "combined_summ_country.xlsx")
merged_data3_caware_w <- weighted_avg_dataMIDAS_prCAw
#####
#.
#--------------------------------------------------------------------#
# RIGEPLOT and ATB data
#--------------------------------------------------------------------#
library(ggridges)
library(RColorBrewer)
library(viridis)
######
# Order the 'antimicrobials' factor levels

weighted_avg_data_v2 <- within(weighted_avg_dataMIDAS_prCA,
                               antimicrobials <- ordered(adila_antimicrobials, levels = rev(sort(unique(adila_antimicrobials)))))
weighted_avg_data_v2$WB_incomeshort[weighted_avg_data_v2$WB_incomeshort == "LMIC"] <- "MIC"

# Filter the data for aware category
weighted_avg_data_v2 <- weighted_avg_data_v2 %>%
  mutate(WB_incomeshort = case_when(
    WB_incomeshort == "HIC" ~ "HIC",
    WB_incomeshort == "MIC" ~ "MIC",
    TRUE ~ NA_character_
  ))

access_antimicrobials <- c("Amikacin", "Amoxicillin", "Ampicillin", "Benzylpenicillin", "Cefalexin", "Cefazolin", "Clindamycin", "Gentamicin", "Metronidazole", "Nitrofurantoin", "Oxacillin", "Tetracycline", "Phenoxymethylpenicillin")
weighted_avg_data_v2_access <- filter(weighted_avg_data_v2, aware_category == "Access", antimicrobials %in% access_antimicrobials)

watch_antimicrobials <- c("Azithromycin", "Cefepime", "Cefixine", "Cefotaxime", "Ceftazidime", "Ceftriazone", "Ciprofloxacin", "Clarithromycin", "Ertapenem", "Erythromycin", "Levofloxacin", "Lincomycin", "Meropenem", "Minocycline", "Moxifloxacin", "Norfloxacin", "Piperacillin+Tazobactam", "Rifaximin", "Spiramycin", "Teicoplanin", "Vancomycin")
weighted_avg_data_v2_watch <- filter(weighted_avg_data_v2, aware_category == "Watch", antimicrobials %in% watch_antimicrobials)

reserve_antimicrobials <- c("Colistin", "Linezolid", "Tedizolid", "Tigecycline", "Daptomycin", "Ceftaroline-fosamil")
weighted_avg_data_v2_reserve <- filter(weighted_avg_data_v2, aware_category == "Reserve", antimicrobials %in% reserve_antimicrobials)


# Create the Ridgeplot
b1<- ggplot(weighted_avg_data_v2_access, aes(x = PriceperDDD_w, y = adila_antimicrobials, fill = WB_incomeshort)) +
  geom_density_ridges(scale = 0.75, alpha = 0.3) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 20, by = 2)) +
  scale_fill_viridis_d() +
  coord_cartesian(xlim = c(0, 20)) +
  theme_classic() +  # Use classic theme as a base
  theme(legend.position = c(0.95, 0.25),  # Position the legend inside the plot region
        legend.background = element_rect(fill = alpha('white', 1), color = NA),  # Transparent background for legend
        legend.text = element_text(size = 13),  # Adjust legend text size
        legend.title = element_text(size = 13, face = "bold"),  # Adjust legend title size
        axis.line = element_line(size = 0.5, colour = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_line(size = 0.5, colour = "black"),
        axis.text.x = element_text(face = "bold", size = 13),
        axis.text.y = element_text(face = "bold", size = 13),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "mm")) +  # Adjust plot margins
  xlab("Antibiotic price per DDD (I$)") + 
  ylab("Antibiotics") +
  labs(fill = "Income group")

b2<-ggplot(weighted_avg_data_v2_watch, aes(x = PriceperDDD_w, y = adila_antimicrobials, fill = WB_incomeshort)) +
  geom_density_ridges(scale = 0.75, alpha = 0.3) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 30, by = 3)) +
  scale_fill_viridis_d() +
  coord_cartesian(xlim = c(0, 30)) +
  theme_classic() +  # Use classic theme as a base
  theme(legend.position = c(0.95, 0.25),  # Position the legend inside the plot region
        legend.background = element_rect(fill = alpha('white', 1), color = NA),  # Transparent background for legend
        legend.text = element_text(size = 13),  # Adjust legend text size
        legend.title = element_text(size = 13, face = "bold"),
        axis.line = element_line(size = 0.5, colour = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_line(size = 0.5, colour = "black"),
        axis.text.x = element_text(face = "bold", size = 13),
        axis.text.y = element_text(face = "bold", size = 13),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "mm")) +  # Adjust plot margins
  xlab("Antibiotic price per DDD (I$)") + 
  ylab("Antibiotics") +
  labs(fill = "Income group")


b3 <- ggplot(weighted_avg_data_v2_reserve, aes(x = PriceperDDD_w, y = adila_antimicrobials, fill = WB_incomeshort)) +
  geom_density_ridges(scale = 0.75, alpha = 0.3) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 400, by = 50)) +
  scale_fill_viridis_d() +
  coord_cartesian(xlim = c(0, 400)) +
  theme_classic() +  # Use classic theme as a base
  theme(legend.position = c(0.8, 0.5),  # Position the legend inside the plot region
        legend.background = element_rect(fill = alpha('white', 0.5), color = NA),  # Transparent background for legend
        legend.text = element_text(size = 13),  # Adjust legend text size
        legend.title = element_text(size = 13, face = "bold"),
        axis.line = element_line(size = 0.5, colour = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_line(size = 0.5, colour = "black"),
        axis.text.x = element_text(face = "bold", size = 13),
        axis.text.y = element_text(face = "bold", size = 13),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "mm")) +  # Adjust plot margins
  xlab("Antibiotic price per DDD (I$)") + 
  ylab("Antibiotics") +
  labs(fill = "Income group")


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("distributions_wb_atb_reserve.tiff", plot = b3, device = "tiff", width =9, height = 11, dpi = 500, units = "in")
ggsave("distributions_wb_atb_access.tiff", plot = b1, device = "tiff", width =9, height = 11, dpi = 500, units = "in")
ggsave("distributions_wb_atb_watch.tiff", plot = b2, device = "tiff", width =9, height = 11, dpi = 500, units = "in")


b1a <- b1 + 
  labs(subtitle = "A. Access") +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 10,face = "plain" ),
        plot.margin = margin(5, 5, 5, 5),
        plot.subtitle = element_text(hjust = 0.5, size=14, face = "bold"))
b2a <- b2 + 
  labs(subtitle = "B. Watch") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10,face = "plain"),
        legend.position = "none",
        plot.margin = margin(5, 5, 5, 5),
        plot.subtitle = element_text(hjust = 0.5, size=14, face = "bold"))
b3a <- b3 + 
  labs(subtitle = "C. Reserve") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10,face = "plain"),
        plot.margin = margin(5, 5, 5, 5),
        plot.subtitle = element_text(hjust = 0.5, size=14, face = "bold"))
# Combine the three plots into one row
combined_plotlol <- b1a + b2a + b3a + 
  plot_layout(ncol = 3)
# Display the combined plot
print(combined_plotlol)
ggsave("Figure2.tiff", plot = combined_plotlol, device = "tiff", width =16, height = 9, dpi = 1000, units = "in")

######

#--------------------------------------------------------------------#
# Summary Statistics by ANTIBIOTIC
#--------------------------------------------------------------------#
#####
#Libraries
#install.packages("cachem")
library(knitr)
library(kableExtra)
library(openxlsx)
library(cachem)
# Compute summary statistics by antimicrobials and aware_category
summary_statistics <- weighted_avg_data_v2 %>%
  group_by(adila_antimicrobials, aware_category) %>%
  summarise(
    median = median(PriceperDDD_w, na.rm = TRUE),
    p25 = quantile(PriceperDDD_w, 0.25, na.rm = TRUE),
    p75 = quantile(PriceperDDD_w, 0.75, na.rm = TRUE),
    IQR = IQR(PriceperDDD_w, na.rm = TRUE)
  ) %>%
  ungroup()
# Ensure the order of aware_category
summary_statistics$aware_category <- factor(summary_statistics$aware_category, levels = c("Access", "Watch", "Reserve"))
# Order the summary statistics
summary_statistics <- summary_statistics %>%
  arrange(aware_category, adila_antimicrobials)
# Create and display the table with sections for each aware_category
summary_statistics %>%
  kable("html", caption = "Summary Statistics of Antibiotic Prices by AWARE Category") %>%
  kable_styling(full_width = FALSE) %>%
  pack_rows("Access", 1, nrow(filter(summary_statistics, aware_category == "Access"))) %>%
  pack_rows("Watch", nrow(filter(summary_statistics, aware_category == "Access")) + 1, nrow(filter(summary_statistics, aware_category == "Access")) + nrow(filter(summary_statistics, aware_category == "Watch"))) %>%
  pack_rows("Reserve", nrow(filter(summary_statistics, aware_category == "Access")) + nrow(filter(summary_statistics, aware_category == "Watch")) + 1, nrow(summary_statistics))
# Export the summary statistics to Excel
write.xlsx(summary_statistics, "SummaryS_Atb_Prices.xlsx")
#####
#Differentiate between Oral/Parenteral and AWaRe; by country, by WB income groups and by WHO regions #####
#by COUNTRY/atb adm route/ aware atb: /////////// /////////// ///////////
filtered_midas_pricekok <- midas_price %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve"))
filtered_midas_pricekok$CAdminA <- paste(filtered_midas_pricekok$country, filtered_midas_pricekok$route_of_administration, filtered_midas_pricekok$aware_category, sep = "_") 

weighted_avg_dataMIDAS_prCAdminA <- filtered_midas_pricekok %>%
  group_by(CAdminA) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditureCAA= sum(total_expenditure), total_ddd_soldCAA = sum(ddd), median_PriceperDDD = median(PriceperDDD, na.rm = TRUE),
            p25_PriceperDDD = quantile(PriceperDDD, 0.25, na.rm = TRUE),
            p75_PriceperDDD = quantile(PriceperDDD, 0.75, na.rm = TRUE), country=first(country), aware_category=first(aware_category), route_of_administration=first(route_of_administration), WB_income= first(WB_income), na.rm = TRUE)

summary_tableCC_ad <- weighted_avg_dataMIDAS_prCAdminA %>%
  mutate(aware_category = factor(aware_category, levels = c("Access", "Watch", "Reserve"))) %>%
  group_by(country, route_of_administration, aware_category) %>%
  summarise(
    VolumeweightedP = PriceperDDD_w,
    p25_PriceperDDD = p25_PriceperDDD,
    median_PriceperDDD = median_PriceperDDD,
    p75_PriceperDDD = p75_PriceperDDD
  )
print(summary_tableCC_ad)
write_xlsx(summary_tableCC_ad, "summary_tableCC_ad.xlsx")


#World Bank income groups /////////// ///////////
filtered_midas_pricekokWB <- midas_price %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve"))
filtered_midas_pricekokWB$WBAdminA <- paste(filtered_midas_pricekok$WB_income, filtered_midas_pricekok$route_of_administration, filtered_midas_pricekok$aware_category, sep = "_") 

weighted_avg_dataMIDAS_prCAdminWB <- filtered_midas_pricekokWB %>%
  group_by(WBAdminA) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditureCAA= sum(total_expenditure), total_ddd_soldCAA = sum(ddd),     median_PriceperDDD = median(PriceperDDD, na.rm = TRUE),
            p25_PriceperDDD = quantile(PriceperDDD, 0.25, na.rm = TRUE),
            p75_PriceperDDD = quantile(PriceperDDD, 0.75, na.rm = TRUE), country=first(country), aware_category=first(aware_category), route_of_administration=first(route_of_administration), WB_income= first(WB_income), na.rm = TRUE)

summary_tableWB_ad <- weighted_avg_dataMIDAS_prCAdminWB %>%
  mutate(aware_category = factor(aware_category, levels = c("Access", "Watch", "Reserve"))) %>%
  group_by(WB_income, route_of_administration, aware_category) %>%
  summarise(
    VolumeweightedP = PriceperDDD_w,
    p25_PriceperDDD = p25_PriceperDDD,
    median_PriceperDDD = median_PriceperDDD,
    p75_PriceperDDD = p75_PriceperDDD
  )
print(summary_tableWB_ad)
write_xlsx(summary_tableWB_ad, "summary_tableWB_ad.xlsx")

#WHO-region /////////// ///////////
filtered_midas_pricekokWR <- midas_price %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve"))
filtered_midas_pricekokWR$WRAdminA <- paste(filtered_midas_pricekok$WHO_region, filtered_midas_pricekok$route_of_administration, filtered_midas_pricekok$aware_category, sep = "_") 

weighted_avg_dataMIDAS_prCAdminWR <- filtered_midas_pricekokWR %>%
  group_by(WRAdminA) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd),     median_PriceperDDD = median(PriceperDDD, na.rm = TRUE),
            p25_PriceperDDD = quantile(PriceperDDD, 0.25, na.rm = TRUE),
            p75_PriceperDDD = quantile(PriceperDDD, 0.75, na.rm = TRUE), country=first(country), aware_category=first(aware_category), route_of_administration=first(route_of_administration), WB_income= first(WB_income), WHO_region=first(WHO_region), na.rm = TRUE)

summary_tableWHO_ad <- weighted_avg_dataMIDAS_prCAdminWR %>%
  mutate(aware_category = factor(aware_category, levels = c("Access", "Watch", "Reserve"))) %>%
  group_by(WHO_region, route_of_administration, aware_category) %>%
  summarise(
    VolumeweightedP = PriceperDDD_w,
    p25_PriceperDDD = p25_PriceperDDD,
    median_PriceperDDD = median_PriceperDDD,
    p75_PriceperDDD = p75_PriceperDDD
  )
print(summary_tableWHO_ad)
write_xlsx(summary_tableWHO_ad, "summary_tableWHO_ad.xlsx")
# Browse the  weighted_avg_dataMIDAS_prCAdmin  weighted_avg_dataMIDAS_prCAdminWB AND weighted_avg_dataMIDAS_prCAdminWR and copy paste them into excel to prepare the Tables.


stats_PArenA <- weighted_avg_dataMIDAS_prCAdminA %>%
  group_by(route_of_administration, aware_category) %>%
  summarise(
    min = min(PriceperDDD_w, na.rm = TRUE),
    Q1 = quantile(PriceperDDD_w, 0.25, na.rm = TRUE),
    median = median(PriceperDDD_w, na.rm = TRUE),
    Q3 = quantile(PriceperDDD_w, 0.75, na.rm = TRUE),
    max = max(PriceperDDD_w, na.rm = TRUE)
  )
print(stats_PArenA)


#####

#--------------------------------------------------------------------#
# WORLD BANK DATA: DDDs consumption per 1000 populations
#--------------------------------------------------------------------#

#####Downloading WB data for all countries, GDP, GINI, CHE, etc#######
library(tibble)
#devtools::install_github("worldbank/pipr")
library(pipr)
library(dplyr)
library(httr)
library(dplyr)
library(wbstats)
library(writexl)
#remove.packages("wbstats")
#remove.packages("httr")
#remove.packages("pipr")
#remove.packages("cachem")

# Define the countries and indicators
country_iso3_codex <- c('ARE', 'ARG', 'AUS', 'AUT', 'BEL', 'BGD', 'BGR', 'BIH', 'BLR', 'BRA', 'CAN', 'CHE', 'CHL', 
                        'CHN', 'COL', 'CZE', 'DEU', 'DOM', 'DZA', 'ECU', 'EGY', 'ESP', 'EST', 'FIN', 'FRA', 
                        'GBR', 'GRC', 'HKG', 'HRV', 'HUN', 'IDN', 'IND', 'IRL', 'ITA', 'JOR', 'JPN', 'KAZ', 
                        'KOR', 'KWT', 'LBN', 'LKA', 'LTU', 'LUX', 'LVA', 'MAR', 'MEX', 'MYS', 'NLD', 'NOR', 
                        'NZL', 'PAK', 'PER', 'PHL', 'POL', 'PRI', 'PRT', 'ROU', 'RUS', 'SAU', 'SGP', 'SRB', 
                        'SVK', 'SVN', 'SWE', 'THA', 'TUN', 'TUR', 'TWN', 'URY', 'USA', 'VEN', 'VNM', 'ZAF')

indicators <- c(
  "NY.GDP.MKTP.CD",  # GDP (current US$)
  "SP.POP.TOTL",     # Population, total
  "SI.POV.GINI",     
  "SI.POV.NAHC",    # Poverty headcount ratio at national poverty lines (% of population)
  "SH.XPD.CHEX.GD.ZS", # Current health expenditure (% of GDP)
  "SH.XPD.CHEX.PC.CD"  # Current health expenditure per capita
)
# Split the list of countries into smaller chunks
split_countries <- split(country_iso3_codex, ceiling(seq_along(country_iso3_codex)/20))
# Function to fetch data for a chunk of countries
fetch_chunk_data <- function(countries, indicators, start_date, end_date) {
  for (i in 1:5) {  # Retry up to 5 times
    try({
      data <- wb(country = countries, indicator = indicators, startdate = start_date, enddate = end_date)
      return(data)
    }, silent = TRUE)
    Sys.sleep(5)  # Wait for 5 seconds before retrying
  }
  stop("World Bank API request failed after multiple attempts")
}
# Fetch data for each chunk and combine the results
all_data <- lapply(split_countries, function(countries) {
  fetch_chunk_data(countries, indicators, 2017, 2021)
})
# Combine all the data into a single dataframe
combined_data <- bind_rows(all_data)
# Print a sample of the fetched data to verify
print(head(combined_data))
# Export the data to an Excel file
#write_xlsx(combined_data, "fetched_data.xlsx")

######

###NEW, load fetched_data##########
library(readxl)
file_pathyxx <- "fetched_data.xlsx"
combined_data <- read_excel(file_pathyxx)

# Pivot the data to wide format
wide_data <- combined_data %>%
  pivot_wider(names_from = indicatorID, values_from = value)
# Rename columns for clarity
data <- wide_data %>%
  rename(
    country_iso3_code = iso3c,
    year = date,
    gdp = `NY.GDP.MKTP.CD`,
    population = `SP.POP.TOTL`,
    gini = `SI.POV.GINI`,
    nahc = `SI.POV.NAHC`,
    CHE_PERCgdp = `SH.XPD.CHEX.GD.ZS`,
    CHE_pp = `SH.XPD.CHEX.PC.CD`
  )


# Filter to keep only the data for 2019
fill_missing_data <- function(df) {
  df %>%
    group_by(country_iso3_code) %>%
    arrange(country_iso3_code, desc(year)) %>%
    fill(gdp, population, gini, nahc, CHE_PERCgdp, CHE_pp, .direction = "downup")
}
# Apply the function to fill missing data
data_filled <- fill_missing_data(data)

data_2019 <- data_filled %>%
  filter(year == 2019, country_iso3_code %in% country_iso3_codex)


# INSERT Taiwan to the WB data: TWN
data_2019 <- data_2019 %>%
  ungroup()
new_row <- tibble(
  country_iso3_code = "TWN",
  year = 2019)
data_2019$year <- as.numeric(data_2019$year)  # Convert existing year to numeric
data_2019 <- data_2019 %>%
  add_row(new_row)

data_2019<- data_2019 %>%
  distinct(country_iso3_code, .keep_all = TRUE)  # Remove duplicates based on iso3c



#Replacing data as for Venezuela GDP in 2019
data_2019 <- data_2019 %>%
  mutate(gdp = if_else(country == "Venezuela, RB", 76021696192, gdp))
data_2019 <- data_2019 %>%
  mutate(
    gdp = if_else(country_iso3_code == 'ARE', 418000000000, gdp),
    population = if_else(country_iso3_code == 'ARE', 9219000, population)
  )
data_2019 <- data_2019 %>%
  mutate(
    gdp = if_else(country_iso3_code == 'TWN', 611396000000, gdp),
    population = if_else(country_iso3_code == 'TWN', 23600000, population),
    year= if_else(country_iso3_code == 'TWN', 2019, year)
  )
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "DZA", 38.1, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "JPN", 44.2, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "JOR", 51.6, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "TWN", 34.2, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "HKG", 51.8, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "KWT", 47.1, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "LBN", 45.8, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "MAR", 50.6, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "PRI", 55.1, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "NZL", 65.4, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "SGP", 45, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "VEN", 60, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "SAU", 45.6, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "ZAF", 34.6, gini))
data_2019 <- data_2019 %>%
  mutate(gini = if_else(country_iso3_code == "BIH", 65.7, gini))
#GDP per capita
data_2019$GDP_pc <- data_2019$gdp/ data_2019$population

#Merge weights for ppp-adjusted GDP.
data_2019 <- data_2019 %>%
  inner_join(ppp_2019_iso %>% dplyr::select(country_iso3_code, PPP_rate, ER_rate), by = "country_iso3_code")

data_2019$GDP_pcPPP <- (data_2019$GDP_pc * data_2019$ER_rate)/data_2019$PPP_rate
data_2019$GDP_PPP <- (data_2019$gdp * data_2019$ER_rate)/data_2019$PPP_rate
######
#####DID calculation below #DID <- (total_DDDs / population) * 1000 * (1/365) so it is antibiotic usage per 1,000 inhabitants per day ######
merged_dataxx <- merge(merged_data3_caware_w, data_2019, by = "country_iso3_code", all = TRUE)
merged_dataxx$Acc_ddd_s_p1000<- (merged_data3_caware_w$total_ddd_sold/merged_dataxx$population)*1000*(1/365)
merged_dataxx <- merged_dataxx %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve"))

######
#FINAL dataset: merged_dataxx
#####
##### First Graph bubbles, not too good.#####
#Graph DDD per 1,000 consumption and GDP per capita? DDD consumption per aWARE category, but also prices and GDP somehow. think of WHO region and WB still.
who_colors <- c("AFR" = "#377eb8",  # Blue
                "AMR" = "#4daf4a",  # Green
                "EMR" = "#e41a1c",  # Red
                "EUR" = "#ff7f00",  # Orange
                "WPR" = "#984ea3")  # Purple
# Extract unique categories
categories <- unique(merged_dataxx$aware_category)
plots <- list()  # Initialize a list to store plots
for (category in categories) {
  # Calculate quantiles for five distinct size categories
  size_breaks <- quantile(subset(merged_dataxx, aware_category == category)$PriceperDDD_w, 
                          probs = seq(0, 1, length.out = 3))  # This ensures exactly 5 ubbles
  
  # Create each plot and add it to the list
  plots[[category]] <- ggplot(subset(merged_dataxx, aware_category == category), 
                              aes(x = Acc_ddd_s_p1000, y = GDP_pc, size = PriceperDDD_w, color = WHO_region)) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "red", show.legend = FALSE) +  # Spline fit
    geom_point(alpha = 0.7) +  # Points on top
    scale_size_area(max_size = 12, 
                    breaks = size_breaks, 
                    labels = as.character(round(size_breaks, 0)),
                    name = "Price ($int) per DDD") +
    scale_color_manual(values = who_colors, name = "WHO region") +
    labs(x = "DDDs per 1,000 inhabitants/day", y = "GDP PPP", title = paste("Aware Category:", category)) +
    theme_minimal() +
    theme(text = element_text(family = "sans", size = 12),
          axis.title = element_text(size = 12, face = "plain"),
          legend.position = c(0.99, 0.4),  # Default position for all other plots
          legend.justification = c(1, 0),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "black"),
          legend.background = element_rect(fill = NA, colour = NA),
          legend.key = element_blank(),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.line = element_line(color = "black"))
}

specific_categories <- c("Access", "Reserve")
for (category in specific_categories) {
  if (category %in% names(plots)) {  # Check if the plot for the category exists in the list
    plots[[category]] <- plots[[category]] + 
      guides(color = "none") +  # Remove WHO region legend
      theme(legend.position = c(0.01, 0.99),  # Move legend to top left corner
            legend.justification = c(0, 1),
            axis.title.x= element_blank())  # Top left corner
  }
}
specific_categories2 <- c("Watch", "Reserve")
for (category in specific_categories2) {
  if (category %in% names(plots)) {  # Check if the plot for the category exists in the list
    plots[[category]] <- plots[[category]] +   
      theme(axis.title.y= element_blank())  # Top left corner
  }
}

# Example to print the plo
cox1<-plots$Access
cox2<-plots$Watch
cox3<-plots$Reserve

combo_x<- cox1| cox2|cox3
combo_x
setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("plot_bubbles_combo.tiff", plot = combo_x, device = "tiff", width =14, height = 7, dpi = 500, units = "in")
getwd()
#####
##### Second graph bubbles, not too good #####
# Load necessary packages

library(ggplot2)
library(patchwork)

# Ensure merged_dataxx is loaded and filtered appropriately
# Filter out rows with missing values in relevant columns
merged_dataxx <- merged_dataxx %>%
  filter(!is.na(PriceperDDD_w), !is.na(Acc_ddd_s_p1000), !is.na(GDP_pc))

categories <- unique(merged_dataxx$aware_category)
plots <- list()  # Initialize a list to store plots

for (category in categories) {
  # Calculate quantiles for three distinct size categories
  size_breaks <- quantile(subset(merged_dataxx, aware_category == category)$GDP_pc, 
                          probs = seq(0, 1, length.out = 3), na.rm = TRUE)  # Adjust to get the required bubble sizes
  
  # Create each plot and add it to the list
  plots[[category]] <- ggplot(subset(merged_dataxx, aware_category == category), 
                              aes(x = Acc_ddd_s_p1000, y = PriceperDDD_w, size = GDP_pc, color = WHO_region)) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "red", show.legend = FALSE) +  # Spline fit
    geom_point(alpha = 0.7) +  # Points on top
    scale_size_area(max_size = 12, 
                    breaks = size_breaks, 
                    labels = as.character(round(size_breaks, 0)),
                    name = "GDP per capita") +
    scale_color_manual(values = who_colors, name = "WHO region") +
    labs(x = "DDDs per 1,000 inhabitants/day", y = "Price per DDD", title = paste("Aware Category:", category)) +
    theme_minimal() +
    theme(text = element_text(family = "sans", size = 12),
          axis.title = element_text(size = 12, face = "plain"),
          legend.position = c(0.99, 0.4),  # Default position for all other plots
          legend.justification = c(1, 0),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "black"),
          legend.background = element_rect(fill = NA, colour = NA),
          legend.key = element_blank(),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.line = element_line(color = "black"))
}

specific_categories <- c("Access", "Reserve")
for (category in specific_categories) {
  if (category %in% names(plots)) {  # Check if the plot for the category exists in the list
    plots[[category]] <- plots[[category]] + 
      guides(color = "none") +  # Remove WHO region legend
      theme(legend.position = c(0.01, 0.99),  # Move legend to top left corner
            legend.justification = c(0, 1),
            axis.title.x = element_blank())  # Top left corner
  }
}

specific_categories2 <- c("Watch", "Reserve")
for (category in specific_categories2) {
  if (category %in% names(plots)) {  # Check if the plot for the category exists in the list
    plots[[category]] <- plots[[category]] +   
      theme(axis.title.y = element_blank())  # Top left corner
  }
}

# Combine the plots
cox1 <- plots$Access
cox2 <- plots$Watch
cox3 <- plots$Reserve

combo_x <- cox1 | cox2 | cox3

# Set the working directory and save the plot
setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("plot_bubbles_combo2.tiff", plot = combo_x, device = "tiff", width = 14, height = 7, dpi = 500, units = "in")


#####

#---------------------------------------------------------------------------------------------#
#SPLINE fitting and graphs below: Price per DDD & GDP pc and PPP -----------------------------#
#---------------------------------------------------------------------------------------------#
# Step 0: Distribution for Prices first: check outcome variable distribution: GRAPH.###### #####
filtered_dataxhj <- weighted_avg_dataMIDAS_prCAw %>%
  filter(aware_category %in% c("Access", "Watch", "Reserve"))

lancet_colors <- c("Access" = "#42B540FF", "Watch" = "yellow", "Reserve" = "#E41A1C")
kggij<- ggplot(filtered_dataxhj, aes(x = PriceperDDD_w, fill = aware_category, color = aware_category)) +
  geom_density(alpha = 0.7, size = 1) +
  facet_wrap(~ aware_category, nrow = 3, scales = "free") +
  scale_fill_manual(values = lancet_colors) +
  scale_color_manual(values = lancet_colors) +
  theme_minimal() +
  labs(
    title = "Density of Price per DDD by AWaRe Category",
    x = "Price per DDD (int$)",
    y = "Density",
    fill = "AWaRe Category",
    color = "AWaRe Category"
  ) +
  theme(
    text = element_text(family = "Arial", color = "#333333"),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.text = element_text(size = 12),
    legend.position = "none"  # Remove legend as it's not needed in faceted plots
  )
ggsave("DensitypricesAWaRe.tiff", plot = kggij, device = "tiff", width =10, height = 10, dpi = 500, units = "in")
getwd()
#####
# Step 1: Fitting different distributions; best AIC is for gamma distribution ##### #####
library(fitdistrplus)
fit_and_compare_distributions <- function(data, category) {
  # Ensure the data is a numeric vector of length greater than 1
  if (is.numeric(data) && length(data) > 1) {
    # Fit distributions
    fit_normal <- try(fitdist(data, "norm"))
    fit_gamma <- try(fitdist(data, "gamma"))
    fit_lognormal <- try(fitdist(data, "lnorm"))
    fit_weibull <- try(fitdist(data, "weibull"))
    # Compare fits using AIC
    fits <- list(normal = fit_normal, gamma = fit_gamma, lognormal = fit_lognormal, weibull = fit_weibull)
    valid_fits <- fits[sapply(fits, inherits, "fitdist")]
    fit_comparison <- gofstat(valid_fits)
    return(list(category = category, fit_comparison = fit_comparison))
  } else {
    return(list(category = category, fit_comparison = NULL))
  }
}
# Apply function to each category
access_data <- filtered_dataxhj %>% filter(aware_category == "Access") %>% pull(PriceperDDD_w)
watch_data <- filtered_dataxhj %>% filter(aware_category == "Watch") %>% pull(PriceperDDD_w)
reserve_data <- filtered_dataxhj %>% filter(aware_category == "Reserve") %>% pull(PriceperDDD_w)
access_fit <- fit_and_compare_distributions(access_data, "Access")
watch_fit <- fit_and_compare_distributions(watch_data, "Watch")
reserve_fit <- fit_and_compare_distributions(reserve_data, "Reserve")
# Print comparison results
if (!is.null(access_fit$fit_comparison)) {
  print("Access Category Fit Comparison:")
  print(access_fit$fit_comparison)
}
if (!is.null(watch_fit$fit_comparison)) {
  print("Watch Category Fit Comparison:")
  print(watch_fit$fit_comparison)
}
if (!is.null(reserve_fit$fit_comparison)) {
  print("Reserve Category Fit Comparison:")
  print(reserve_fit$fit_comparison)
}

#Gamma distribution it is!

#####
##Step 2:MODELS SPLINE with GDP per capita\ #####
library(mgcv)
# Fit models with different values of k=polynomial degree
#Choose best: 3rd polynomial degree
data_access <- subset(merged_dataxx, aware_category == "Access")
data_watch <- subset(merged_dataxx, aware_category == "Watch")
data_reserve <- subset(merged_dataxx, aware_category == "Reserve")

k_values <- c(1, 2, 3, 4, 5, 6, 7, 8)
models <- lapply(k_values, function(k) {
  glm(PriceperDDD_w ~ ns(GDP_pc, df = k), data = data_watch, family = Gamma(link = "log"))
})
# Extract and compare AIC to choose best goodness-of-fit
aic_values <- sapply(models, AIC)
names(aic_values) <- k_values
aic_values #CHECK AIC and decide the model

#glm models
gam_model_a <- glm(PriceperDDD_w ~ ns(GDP_pc, df = 7), family = Gamma(link = "log"), data = data_access)
summary(gam_model_a)
gam_model_w <- glm(PriceperDDD_w~ ns(GDP_pc, df = 3), family = Gamma(link = "log"), data = data_watch)
summary(gam_model_w)
gam_model_r <- glm(PriceperDDD_w ~ ns(GDP_pc, df = 4), family = Gamma(link = "log"), data = data_reserve)
summary(gam_model_r)

#---------------------------------------------------------#
#multivariate multiple model# ----------------------------#
access_data <- merged_dataxx %>% 
  filter(aware_category == "Access") %>%
  dplyr::select(country_iso3_code, GDP_pc, GDP_pcPPP, WHO_region, WB_income, WB_incomeshort,  Access = PriceperDDD_w)
watch_data <- merged_dataxx %>% 
  filter(aware_category == "Watch") %>%
  dplyr::select(country_iso3_code, GDP_pc, GDP_pcPPP, WHO_region, WB_income, WB_incomeshort, Watch = PriceperDDD_w)
reserve_data <- merged_dataxx %>% 
  filter(aware_category == "Reserve") %>%
  dplyr::select(country_iso3_code, GDP_pc, GDP_pcPPP, WHO_region, WB_income, WB_incomeshort, Reserve = PriceperDDD_w)
combined_dataXOX <- access_data %>%
  full_join(watch_data, by = c("country_iso3_code", "GDP_pc", "GDP_pcPPP", "WHO_region", "WB_income","WB_incomeshort")) %>%
  full_join(reserve_data, by = c("country_iso3_code", "GDP_pc", "GDP_pcPPP", "WHO_region", "WB_income", "WB_incomeshort"))

#Model fit below:
lm_model_multiv <- lm(cbind(Access, Watch, Reserve) ~ ns(GDP_pc, df = 4), data = combined_dataXOX, amily = gaussian(link = "log"))
summary(lm_model_multiv)
new_data <- data.frame(GDP_pc = seq(min(combined_dataXOX$GDP_pc), max(combined_dataXOX$GDP_pc), length.out = 100))
# List to store predictions
predictions_list <- list()
# Loop through each response variable and make predictions
response_vars <- c("Access", "Watch", "Reserve")
for (response_var in response_vars) {
  # Fit a univariate model for each response variable with the same predictor
  lm_model <- lm(as.formula(paste(response_var, "~ ns(GDP_pc, df = 4)")), data = combined_dataXOX)
  # Predict with confidence intervals
  predictions <- predict(lm_model, newdata = new_data, interval = "confidence")
  # Store predictions in the list with response variable name
  predictions_list[[response_var]] <- data.frame(new_data, 
                                                 fit = predictions[, "fit"], 
                                                 lwr = predictions[, "lwr"], 
                                                 upr = predictions[, "upr"],
                                                 response_var = response_var)
}
# Combine the results into a single dataframe
predictions_df <- bind_rows(predictions_list)
# Define Lancet colors
colors <- c("Access" = "#00B050", "Watch" = "#FFC000", "Reserve" = "#FF0000")
predictions_df <- predictions_df %>%
  mutate(lwr = if_else(lwr < 0, 0, lwr))
predictions_df <- predictions_df %>%
  mutate(fit = if_else(response_var == "Reserve", fit + 2, fit))

# Create individual plots for each category
common_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),  # Bold plot titles for A, B, C
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add border around each plot
    axis.text = element_text(size = 10),  # Adjust axis text size
    axis.title = element_text(size = 12)  # Adjust axis title size
  )

# Create individual plots for each category with adjustments
plot_access <- ggplot(predictions_df %>% filter(response_var == "Access"), aes(x = GDP_pc, y = fit)) +
  geom_line(color = colors["Access"]) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = colors["Access"], alpha = 0.1) +
  labs(title = "A", x = "", y = "Access (Predicted price per DDD)") +
  scale_x_continuous(breaks = seq(0, 100000, by = 20000), limits = c(0, 100000)) +  # Set x-axis range and breaks
  common_theme

plot_watch <- ggplot(predictions_df %>% filter(response_var == "Watch"), aes(x = GDP_pc, y = fit)) +
  geom_line(color = colors["Watch"]) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = colors["Watch"], alpha = 0.1) +
  labs(title = "B", x = "", y = "Watch (Predicted price per DDD)") +
  scale_x_continuous(breaks = seq(0, 100000, by = 20000), limits = c(0, 100000)) +  # Set x-axis range and breaks
  common_theme

plot_reserve <- ggplot(predictions_df %>% filter(response_var == "Reserve"), aes(x = GDP_pc, y = fit)) +
  geom_line(color = colors["Reserve"]) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = colors["Reserve"], alpha = 0.1) +
  labs(title = "C", x = "GDP per capita (in 2019 USD)", y = "Reserve (Predicted price per DDD)") +
  scale_x_continuous(breaks = seq(0, 100000, by = 20000), limits = c(0, 100000)) +  # Set x-axis range and breaks
  common_theme

# Combine the three plots into one column layout
combined_plot <- grid.arrange(plot_access, plot_watch, plot_reserve, ncol = 1)

# Display the combined plot
combined_plot

# Save the combined plot to a TIFF file with specified dimensions and resolution
ggsave("plot_multivmodel_aware.tiff", plot = combined_plot, device = "tiff", width = 7, height = 10, dpi = 1000, units = "in")




#finish here the graph using multivariate model#



#Graphs for alone MODELS.
predictions_a <- predict(gam_model_a, newdata = data_access, type = "response", se = T)
# Create 'fit', 'lower', and 'upper' in data_access
data_access$fit <- predictions_a$fit
data_access$lower <- predictions_a$fit - 1.96 * predictions_a$se.fit
data_access$upper <- predictions_a$fit + 1.96 * predictions_a$se.fit

predictions_w <- predict(gam_model_w, newdata = data_watch, type = "response", se = T)
# Create 'fit', 'lower', and 'upper' in data_watch
data_watch$fit <- predictions_w$fit
data_watch$lower <- predictions_w$fit - 1.96 * predictions_w$se.fit
data_watch$upper <- predictions_w$fit + 1.96 * predictions_w$se.fit

predictions_r <- predict(gam_model_r, newdata = data_reserve, type = "response", se = T)
# Create 'fit', 'lower', and 'upper' in data_reserve
data_reserve$fit <- predictions_r$fit
data_reserve$lower <- predictions_r$fit - 1.96 * predictions_r$se.fit
data_reserve$upper <- predictions_r$fit + 1.96 * predictions_r$se.fit
#####
##STEP 2.1: New models systemfit, structural equations:#####
combined_dataXOX$GDP_pcPPP[combined_dataXOX$country_iso3_code == "HRV"] <- 29000

eq1 <- Access ~ ns(GDP_pcPPP, df = 4) 
eq2 <- Watch ~ ns(GDP_pcPPP, df = 4) 
eq3 <- Reserve~ns(GDP_pcPPP, df = 4) 
equations <- list(Access = eq1, Watch = eq2, Reserve = eq3)
fit <- systemfit(equations, method = "SUR", data = combined_dataXOX)



gdp_range <- seq(min(combined_dataXOX$GDP_pcPPP, na.rm = TRUE), max(combined_dataXOX$GDP_pcPPP, na.rm = TRUE), length.out = 100)
plot_data <- data.frame(GDP_pcPPP = gdp_range)

# Predict values using the model for each equation
plot_data$Access_pred <- predict(fit$eq[[1]], newdata = plot_data)
plot_data$Watch_pred <- predict(fit$eq[[2]], newdata = plot_data)
plot_data$Reserve_pred <- predict(fit$eq[[3]], newdata = plot_data)

# Convert predictions to numeric
plot_data$Access_pred <- as.numeric(unlist(plot_data$Access_pred))
plot_data$Watch_pred <- as.numeric(unlist(plot_data$Watch_pred))
plot_data$Reserve_pred <- as.numeric(unlist(plot_data$Reserve_pred))

# Construct plots
p1 <- ggplot(plot_data, aes(x = GDP_pcPPP)) +
  #geom_ribbon(aes(ymin = Access_pred_lower, ymax = Access_pred_upper), fill = "#4DAF4A33") +
  geom_line(aes(y = Access_pred), color = "#4DAF4A", size = 1.2) +
  labs(title = "Predicted Access prices vs. GDP per capita", x = "GDP per capita (I$)", y = "Price per DDD (I$)") +
  theme_minimal() 

p2 <- ggplot(plot_data, aes(x = GDP_pcPPP)) +
  #geom_ribbon(aes(ymin = Watch_pred_lower, ymax = Watch_pred_upper), fill = "yellow1") +
  geom_line(aes(y = Watch_pred), color = "gold", size = 1.2) +
  labs(title = "Predicted Watch prices vs. GDP per capita", x = "GDP per capita (I$)", y = "Price per DDD (I$)") +
  theme_minimal()

p3 <- ggplot(plot_data, aes(x = GDP_pcPPP)) +
  #geom_ribbon(aes(ymin = Reserve_pred_lower, ymax = Reserve_pred_upper), fill = "#E41A1C33") +
  geom_line(aes(y = Reserve_pred), color = "#E41A1C", size = 1.2) +
  labs(title = "Predicted Reserve prices vs. GDP per capita", x = "GDP per capita (I$)", y = "Price per DDD (I$)") +
  theme_minimal() 

# Combine the plots vertically for a single display
Figure_predsystem<- grid.arrange(p1, p2, p3, ncol = 1)
ggsave("Figure_predsystem.tiff", plot = Figure_predsystem, device = "tiff", width =8, height = 10, dpi = 500, units = "in")





#####
#Step 3: Graph with 95% CI predicted values #####
#a1x<-plot(gam_model_a, pages = 1, scheme = 1, all.terms = TRUE, se = TRUE, shade = TRUE, rug = FALSE)
#a2x<-plot(gam_model_w, pages = 1, scheme = 1, all.terms = TRUE, se = TRUE, shade = TRUE, rug = FALSE)
#a3x<-plot(gam_model_r, pages = 1, scheme = 1, all.terms = TRUE, se = TRUE, shade = TRUE, rug = FALSE)
# Determine common x-axis range across all subsets
common_range <- c(min(c( data_reserve$GDP_pc, na.rm = TRUE)), 100000)
# Establish common breaks - you may adjust these as needed based on your data distribution
common_breaks <- pretty(common_range, n = 4)
# Access
a1x <- ggplot(data_access, aes(x = GDP_pc)) +
  geom_line(aes(y = fit), color = "#FFD700") +  # Line for the GAM fit
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#FFD700", alpha = 0.2) +  # Confidence interval
  labs(title = "A. Access", y = "Predicted Avg Price", x = "GDP per Capita (USD)") +  # Axis labels and plot title
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, vjust = 0, margin = margin(t = 5, r = 0, b = 5, l = 5)),
    axis.title.x = element_blank(),  # Set x-axis title font size
    axis.title.y = element_blank(),  # Set y-axis title font size
    axis.text.x = element_blank(),  # Set x-axis text font size
    axis.text.y = element_text(size = 13),  # Set y-axis text font size
    axis.line = element_line(color = "black")  # Add black line to mark the axes
  ) +
  scale_x_continuous(limits = common_range, breaks = common_breaks)

# Watch

a2x <- ggplot(data_watch, aes(x = GDP_pc)) +
  geom_line(aes(y = fit), color = "#008000") +  # Line for the GAM fit
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#008000", alpha = 0.2) +  # Confidence interval
  labs(title = "B. Watch", y = "Predicted volume-weighted price per DDD (int$)") +  # Axis labels and plot title
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, vjust = 0, margin = margin(t = 5, r = 0, b = 5, l = 5)),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.line = element_line(color = "black")
  ) +
  scale_x_continuous(limits = common_range, breaks = common_breaks) +
  coord_cartesian(ylim = c(0, NA))

# Reserve

a3x <- ggplot(data_reserve, aes(x = GDP_pc)) +
  geom_line(aes(y = fit), color = "#FF0000") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#FF0000", alpha = 0.2) +
  labs(title = "C. Reserve", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0, vjust = 0, margin = margin(t = 5, r = 0, b = 5, l = 5)),
        axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x= element_text(size = 15),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-70, 260), breaks = seq(-50, 250, by = 50))

# Combine the plots into a single column
a3x <- a3x + labs(x = "GDP per capita (int$)")  # Set x-axis title for Reserve graph

# Now combine the plots

common_range <- c(0, 80000)
common_breaks <- seq(0, 80000, by = 10000)

# Apply the x-axis limits and breaks to each plot
a1x <- a1x + scale_x_continuous(breaks = common_breaks) + coord_cartesian(xlim = common_range)
a2x <- a2x + scale_x_continuous(breaks = common_breaks) + coord_cartesian(xlim = common_range)
a3x <- a3x + scale_x_continuous(breaks = common_breaks) + coord_cartesian(xlim = common_range)

combined_plot <- a1x / a2x / a3x + 
  plot_layout(ncol = 1, heights = c(1, 1, 1))  # Ensure equal spacing and heights
setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("plot_spline_comboGDPppPPPadj.tiff", plot = combined_plot, device = "tiff", width =8, height = 10, dpi = 500, units = "in")
getwd()

#######
#Step 4: Regression results export excel with WHO and WB interactions #########
# Fit the initial four models without interaction terms
library(broom)
gam_model_a <- glm(Access ~ ns(GDP_pc, df = 7), family = Gamma(link = "log"), data = access_data)
gam_model_w <- glm(Watch ~ ns(GDP_pc, df = 3), family = Gamma(link = "log"), data = watch_data)
gam_model_r <- glm(Reserve ~ ns(GDP_pc, df = 4), family = Gamma(link = "log"), data = reserve_data)
lm_model_multiv <- lm(cbind(Access, Watch, Reserve) ~ ns(GDP_pc, df = 3), data = combined_dataXOX)
dependent_vars <- combined_dataXOX[, c("Access", "Watch", "Reserve")]
# Compute the correlation matrix
correlation_matrix <- cor(dependent_vars, use = "complete.obs")
print(correlation_matrix)
summary(lm_model_multiv)
summary_multiv <- summary(lm_model_multiv)
# Extract R^2 for each dependent variable
r_squared_access <- summary_multiv$"Response Access"$r.squared
r_squared_watch <- summary_multiv$"Response Watch"$r.squared
r_squared_reserve <- summary_multiv$"Response Reserve"$r.squared
# Print the R^2 values
cat("R^2 for Access:", r_squared_access, "\n")
cat("R^2 for Watch:", r_squared_watch, "\n")
cat("R^2 for Reserve:", r_squared_reserve, "\n")
# Extract Adjusted R^2 for each dependent variable
adjusted_r_squared_access <- summary_multiv$"Response Access"$adj.r.squared
adjusted_r_squared_watch <- summary_multiv$"Response Watch"$adj.r.squared
adjusted_r_squared_reserve <- summary_multiv$"Response Reserve"$adj.r.squared
# Print the Adjusted R^2 values
cat("Adjusted R^2 for Access:", adjusted_r_squared_access, "\n")
cat("Adjusted R^2 for Watch:", adjusted_r_squared_watch, "\n")
cat("Adjusted R^2 for Reserve:", adjusted_r_squared_reserve, "\n")
#MANOVA:
manova_model <- manova(cbind(Access, Watch, Reserve) ~ ns(GDP_pc, df = 3), data = combined_dataXOX)

# Summary of MANOVA
summary(manova_model)


# Extract and tidy model results
tidy_gam_a <- tidy(gam_model_a, conf.int = TRUE)
tidy_gam_w <- tidy(gam_model_w, conf.int = TRUE)
tidy_gam_r <- tidy(gam_model_r, conf.int = TRUE)
tidy_lm_multiv <- tidy(lm_model_multiv, conf.int = TRUE)
# Create an Excel workbook
wb <- createWorkbook()
# Add model results to the workbook
addWorksheet(wb, "Gamma Model Access")
writeData(wb, "Gamma Model Access", tidy_gam_a)
addWorksheet(wb, "Gamma Model Watch")
writeData(wb, "Gamma Model Watch", tidy_gam_w)
addWorksheet(wb, "Gamma Model Reserve")
writeData(wb, "Gamma Model Reserve", tidy_gam_r)
addWorksheet(wb, "Multivariate Linear Model")
writeData(wb, "Multivariate Linear Model", tidy_lm_multiv)
# Save the workbook
saveWorkbook(wb, "RegResults_PriceVSGDPpc.xlsx", overwrite = TRUE)


# Fit models with WB_incomeshort interaction; did not improve goodness-of-fit with WB_incomeshort
# Fit models with WHO_region interaction; did not improve goodness-of-fit with WB_incomeshort
#####
#Step 5: Regression results for %Access antibiotics; perhaps gamma or beta distribution#####
merged_data3_caware_wPERC <- merged_data3_caware_w %>%
  # filter(aware_category %in% c("Access", "Watch", "Reserve")) %>%
  group_by(country_iso3_code) %>%
  mutate(total_ddd_soldT = sum(total_ddd_sold, na.rm = TRUE)) %>%
  ungroup()
merged_data3_caware_wPERC <- merged_data3_caware_wPERC %>%
  group_by(country_iso3_code) %>%
  mutate(Percent_Access = if_else(aware_category == "Access", total_ddd_sold / total_ddd_soldT * 100, NA_real_)) %>%
  mutate(Percent_Watch = if_else(aware_category == "Watch", total_ddd_sold / total_ddd_soldT * 100, NA_real_)) %>%
  ungroup()
summary_dataACCESS <- merged_data3_caware_wPERC %>%
  group_by(country_iso3_code) %>%
  summarise(
    Percent_Access = mean(Percent_Access, na.rm = TRUE),
    Percent_Watch = mean(Percent_Watch, na.rm = TRUE)
  ) %>%
  arrange(Percent_Access) %>%
  filter(!is.na(Percent_Access))

merged_datapercAr <- left_join(summary_dataACCESS, access_data, by = "country_iso3_code")
merged_datapercAr$Percent_Access100<- merged_datapercAr$Percent_Access
merged_datapercAr$Percent_Access<- merged_datapercAr$Percent_Access/100

library(betareg)
k_values <- c(1, 2, 3, 4, 5, 6, 7, 8)
models <- lapply(k_values, function(k) {
  betareg(Percent_Access ~ ns(GDP_pc, df = k), data = merged_datapercAr)
})
# Extract and compare AIC to choose best goodness-of-fit
aic_values <- sapply(models, AIC)
names(aic_values) <- k_values
aic_values

beta_model_a <- betareg(Percent_Access ~ ns(GDP_pc, df = 6), data = merged_datapercAr)
summary(beta_model_a)
model_aic <- AIC(beta_model_a)
model_aic

lm_model_multiv <- lm(Percent_Access100 ~ ns(GDP_pc, df = 6), data = merged_datapercAr)
summary(lm_model_multiv)
model_aic <- AIC(lm_model_multiv)
print(model_aic)

tidy_beta_a <- tidy(beta_model_a, conf.int = TRUE)
tidy_lm_multiv <- tidy(lm_model_multiv, conf.int = TRUE)

# Create an Excel workbook
wb2 <- createWorkbook()
# Add model results to the workbook
addWorksheet(wb2, "Beta Model Access")
writeData(wb2, "Beta Model Access", tidy_beta_a)
addWorksheet(wb2, "Linear Model Percent Access")
writeData(wb2, "Linear Model Percent Access", tidy_lm_multiv)
# Save the workbook
saveWorkbook(wb2, "ResultsPercenAccVSGDPpc.xlsx", overwrite = TRUE)

#####
#Step 6: Predictions and graphs
#####Prediction beta model for percent access and GDP PC #####
merged_datapercAr$GDP_pcPPP[merged_datapercAr$country_iso3_code == "HRV"] <- 29000

beta_model_a <- betareg(Percent_Access ~ ns(GDP_pcPPP, df = 6), data = merged_datapercAr)

# Generate predictions on the link scale
eta <- predict(beta_model_a, newdata = merged_datapercAr, type = "link")

# Extract the model matrix for the new data
X_new <- model.matrix(~ ns(GDP_pcPPP, df = 6), data = merged_datapercAr)

# Extract the relevant part of the variance-covariance matrix (mean model only)
vcov_mean_model <- vcov(beta_model_a)[1:ncol(X_new), 1:ncol(X_new)]

# Verify dimensions
print(dim(X_new))
print(dim(vcov_mean_model))

# Calculate the standard errors of the linear predictors
se_eta <- sqrt(diag(X_new %*% vcov_mean_model %*% t(X_new)))

# Calculate the 95% confidence intervals on the link scale
fit <- plogis(eta)
lower <- plogis(eta - 1.96 * se_eta)
upper <- plogis(eta + 1.96 * se_eta)

# Add predictions and confidence intervals to the dataframe
merged_datapercAr$fit <- fit
merged_datapercAr$lower <- lower
merged_datapercAr$upper <- upper

# Plot the results
betaregplot<- ggplot(merged_datapercAr, aes(x = GDP_pcPPP, y = Percent_Access100)) +
  geom_point(color = "#0073C2FF") +  # Lancet-style point color
  geom_line(aes(y = fit*100), color = "#004676", size = 1) +  # Lancet-style line color
  geom_ribbon(aes(ymin = lower*100, ymax = upper*100), fill = "#56B4E9FF", alpha = 0.3) +  # Lancet-style ribbon color
  theme_minimal() +
  labs(
    title = "",
    x = "GDP per Capita (I$)",
    y = "Percentage of Access AMU"
  ) +
  theme(
    text = element_text(family = "Arial", color = "#333333"),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.text.x = element_text(size = 7),
    legend.position = "none"
  )+
  xlim(0, 120000)

ggsave("betaregplot.tiff", plot = betaregplot, device = "tiff", width =11, height = 7, dpi = 500, units = "in")

##### Predictions linear model for access proportions and GDP PC######

lm_model_multiv <- lm(Percent_Access100 ~ ns(GDP_pc, df = 6), data = merged_datapercAr)

# Generate predictions and confidence intervals
predictions <- predict(lm_model_multiv, newdata = merged_datapercAr, interval = "confidence")

# Add predictions and confidence intervals to the dataframe
merged_datapercAr$fit <- predictions[, "fit"]
merged_datapercAr$lower <- predictions[, "lwr"]
merged_datapercAr$upper <- predictions[, "upr"]

# Plot the results
ggplot(merged_datapercAr, aes(x = GDP_pc, y = Percent_Access100)) +
  geom_point(color = "#0073C2FF") +  # Lancet-style point color
  geom_line(aes(y = fit), color = "#004676", size = 1) +  # Lancet-style line color
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#56B4E9FF", alpha = 0.3) +  # Lancet-style ribbon color
  theme_minimal() +
  labs(
    title = "Linear Model with 95% CI",
    x = "GDP per Capita",
    y = "Percent Access (100%)"
  ) +
  theme(
    text = element_text(family = "Arial", color = "#333333"),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )




######-
#Step 7:GB2 models. Load income data fit model for income distributions and calculate what's percentage of the population that could pay for those antibiotics on a daily basis, also calculate for what % of population that would be cathastrophic (differente thersholds; 10%, 20%, 30%) and for which is OOP    ####
#######
###MODEL CDF Income#####
# Load necessary packages
library(readxl)
library(dplyr)
library(purrr)
library(fitdistrplus)
library(GB2)
library(GB2group)
#https://cran.r-project.org/web/packages/GB2group/GB2group.pdf

# Load the WIID data from the specific sheet
wiid_data <- readxl::read_excel("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_Projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/WIID_28NOV2023.xlsx", sheet = "data_wiib")

#DATA MANAGEMENT AND TEST DATA:
# Normalize income share to sum to 1
wiid_data <- wiid_data %>%
  group_by(country_iso3_code) %>%
  mutate(income_share = income_share / sum(income_share))

merged_data <- dplyr::left_join(wiid_data, data_2019, by = "country_iso3_code")
merged_data$gini <- as.numeric(merged_data$gini)
merged_data$gini <- merged_data$gini/100
merged_data <- dplyr::left_join(merged_data, ER_ppp_19WB, by = "country_iso3_code")
#PERHAPS WE SHOULD UPDATE THE BELOW TO DAILY GDP_pc, rather than an annual figure?
merged_data$GDP_pc <- merged_data$GDP_pc/365

merged_data <- merged_data %>%
  dplyr::select(country_iso3_code, income_share, group, GDP_pc, gini)


library(dplyr)
library(purrr)
library(GB2group)

# Define the price thresholds per country with additional thresholds
merged_data3_caware_w_w2<- merged_data3_caware_w_w
merged_data3_caware_w_r2 <- merged_data3_caware_w_r
colnames(merged_data3_caware_w_w2)[colnames(merged_data3_caware_w_w2) == "PriceperDDD_w"] <- "threshold2"
colnames(merged_data3_caware_w_r2)[colnames(merged_data3_caware_w_r2) == "PriceperDDD_w"] <- "threshold3"

threshold_per_day <- merged_data3_caware_w_a[, c("PriceperDDD_w", "country_iso3_code")]
colnames(threshold_per_day)[colnames(threshold_per_day) == "PriceperDDD_w"] <- "threshold"
threshold_per_day <- merge(threshold_per_day , merged_data3_caware_w_w2[, c("country_iso3_code", "threshold2")], 
                           by = "country_iso3_code", all.x = TRUE)
threshold_per_day <- merge(threshold_per_day , merged_data3_caware_w_r2[, c("country_iso3_code", "threshold3")], 
                           by = "country_iso3_code", all.x = TRUE)
threshold_per_day  <- threshold_per_day  %>% distinct(country_iso3_code, .keep_all = TRUE)


#UPDATE THESE VALUES ACCORDINGLY and  ADJUST (reverse) THEM to not PPP.
threshold_per_day <- merge(x= threshold_per_day,y=ppp_2019_iso, 
                           by="country_iso3_code", all.y = TRUE)
#threshold_per_day$threshold <- threshold_per_day$threshold * threshold_per_day$ppp.rate
#threshold_per_day$threshold2 <- threshold_per_day$threshold2 * threshold_per_day$ppp.rate
#threshold_per_day$threshold3 <- threshold_per_day$threshold3 * threshold_per_day$ppp.rate



# Define a function to fit the GB2 model
fit_gb2_model <- function(data) {
  fitgroup.gb2(y = data$income_share, gini.e = data$gini[1], pc.inc = data$GDP_pc[1])
}

# Define a function to calculate the CDF at a given threshold
calculate_cdf <- function(fit, threshold) {
  if (is.null(fit) || length(fit$omd.estimation) == 0) {
    return(NA)
  }
  tryCatch({
    params <- fit$omd.estimation["Coef.", ]
    shape1 <- params["a"]
    shape2 <- params["b"]
    shape3 <- params["p"]
    rate <- params["q"]
    cdf_value <- pgb2(threshold, shape1, shape2, shape3, rate)
    return(cdf_value)
  }, error = function(e) {
    warning(paste("Error calculating CDF for threshold:", threshold, ":", e$message))
    return(NA)
  })
}

merged_data <- merged_data %>%
  filter(!is.na(gini))
# Fit the GB2 model and calculate the CDF for each country
result <- merged_data %>%
  group_by(country_iso3_code) %>%
  summarize(gb2_fit = list(fit_gb2_model(cur_data())))

# Add the thresholds to the result
result <- result %>%
  left_join(threshold_per_day, by = "country_iso3_code")

# Calculate the percentage of the population below each threshold for threshold, threshold2, and threshold3
result <- result %>%
  mutate(
    # For threshold
    threshold_10pct = threshold * 10,
    threshold_20pct = threshold * 5,
    threshold_30pct = threshold * (10 / 3),
    threshold_40pct = threshold * 2.5,
    percentage_below_10pct = purrr::map2_dbl(gb2_fit, threshold_10pct, ~ calculate_cdf(.x, .y) * 100),
    percentage_below_20pct = purrr::map2_dbl(gb2_fit, threshold_20pct, ~ calculate_cdf(.x, .y) * 100),
    percentage_below_30pct = purrr::map2_dbl(gb2_fit, threshold_30pct, ~ calculate_cdf(.x, .y) * 100),
    percentage_below_40pct = purrr::map2_dbl(gb2_fit, threshold_40pct, ~ calculate_cdf(.x, .y) * 100),
    
    # For threshold2
    threshold2_10pct = threshold2 * 10,
    threshold2_20pct = threshold2 * 5,
    threshold2_30pct = threshold2 * (10 / 3),
    threshold2_40pct = threshold2 * 2.5,
    percentage_below2_10pct = purrr::map2_dbl(gb2_fit, threshold2_10pct, ~ calculate_cdf(.x, .y) * 100),
    percentage_below2_20pct = purrr::map2_dbl(gb2_fit, threshold2_20pct, ~ calculate_cdf(.x, .y) * 100),
    percentage_below2_30pct = purrr::map2_dbl(gb2_fit, threshold2_30pct, ~ calculate_cdf(.x, .y) * 100),
    percentage_below2_40pct = purrr::map2_dbl(gb2_fit, threshold2_40pct, ~ calculate_cdf(.x, .y) * 100),
    
    # For threshold3
    threshold3_10pct = threshold3 * 10,
    threshold3_20pct = threshold3 * 5,
    threshold3_30pct = threshold3 * (10 / 3),
    threshold3_40pct = threshold3 * 2.5,
    percentage_below3_10pct = purrr::map2_dbl(gb2_fit, threshold3_10pct, ~ calculate_cdf(.x, .y) * 100),
    percentage_below3_20pct = purrr::map2_dbl(gb2_fit, threshold3_20pct, ~ calculate_cdf(.x, .y) * 100),
    percentage_below3_30pct = purrr::map2_dbl(gb2_fit, threshold3_30pct, ~ calculate_cdf(.x, .y) * 100),
    percentage_below3_40pct = purrr::map2_dbl(gb2_fit, threshold3_40pct, ~ calculate_cdf(.x, .y) * 100)
  )

# Display the result
print(result)

#######
###GRAPHS and Tables for % a full day's income######
library(ggplot2)
library(dplyr)
library(tidyr)

# Prepare data for plotting
plot_data <- result %>%
  dplyr:: select(country_iso3_code, starts_with("percentage_below_")) %>%
  pivot_longer(cols = starts_with("percentage_below_"), 
               names_to = "threshold", 
               values_to = "percentage") %>%
  mutate(threshold = recode(threshold, 
                            "percentage_below_10pct" = "10%",
                            "percentage_below_20pct" = "20%",
                            "percentage_below_30pct" = "30%",
                            "percentage_below_40pct" = "40%"))

# Determine y-axis limits based on the data
y_max <- ceiling(max(plot_data$percentage, na.rm = TRUE))

# Plot the data
plot_data <- plot_data %>%
  group_by(country_iso3_code) %>%
  filter(any(threshold == "10%" & percentage > 1)) %>%
  arrange(desc(country_iso3_code))


threshold_widths <- c("10%" = 1, "20%" = 1, "30%" = 1, "40%" = 1)
# Convert thresholds to widths for plotting
plot_data$widths <- as.numeric(threshold_widths[plot_data$threshold])


# Reshape the data to wide format for Excel export
excel_data <- plot_data %>%
  pivot_wider(names_from = threshold, values_from = percentage)
# Save to Excel
write.xlsx(excel_data, file = "threshold_percentagesACCESS.xlsx")


merged_datahhhhj <- merge(plot_data, midas_price, by = "country_iso3_code")
# Select only the required columns
merged_datahhhhj <- merged_datahhhhj %>%
  dplyr::select(country_iso3_code, WHO_region, WB_income, percentage, threshold)

# Compute the statistics grouped by WHO_region and threshold
stats_by_who_region <- merged_datahhhhj %>%
  group_by(WHO_region, threshold) %>%
  summarise(
    median = median(percentage, na.rm = TRUE),
    p25 = quantile(percentage, 0.25, na.rm = TRUE),
    p75 = quantile(percentage, 0.75, na.rm = TRUE),
    IQR = IQR(percentage, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = threshold, values_from = c(median, p25, p75, IQR))
# Compute the statistics grouped by WB_income and threshold
stats_by_wb_income <- merged_datahhhhj %>%
  group_by(WB_income, threshold) %>%
  summarise(
    median = median(percentage, na.rm = TRUE),
    p25 = quantile(percentage, 0.25, na.rm = TRUE),
    p75 = quantile(percentage, 0.75, na.rm = TRUE),
    IQR = IQR(percentage, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = threshold, values_from = c(median, p25, p75, IQR))
# Combine the statistics into a single dataframe
combined_stats <- bind_rows(
  stats_by_who_region %>% mutate(Group = "WHO_region"),
  stats_by_wb_income %>% mutate(Group = "WB_income")
)
# Reorder columns for clarity
combined_stats <- combined_stats %>%
  dplyr::select(Group, everything())
# Print the combined statistics to view it
print(combined_stats)
# Export to Excel
write.xlsx(combined_stats, file = "CHE_OOPEperWHOregionWBincome_a.xlsx")
stats_by_all <- merged_datahhhhj %>%
  group_by(threshold) %>%
  summarise(
    median = median(percentage, na.rm = TRUE),
    p25 = quantile(percentage, 0.25, na.rm = TRUE),
    p75 = quantile(percentage, 0.75, na.rm = TRUE),
    IQR = IQR(percentage, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = threshold, values_from = c(median, p25, p75, IQR))








threshold111<-ggplot(plot_data, aes(x = factor(country_iso3_code, levels = unique(country_iso3_code)), y = percentage, fill = threshold, width = widths)) +
  geom_bar(stat = "identity", position = position_identity(), alpha = 0.8, color = "black") +
  coord_flip() +
  labs(title = "",
       x = "Country ISO-3 code",
       y = "Percentage of the population for whom the price of antibiotics represents 10%, 20%, 30%, or 40% of their daily income",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(angle = 0, hjust = 1, size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = c(0.95, 0.90),  # Adjust these values to position inside the plot
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "white", colour = "white"), # Optional: to add a border
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "white"),
    panel.grid.major.x = element_line(color="white", size=0.5),  # Only show grid line at x=0
    panel.grid.major.y = element_line(color="white", size=0.5)   # Only show grid line at y=0
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  scale_fill_manual(values = c(
    "10%" = "#e5f5e0",  # Very light green
    "20%" = "#a1d99b",  # Soft green
    "30%" = "#41ab5d",  # Medium green
    "40%" = "#006d2c"   # Dark green
  ))
# good blues: scale_fill_manual(values = c("10% Threshold" = "#c6dbef", 
#"20% Threshold" = "#6baed6", 
#"30% Threshold" = "#3182bd",
#"40% Threshold" = "#08519c"))




#THRESHOLD 222: Prepare data for plotting
plot_data <- result %>%
  dplyr:: select(country_iso3_code, starts_with("percentage_below2_")) %>%
  pivot_longer(cols = starts_with("percentage_below2_"), 
               names_to = "threshold2", 
               values_to = "percentage") %>%
  mutate(threshold2 = recode(threshold2, 
                             "percentage_below2_10pct" = "10%",
                             "percentage_below2_20pct" = "20%",
                             "percentage_below2_30pct" = "30%",
                             "percentage_below2_40pct" = "40%"))

# Plot the data
plot_data <- plot_data %>%
  group_by(country_iso3_code) %>%
  filter(any(threshold2 == "10%" & percentage > 1)) %>%
  arrange(desc(country_iso3_code))

threshold_widths <- c("10%" = 1, "20%" = 1, "30%" = 1, "40%" = 1)
# Convert thresholds to widths for plotting
plot_data$widths <- as.numeric(threshold_widths[plot_data$threshold2])

# Reshape the data to wide format for Excel export
excel_data <- plot_data %>%
  pivot_wider(names_from = threshold2, values_from = percentage)
# Save to Excel
write.xlsx(excel_data, file = "threshold_percentagesWATCH.xlsx")




merged_datahhhhj <- merge(plot_data, midas_price, by = "country_iso3_code")
# Select only the required columns
merged_datahhhhj <- merged_datahhhhj %>%
  dplyr::select(country_iso3_code, WHO_region, WB_income, percentage, threshold2)

# Compute the statistics grouped by WHO_region and threshold
stats_by_who_region <- merged_datahhhhj %>%
  group_by(WHO_region, threshold2) %>%
  summarise(
    median = median(percentage, na.rm = TRUE),
    p25 = quantile(percentage, 0.25, na.rm = TRUE),
    p75 = quantile(percentage, 0.75, na.rm = TRUE),
    IQR = IQR(percentage, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = threshold2, values_from = c(median, p25, p75, IQR))
# Compute the statistics grouped by WB_income and threshold
stats_by_wb_income <- merged_datahhhhj %>%
  group_by(WB_income, threshold2) %>%
  summarise(
    median = median(percentage, na.rm = TRUE),
    p25 = quantile(percentage, 0.25, na.rm = TRUE),
    p75 = quantile(percentage, 0.75, na.rm = TRUE),
    IQR = IQR(percentage, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = threshold2, values_from = c(median, p25, p75, IQR))
# Combine the statistics into a single dataframe
combined_stats <- bind_rows(
  stats_by_who_region %>% mutate(Group = "WHO_region"),
  stats_by_wb_income %>% mutate(Group = "WB_income")
)
# Reorder columns for clarity
combined_stats <- combined_stats %>%
  dplyr::select(Group, everything())
# Print the combined statistics to view it
print(combined_stats)
# Export to Excel
write.xlsx(combined_stats, file = "CHE_OOPEperWHOregionWBincome_w.xlsx")
stats_by_all <- merged_datahhhhj %>%
  group_by(threshold2) %>%
  summarise(
    median = median(percentage, na.rm = TRUE),
    p25 = quantile(percentage, 0.25, na.rm = TRUE),
    p75 = quantile(percentage, 0.75, na.rm = TRUE),
    IQR = IQR(percentage, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = threshold2, values_from = c(median, p25, p75, IQR))



















threshold222<- ggplot(plot_data, aes(x = factor(country_iso3_code, levels = unique(country_iso3_code)), y = percentage, fill = threshold2, width = widths)) +
  geom_bar(stat = "identity", position = position_identity(), alpha = 0.8, color = "black") +
  coord_flip() +
  labs(title = "",
       x = "Country ISO-3 code",
       y = "Percentage of the population for whom the price of antibiotics represents 10%, 20%, 30%, or 40% of their daily income",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(angle = 0, hjust = 1, size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = c(0.98, 0.90),  # Adjust these values to position inside the plot
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "white", colour = "white"), # Optional: to add a border
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "white"),
    panel.grid.major.x = element_line(color="white", size=0.5),  # Only show grid line at x=0
    panel.grid.major.y = element_line(color="white", size=0.5)   # Only show grid line at y=0
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  scale_fill_manual(values = c(
    "10%" = "#fff7bc",  # Very light yellow
    "20%" = "#fec44f",  # Golden yellow
    "30%" = "#fe9929",  # Amber
    "40%" = "#cc4c02"   # Deep orange (as the darkest yellow shade)
  ))



#THRESHOLD 333: Prepare data for plotting RESERVE ATBs

plot_data <- result %>%
  dplyr:: select(country_iso3_code, starts_with("percentage_below3_")) %>%
  pivot_longer(cols = starts_with("percentage_below3_"), 
               names_to = "threshold3", 
               values_to = "percentage") %>%
  mutate(threshold3 = recode(threshold3, 
                             "percentage_below3_10pct" = "10%",
                             "percentage_below3_20pct" = "20%",
                             "percentage_below3_30pct" = "30%",
                             "percentage_below3_40pct" = "40%"))

plot_data <- plot_data %>%
  group_by(country_iso3_code) %>%
  filter(any(threshold3 == "10%" & percentage > 1)) %>%
  arrange(desc(country_iso3_code))

threshold_widths <- c("10%" = 1, "20%" = 1, "30%" = 1, "40%" = 1)
# Convert thresholds to widths for plotting
plot_data$widths <- as.numeric(threshold_widths[plot_data$threshold3])


# Plot the data
threshold333 <- ggplot(plot_data, aes(x = factor(country_iso3_code, levels = unique(country_iso3_code)), y = percentage, fill = threshold3, width = widths)) +
  geom_bar(stat = "identity", position = position_identity(), alpha = 0.8, color = "black") +
  coord_flip() +
  labs(title = "",
       x = "",
       y = "Percentage of the population for whom the price of antibiotics constitutes at least 10%, 20%, 30%, or 40% of their daily income",
       fill = NULL) +  # Set fill to NULL to remove the label space
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(angle = 0, hjust = 1, size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = c(0.20, 0.88),  # Adjust these values to position inside the plot
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "white"), # Optional: to add a border
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "white"),
    panel.grid.major.x = element_line(color="white", size=0.5),  # Only show grid line at x=0
    panel.grid.major.y = element_line(color="white", size=0.5)   # Only show grid line at y=0
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  scale_fill_manual(values = c(
    "10%" = "#fee5d9",  # Very light pink
    "20%" = "#fcae91",  # Pale red
    "30%" = "#fb6a4a",  # Strong red
    "40%" = "#67000d"   # Dark maroon, almost black
  ), guide = guide_legend(title = NULL))  # Remove legend title space


library(gridExtra)
library(grid)

threshold222 <- threshold222 + theme(axis.title.x = element_blank()) +theme(axis.title.y = element_blank())
threshold111 <- threshold111 + theme(axis.title.x = element_blank())
threshold333 <- threshold333 + theme(axis.title.x = element_blank())
setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")

tiff("DayIncome_accessWatchReserv.tiff", width = 17, height = 10, units = "in", res = 500, compression = "lzw")
# Plot the combined plot
combined_plotxoxxx <- grid.arrange(
  grobs = list(threshold111, threshold222, threshold333),
  ncol = 3,
  bottom = textGrob("Percentage of the population for whom the price of antibiotics represents 10%, 20%, 30%, or 40% of their daily income", gp = gpar(fontsize = 14))
)

# Draw labels directly on the plot after arranging them
grid.text("A. Access", x = 0.04, y = 0.98, just = c("center", "center"), gp = gpar(col = "black", fontsize = 16, fontface = "bold"))
grid.text("B. Watch", x = 0.35, y = 0.98, just = c("center", "center"), gp = gpar(col = "black", fontsize = 16, fontface = "bold"))
grid.text("C. Reserve", x = 0.70, y = 0.98, just = c("center", "center"), gp = gpar(col = "black", fontsize = 16, fontface = "bold"))

# Close the TIFF device
dev.off()









merged_datahhhhj <- merge(plot_data, midas_price, by = "country_iso3_code")
# Select only the required columns
merged_datahhhhj <- merged_datahhhhj %>%
  dplyr::select(country_iso3_code, WHO_region, WB_income, percentage, threshold3)

# Compute the statistics grouped by WHO_region and threshold
stats_by_who_region <- merged_datahhhhj %>%
  group_by(WHO_region, threshold3) %>%
  summarise(
    median = median(percentage, na.rm = TRUE),
    p25 = quantile(percentage, 0.25, na.rm = TRUE),
    p75 = quantile(percentage, 0.75, na.rm = TRUE),
    IQR = IQR(percentage, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = threshold3, values_from = c(median, p25, p75, IQR))
# Compute the statistics grouped by WB_income and threshold
stats_by_wb_income <- merged_datahhhhj %>%
  group_by(WB_income, threshold3) %>%
  summarise(
    median = median(percentage, na.rm = TRUE),
    p25 = quantile(percentage, 0.25, na.rm = TRUE),
    p75 = quantile(percentage, 0.75, na.rm = TRUE),
    IQR = IQR(percentage, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = threshold3, values_from = c(median, p25, p75, IQR))
# Combine the statistics into a single dataframe
combined_stats <- bind_rows(
  stats_by_who_region %>% mutate(Group = "WHO_region"),
  stats_by_wb_income %>% mutate(Group = "WB_income")
)
# Reorder columns for clarity
combined_stats <- combined_stats %>%
  dplyr::select(Group, everything())
# Print the combined statistics to view it
print(combined_stats)
# Export to Excel
write.xlsx(combined_stats, file = "CHE_OOPEperWHOregionWBincome_re.xlsx")

#####



#####
#---------------------------------------------------------------------------------------------#
#PERCENTAGE of ACCESS/WATCH/ and ALL ATB consumption per Country graphs-----------------------#
#---------------------------------------------------------------------------------------------#
merged_data3_caware_w$aware_category[merged_data3_caware_w$aware_category == ""] <- "Unclassified"
merged_data3_caware_wPERC$aware_category[merged_data3_caware_wPERC$aware_category == ""] <- "Unclassified"
#% access atbs in the country, compared to all aware ATBs being sold (%) #####
merged_data3_caware_wPERC <- merged_data3_caware_w %>%
 # filter(aware_category %in% c("Access", "Watch", "Reserve")) %>%
  group_by(country_iso3_code) %>%
  mutate(total_ddd_soldT = sum(total_ddd_sold, na.rm = TRUE)) %>%
  ungroup()
merged_data3_caware_wPERC <- merged_data3_caware_wPERC %>%
  group_by(country_iso3_code) %>%
  mutate(Percent_Access = if_else(aware_category == "Access", total_ddd_sold / total_ddd_soldT * 100, NA_real_)) %>%
  mutate(Percent_Watch = if_else(aware_category == "Watch", total_ddd_sold / total_ddd_soldT * 100, NA_real_)) %>%
  mutate(Percent_Reserve = if_else(aware_category == "Reserve", total_ddd_sold / total_ddd_soldT * 100, NA_real_)) %>%

  ungroup()
summary_data <- merged_data3_caware_wPERC %>%
  group_by(country_iso3_code) %>%
  summarise(
    Percent_Access = mean(Percent_Access, na.rm = TRUE),
    Percent_Watch = mean(Percent_Watch, na.rm = TRUE),
    Percent_Reserve = mean(Percent_Reserve, na.rm = TRUE)
  ) %>%
  arrange(Percent_Access) %>%
  filter(!is.na(Percent_Access))
library(ggplot2)
summary_dataKOKK<- summary_data  
#####
#Access graph######
summary_data$country_iso3_code <- factor(summary_data$country_iso3_code, levels = summary_data$country_iso3_code[order(summary_data$Percent_Access)])
plotxol <- ggplot(summary_data, aes(x = country_iso3_code, y = Percent_Access)) +
  geom_bar(stat = "identity", fill = "#7fc97f", color = "black", width = 0.7) +  # Green bars with black contour
  geom_rect(data = summary_data, aes(xmin = as.numeric(factor(country_iso3_code)) - 0.35, xmax = as.numeric(factor(country_iso3_code)) + 0.35,
                                     ymin = Percent_Access, ymax = 70), inherit.aes = FALSE, fill = "#b2e2bd", alpha = 0.5) +
  #geom_hline(aes(yintercept = 80, linetype = "80% Access target"), color = "red", size = 1.5) +
  geom_hline(aes(yintercept = 70, linetype = "70% Access target"), color = "red", size = 1.5) +
  scale_linetype_manual(name = "Line Type", values = c("70% Access target" = "dashed"), guide = guide_legend(title = "")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  labs(x = "Country", y = "Percentage of Access antibiotics sold (%)", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8, color = "black"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    legend.position = c(0.01, 0.99),  # Position inside the plot area, top left
    legend.justification = c(0, 1),  # Anchor point of the legend
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.margin = margin(10, 10, 10, 10)  # Margin around the legend
  )
setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("plot_PercentageAccess.tiff", plot = plotxol, device = "tiff", width =10, height = 6, dpi = 500, units = "in")
getwd()
######
#WATCH graph######
summary_data$country_iso3_code <- factor(summary_data$country_iso3_code, levels = summary_data$country_iso3_code[order(summary_data$Percent_Watch)])
plotxol2 <- ggplot(summary_data, aes(x = country_iso3_code, y = Percent_Watch)) +
  geom_bar(stat = "identity", fill = "yellow", color = "black", width = 0.7) +  # Yellow bars with black contour
  geom_hline(aes(yintercept = 20, linetype = "20% non-Access target"), color = "red", size = 1.0) +
  scale_linetype_manual(
    name = " ",  # Empty string for the legend title
    values = c("20% non-Access target" = "dashed"),  # Define linetype for the specific label
    guide = guide_legend(title = " ")  # Ensure the legend title is shown as an empty string
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  labs(x = "Country", y = "Percentage of Watch antibiotics sold (%)", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8, color = "black"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    legend.position = c(0.01, 0.99),  # Position inside the plot area, top left
    legend.justification = c(0, 1),  # Anchor point of the legend
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.margin = margin(10, 10, 10, 10)  # Margin around the legend
  )
setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("plot_PercentageWatch.tiff", plot = plotxol2, device = "tiff", width =10, height = 6, dpi = 500, units = "in")
getwd()
######
#All aware_categories: ######
merged_data3_caware_wPERC <- merged_data3_caware_wPERC %>%
  group_by(country_iso3_code, aware_category) %>%
  summarise(total_ddd_sold = sum(total_ddd_sold), total_ddd_soldT = sum(total_ddd_soldT)) %>%
  ungroup() %>%
  mutate(Percent = total_ddd_sold / total_ddd_soldT * 100)

# Summarize the data
summary_data <- merged_data3_caware_wPERC %>%
  group_by(country_iso3_code, aware_category) %>%
  summarise(Percent = sum(Percent, na.rm = TRUE)) %>%
  ungroup()

# Set the factor levels for aware_category
summary_data$aware_category <- factor(summary_data$aware_category, levels = c("Access", "Watch", "Reserve", "Not Recommended", "Unclassified"))

# Order data by percentage of Access antibiotics
summary_data <- summary_data %>%
  spread(key = aware_category, value = Percent, fill = 0) %>%
  arrange(Access) %>%
  gather(key = aware_category, value = Percent, -country_iso3_code) %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = unique(country_iso3_code)))



library(dplyr)
library(tidyr)



weighted_avg_dataMIDAS_prCAw2<- midas_price %>%
  group_by(CAw2) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), derived_aware_category = first(derived_aware_category), country_iso3_code= first(country_iso3_code), WHO_region= first(WHO_region), WB_income=first(WB_income), WB_incomeshort= first(WB_incomeshort), na.rm = TRUE)
weighted_avg_dataMIDAS_prCAw2$aware_category <- weighted_avg_dataMIDAS_prCAw2$derived_aware_category 



# Step 1: Remove empty aware_category and recode as "Unclassified" if needed
merged_data3_caware_wPERC_2<- weighted_avg_dataMIDAS_prCAw
merged_data3_caware_wPERC_2 <- merged_data3_caware_wPERC_2 %>%
  mutate(aware_category = ifelse(aware_category == "", "Unclassified", aware_category))

# Step 2: Filter only the relevant categories
valid_categories <- c("Access", "Watch", "Reserve", "Not Recommended", "Unclassified")
merged_data3_caware_wPERC_2 <- merged_data3_caware_wPERC_2 %>%
  filter(aware_category %in% valid_categories)

# Step 3: Calculate total DDD per country
summary_data <- merged_data3_caware_wPERC_2 %>%
  group_by(country_iso3_code, aware_category) %>%
  summarise(total_ddd = sum(total_ddd_sold, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(country_iso3_code) %>%
  mutate(Percent = total_ddd / sum(total_ddd) * 100) %>%
  ungroup()

# Step 4: Set factor order for aware_category
summary_data$aware_category <- factor(summary_data$aware_category,
                                      levels = c("Access", "Watch", "Reserve", "Not Recommended", "Unclassified"))

# Step 5: Order countries by Access percentage
summary_data_ordered <- summary_data %>%
  filter(aware_category == "Access") %>%
  arrange(Percent) %>%
  pull(country_iso3_code)

summary_data <- summary_data %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = summary_data_ordered))



# Define colors for each aware_category in the specified order
category_colors <- c(
  "Access" = "#7fc97f",
  "Watch" = "#ffff99",
  "Reserve" = "red",
  "Not Recommended" = "black",
  "Unclassified" = "gray"
)

# Create the plot
plotxolxxx <- ggplot(summary_data, aes(x = country_iso3_code, y = Percent, fill = aware_category)) +
  geom_bar(stat = "identity", color = "black", width = 0.7, position = position_stack(reverse = TRUE)) +
  geom_hline(aes(yintercept = 80, linetype = "80% AMU"), color = "red", size = 1.0) +
  geom_hline(aes(yintercept = 70, linetype = "70% AMU"), color = "maroon", size = 1.0) +
  geom_hline(aes(yintercept = 60, linetype = "60% AMU"), color = "#FA8072", size = 1.0) +
  scale_fill_manual(values = category_colors, name = "AWaRe category") +
  scale_linetype_manual(
    name = " ",  # Empty string for the legend title
    values = c("80% AMU" = "dashed", "70% AMU" = "dashed", "60% AMU" = "dashed"),  # Define linetype for the specific label
    guide = guide_legend(title = " ")  # Ensure the legend title is shown as an empty string
  ) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, by = 10)) +
  labs(x = "", y = "Percentage of \nantibiotics sold (%)", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    legend.position = "top",  # Position legend outside the plot area at the top
    legend.justification = "center",
    legend.direction = "horizontal",  # Arrange legend items in a single row
    legend.box = "horizontal",  # Arrange the legend box horizontally
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.margin = margin(1, 1, 1, 1)  # Margin around the legend
  )

# Print the plot
print(plotxolxxx)

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("plot_PercentageAllAware.tiff", plot = plotxolxxx, device = "tiff", width =10, height = 6, dpi = 500, units = "in")
getwd()

######
#STATS: ######
summary_dataKOKKmd <- merge(summary_dataKOKK, midas_price, by = "country_iso3_code")
summary_dataKOKKmd <- summary_dataKOKKmd %>%
  dplyr::select(country_iso3_code, Percent_Access, Percent_Watch, WHO_region, WB_income)

# Compute the statistics grouped by WHO_region
stats_by_who_region <- summary_dataKOKKmd %>%
  group_by(WHO_region) %>%
  summarise(
    median = median(Percent_Access, na.rm = TRUE),
    p25 = quantile(Percent_Access, 0.25, na.rm = TRUE),
    p75 = quantile(Percent_Access, 0.75, na.rm = TRUE),
    IQR = IQR(Percent_Access, na.rm = TRUE)
  ) %>%
  mutate(Group = "WHO_region")
# Compute the statistics grouped by WB_income
stats_by_wb_income <- summary_dataKOKKmd %>%
  group_by(WB_income) %>%
  summarise(
    median = median(Percent_Access, na.rm = TRUE),
    p25 = quantile(Percent_Access, 0.25, na.rm = TRUE),
    p75 = quantile(Percent_Access, 0.75, na.rm = TRUE),
    IQR = IQR(Percent_Access, na.rm = TRUE)
  ) %>%
  mutate(Group = "WB_income")
# Combine the statistics into a single dataframe
combined_stats <- bind_rows(stats_by_who_region, stats_by_wb_income)
# Reorder columns for clarity
combined_stats <- combined_stats %>%
  dplyr::select(Group, everything())
# Export to Excel
write.xlsx(combined_stats, file = "summary_dataPercentAccess_stats.xlsx")


# Compute the statistics grouped by WHO_region
stats_by_who_region <- summary_dataKOKKmd %>%
  group_by(WHO_region) %>%
  summarise(
    median = median(Percent_Watch, na.rm = TRUE),
    p25 = quantile(Percent_Watch, 0.25, na.rm = TRUE),
    p75 = quantile(Percent_Watch, 0.75, na.rm = TRUE),
    IQR = IQR(Percent_Watch, na.rm = TRUE)
  ) %>%
  mutate(Group = "WHO_region")
# Compute the statistics grouped by WB_income
stats_by_wb_income <- summary_dataKOKKmd %>%
  group_by(WB_income) %>%
  summarise(
    median = median(Percent_Watch, na.rm = TRUE),
    p25 = quantile(Percent_Watch, 0.25, na.rm = TRUE),
    p75 = quantile(Percent_Watch, 0.75, na.rm = TRUE),
    IQR = IQR(Percent_Watch, na.rm = TRUE)
  ) %>%
  mutate(Group = "WB_income")
# Combine the statistics into a single dataframe
combined_stats <- bind_rows(stats_by_who_region, stats_by_wb_income)
# Reorder columns for clarity
combined_stats <- combined_stats %>%
  dplyr::select(Group, everything())
# Export to Excel
write.xlsx(combined_stats, file = "summary_dataPercentWatch_stats.xlsx")


summary_data2xkk <- merge(summary_data,summary_dataKOKKmd , by = "country_iso3_code")
summary_data2xkk <- summary_data2xkk[!duplicated(summary_data2xkk$country_iso3_code), ]
stats_by_wb_income2 <- summary_data2xkk %>%
  group_by(WB_income) %>%
  summarise(
    median = median(Percent_Reserve, na.rm = TRUE),
    p25 = quantile(Percent_Reserve, 0.25, na.rm = TRUE),
    p75 = quantile(Percent_Reserve, 0.75, na.rm = TRUE),
    IQR = IQR(Percent_Reserve, na.rm = TRUE)
  ) %>%
  mutate(Group = "WB_income")
stats_by_who_region2 <- summary_data2xkk %>%
  group_by(WHO_region) %>%
  summarise(
    median = median(Percent_Reserve, na.rm = TRUE),
    p25 = quantile(Percent_Reserve, 0.25, na.rm = TRUE),
    p75 = quantile(Percent_Reserve, 0.75, na.rm = TRUE),
    IQR = IQR(Percent_Reserve, na.rm = TRUE)
  ) %>%
  mutate(Group = "WHO_region")

stats_by_stats2s <- summary_data2xkk %>%
  summarise(
    median = median(Percent_Reserve, na.rm = TRUE),
    p25 = quantile(Percent_Reserve, 0.25, na.rm = TRUE),
    p75 = quantile(Percent_Reserve, 0.75, na.rm = TRUE),
    IQR = IQR(Percent_Reserve, na.rm = TRUE)
  ) 


#####
#---------------------------------------------------------------------------------------------#
#GRAPHS WITH COST SAVINGS per capita per country, and ALL if access atb consumption=70%.      #
#---------------------------------------------------------------------------------------------#

#RUN ACCESS GRAPH ABOVE AND % ACCESS atbs FIRST!
#GRAPH with cost savings graph with costs per capita ###### 

summary_data2 <- merged_data3_caware_wPERC %>%
  mutate(
    diff_price = median_price_ww - median_price_wa,
    gap_to_80 = (80 - Percent_Access)/100,
    net_potential_savings = (total_ddd_soldT * gap_to_80)*diff_price
  )
summary_data2 <- summary_data2 %>%
  filter(!is.na(Percent_Access))

# Group by both country_iso3_code and who_regional_office_code, then summarize
#summary_data2 <- summary_data2 %>%
#  group_by(country_iso3_code, WHO_region) %>%
#  summarize(net_potential_savings = sum(net_potential_savings, na.rm = TRUE), .groups = 'drop')

summary_data2$net_potential_savings<- summary_data2$net_potential_savings/1000000000

summary_data2 <- summary_data2 %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = summary_data2$country_iso3_code[order(net_potential_savings)]))

summary_data2 <- summary_data2 %>%
  left_join(data_2019 %>% dplyr::select(country_iso3_code, population), by = "country_iso3_code")

summary_data2 <- summary_data2  %>%
  mutate(population = if_else(country_iso3_code == "TWN", 23600000, population))
summary_data2  <- summary_data2  %>%
  mutate(population = if_else(country_iso3_code == "ARE", 9212000, population))


summary_data2$net_potential_savingsPC <- ((summary_data2$net_potential_savings*1000000000)/ summary_data2$population)

summary_data2 <- summary_data2 %>%
  mutate(net_potential_savingsPC = ifelse(country_iso3_code %in% c("ECU"), 150, net_potential_savingsPC))

summary_data2 <- summary_data2 %>%
  arrange(net_potential_savingsPC)
# Convert country_iso3_code to a factor with levels ordered by net_potential_savingsPC
summary_data2$country_iso3_code <- factor(summary_data2$country_iso3_code, levels = summary_data2$country_iso3_code)


# Create the plot
plotkokk <- ggplot(summary_data2, aes(x = country_iso3_code, y = net_potential_savingsPC, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 15, by = 2)) +  # Set y-axis breaks
  scale_fill_manual(values = c("AFR" = "#E41A1C", "AMR" = "#377EB8", "EMR" = "#984EA3", "EUR" = "#4DAF4A", "WPR" = "#FFFF33", "SEAR" = "#FF7F00"),  # Customize your color palette
                    guide = guide_legend(title = "WHO region")) +
  labs(x = "Country", y = "Net potential savings \n(per capita, $int)", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    legend.position = c(0.15, 0.95),  # Coordinates for top right inside the plot area
    legend.justification = c(1, 1),  # Justifies the legend box within its area
    legend.background = element_blank(),  # Transparent background with a border
    legend.box.background = element_blank(),  # No additional background around the items
    legend.margin = margin(5, 5, 5, 5)  # Margin around the legend
  ) +
  geom_text(data = subset(summary_data2, country_iso3_code %in% c("ECU")),
            aes(label = "*", y = net_potential_savingsPC + 1), size = 10, color = "black")  # Add asterisk


# Print the plot
print(plotkokk)


######
#######GRAPH WITH ALL Cost-savings (not per capita; raw) ######
summary_data2 <- merged_data3_caware_wPERC %>%
  mutate(
    diff_price = median_price_ww - median_price_wa,
    gap_to_80 = (80 - Percent_Access)/100,
    net_potential_savings = (total_ddd_soldT * gap_to_80)*diff_price
  )
summary_data2 <- summary_data2 %>%
  filter(!is.na(Percent_Access))

# Group by both country_iso3_code and who_regional_office_code, then summarize
#summary_data2 <- summary_data2 %>%
#  group_by(country_iso3_code, WHO_region) %>%
#  summarize(net_potential_savings = sum(net_potential_savings, na.rm = TRUE), .groups = 'drop')

summary_data2$net_potential_savings<- summary_data2$net_potential_savings/1000000000
summary_data2 <- summary_data2 %>%
  arrange(net_potential_savings)
summary_data2 <- summary_data2 %>%
  mutate(net_potential_savings = ifelse(country_iso3_code %in% c("CHN", "IND"), 25, net_potential_savings))

# Convert country_iso3_code to a factor with levels ordered by net_potential_savingsPC
summary_data2$country_iso3_code <- factor(summary_data2$country_iso3_code, levels = summary_data2$country_iso3_code)
# Create the plot
plotkokk21 <- ggplot(summary_data2, aes(x = country_iso3_code, y = net_potential_savings, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_y_continuous(limits = c(0, 28), breaks = seq(0,25, by = 5)) +  # Set y-axis breaks
  scale_fill_manual(values = c("AFR" = "#E41A1C", "AMR" = "#377EB8", "EMR" = "#984EA3", "EUR" = "#4DAF4A", "WPR" = "#FFFF33", "SEAR" = "#FF7F00"),  # Customize your color palette
                    guide = guide_legend(title = "WHO region")) +
  labs(x = "Country", y = "Net potential savings \n(in billions, $int)", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    legend.position = c(0.1, 0.95),  # Coordinates for top right inside the plot area
    legend.justification = c(1, 1),  # Justifies the legend box within its area
    legend.background = element_blank(),  # Transparent background with a border
    legend.box.background = element_blank(),  # No additional background around the items
    legend.margin = margin(5, 5, 5, 5)  # Margin around the legend
  ) +
  geom_text(data = subset(summary_data2, country_iso3_code %in% c("CHN", "IND")),
            aes(label = "*", y = net_potential_savings + 2), size = 7, color = "black") + # Add asterisk
  geom_text(data = subset(summary_data2, country_iso3_code == "SVN"),
            aes(label = "$0.02", y = net_potential_savings + 3), 
            size = 4, color = "black", angle = 90, vjust = 0.5, hjust = 0.5) + # Add asterisk
  geom_text(data = subset(summary_data2, country_iso3_code == "SGP"),
            aes(label = "$0.14", y = net_potential_savings + 3), 
            size = 4, color = "black", angle = 90, vjust = 0.5, hjust = 0.5) + # Add asterisk
  geom_text(data = subset(summary_data2, country_iso3_code == "DZA"),
            aes(label = "$0.24", y = net_potential_savings + 3), 
            size = 4, color = "black", angle = 90, vjust = 0.5, hjust = 0.5) + # Add asterisk
  geom_text(data = subset(summary_data2, country_iso3_code == "CHL"),
            aes(label = "$0.45", y = net_potential_savings + 3), 
            size = 4, color = "black", angle = 90, vjust = 0.5, hjust = 0.5) + # Add asterisk
  geom_text(data = subset(summary_data2, country_iso3_code == "BEL"),
            aes(label = "$0.60", y = net_potential_savings + 3), 
            size = 4, color = "black", angle = 90, vjust = 0.5, hjust = 0.5)+ # Add asterisk
  geom_text(data = subset(summary_data2, country_iso3_code == "FRA"),
            aes(label = "$1.58", y = net_potential_savings + 3), 
            size = 4, color = "black", angle = 90, vjust = 0.5, hjust = 0.5)+ # Add asterisk
  geom_text(data = subset(summary_data2, country_iso3_code == "BRA"),
            aes(label = "$4.46", y = net_potential_savings + 3), 
            size = 4, color = "black", angle = 90, vjust = 0.5, hjust = 0.5)+ # Add asterisk
  geom_text(data = subset(summary_data2, country_iso3_code == "BGD"),
            aes(label = "$12.71", y = net_potential_savings + 3), 
            size = 4, color = "black", angle = 90, vjust = 0.5, hjust = 0.5)

# Print the plot
print(plotkokk21)
ggsave("plot_access%_costssavingsTOTAL.tiff", plot = plotkokk21, device = "tiff", width =11, height = 7, dpi = 500, units = "in")
getwd()
#######
### FIGURE 4. Graph with cost savings per capita adjusted to each country's prices:#####
merged_data3_caware_w_a$PriceperDDD_wA<- merged_data3_caware_w_a$PriceperDDD_w
merged_data3_caware_w_w$PriceperDDD_wW<- merged_data3_caware_w_w$PriceperDDD_w
merged_dataLOK <- merge(merged_data3_caware_w_a, merged_data3_caware_w_w, by = "country_iso3_code")
merged_dataLOK <- merged_dataLOK %>%
  dplyr::select(country_iso3_code, PriceperDDD_wA, PriceperDDD_wW)
merged_data3_caware_wPERC <- merge(merged_data3_caware_wPERC, merged_dataLOK, by = "country_iso3_code")
merged_data3_caware_wPERC$PriceperDDD_wW <- merged_data3_caware_wPERC$PriceperDDD_wW

summary_data2 <- merged_data3_caware_wPERC %>%
  mutate(
    diff_price = PriceperDDD_wW - PriceperDDD_wA,
    gap_to_80 = (80 - Percent_Access)/100,
    net_potential_savings = (total_ddd_soldT * gap_to_80)*diff_price
  )
summary_data2 <- summary_data2 %>%
  filter(!is.na(Percent_Access))

# Group by both country_iso3_code and who_regional_office_code, then summarize
#summary_data2 <- summary_data2 %>%
#  group_by(country_iso3_code) %>%
#  summarize(net_potential_savings = sum(net_potential_savings, na.rm = TRUE), WHO_region= first(WHO_region),.groups = 'drop')

summary_data2$net_potential_savings<- summary_data2$net_potential_savings/1000000000

summary_data2 <- summary_data2 %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = summary_data2$country_iso3_code[order(net_potential_savings)]))

summary_data2 <- summary_data2 %>%
  left_join(data_2019 %>% dplyr::select(country_iso3_code, population), by = "country_iso3_code")

summary_data2 <- summary_data2  %>%
  mutate(population = if_else(country_iso3_code == "TWN", 23600000, population))
summary_data2  <- summary_data2  %>%
  mutate(population = if_else(country_iso3_code == "ARE", 9212000, population))


summary_data2$net_potential_savingsPC <- ((summary_data2$net_potential_savings*1000000000)/ summary_data2$population)

#summary_data2 <- summary_data2 %>%
#  mutate(net_potential_savingsPC = ifelse(country_iso3_code %in% c("ECU"), 500, net_potential_savingsPC))

summary_data2 <- summary_data2 %>%
  arrange(net_potential_savingsPC)
# Convert country_iso3_code to a factor with levels ordered by net_potential_savingsPC
summary_data2$country_iso3_code <- factor(summary_data2$country_iso3_code, levels = summary_data2$country_iso3_code)

summary_data2 <- summary_data2 %>%
  filter(net_potential_savingsPC > 0 & !is.na(net_potential_savingsPC))


summary_data2586<- summary_data2  
# Create the plot
#summary_data2 <- summary_data2  %>%
#  mutate(net_potential_savingsPC = if_else(country_iso3_code == "HRV", 8.5, net_potential_savingsPC))
plotkokk2 <- ggplot(summary_data2, aes(x = country_iso3_code, y = net_potential_savingsPC, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_y_continuous(breaks = seq(0, 9, by = 0.5)) +  # Set y-axis breaks
  scale_fill_manual(values = c("AFR" = "#E41A1C", "AMR" = "#377EB8", "EMR" = "#984EA3", "EUR" = "#4DAF4A", "WPR" = "#FFFF33", "SEAR" = "#FF7F00"),  # Customize your color palette
                    guide = guide_legend(title = "WHO region")) +
  labs(x = "Country", y = "Net potential savings \n(per capita, I$)", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    legend.position = c(0.15, 0.95),  # Coordinates for top right inside the plot area
    legend.justification = c(1, 1),  # Justifies the legend box within its area
    legend.background = element_blank(),  # Transparent background with a border
    legend.box.background = element_blank(),  # No additional background around the items
    legend.margin = margin(5, 5, 5, 5))
# Margin around the legend


combined_plotxoli <- plotxolxxx / plotkokk2222   +
  theme(plot.margin = margin(0, 0, 0, 0))
# Add labels A and B
finalizima <- combined_plotxoli + 
  plot_annotation(tag_levels = 'A',tag_suffix = '.') & 
  theme(plot.margin = margin(0, 0, 0, 0), plot.tag = element_text( face = "bold"))
ggsave("Figure4.tiff", plot = finalizima, device = "tiff", width =11, height = 10, dpi = 500, units = "in")
getwd()
ggsave("Figure4.pdf", plot = finalizima, width = 11, height = 10, dpi = 500, units = "in")
getwd()

#  geom_text(data = subset(summary_data2, country_iso3_code %in% c("ECU")),
#            aes(label = "*", y = net_potential_savingsPC + 5), size = 10, color = "black")  # Add asterisk


# Print the plot
print(plotkokk2)
ggsave("plot_costsavingsAdjToRealCountryDiffpricL.tiff", plot = plotkokk2, device = "tiff", width =11, height = 7, dpi = 500, units = "in")


















#######
#####***Graph_BEcareful and run the above first but then this one, would get us an error!h######
summary_data2 <- merged_data3_caware_wPERC %>%
  mutate(
    diff_price = PriceperDDD_wW - PriceperDDD_wA,
    gap_to_80 = (70 - Percent_Access)/100,
    net_potential_savings = (total_ddd_soldT * gap_to_80)*diff_price
  )
summary_data2 <- summary_data2 %>%
  filter(!is.na(Percent_Access))

# Group by both country_iso3_code and who_regional_office_code, then summarize
#summary_data2 <- summary_data2 %>%
#  group_by(country_iso3_code) %>%
#  summarize(net_potential_savings = sum(net_potential_savings, na.rm = TRUE), WHO_region= first(WHO_region),.groups = 'drop')

summary_data2$net_potential_savings<- summary_data2$net_potential_savings/1000000000

summary_data2 <- summary_data2 %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = summary_data2$country_iso3_code[order(net_potential_savings)]))

summary_data2 <- summary_data2 %>%
  left_join(data_2019 %>% dplyr::select(country_iso3_code, population), by = "country_iso3_code")

summary_data2 <- summary_data2  %>%
  mutate(population = if_else(country_iso3_code == "TWN", 23600000, population))
summary_data2  <- summary_data2  %>%
  mutate(population = if_else(country_iso3_code == "ARE", 9212000, population))


summary_data2$net_potential_savingsPC <- ((summary_data2$net_potential_savings*1000000000)/ summary_data2$population)

#summary_data2 <- summary_data2 %>%
#  mutate(net_potential_savingsPC = ifelse(country_iso3_code %in% c("ECU"), 500, net_potential_savingsPC))

summary_data2 <- summary_data2 %>%
  arrange(net_potential_savingsPC)
# Convert country_iso3_code to a factor with levels ordered by net_potential_savingsPC
summary_data2$country_iso3_code <- factor(summary_data2$country_iso3_code, levels = summary_data2$country_iso3_code)

summary_data2 <- summary_data2 %>%
  filter(net_potential_savingsPC > 0 & !is.na(net_potential_savingsPC))


summary_data2586<- summary_data2  
# Create the plot
#summary_data2 <- summary_data2  %>%
#  mutate(net_potential_savingsPC = if_else(country_iso3_code == "HRV", 6, net_potential_savingsPC))

summary_data2 <- summary_data2 %>%
  arrange(net_potential_savingsPC) %>%
  mutate(country_iso3_code = factor(country_iso3_code, levels = unique(country_iso3_code)))

plotkokk2222 <- ggplot(summary_data2, aes(x = country_iso3_code, y = net_potential_savingsPC, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_y_continuous(breaks = seq(0, 6.5, by = 0.5)) +  # Set y-axis breaks
  scale_fill_manual(values = c("AFR" = "#E41A1C", "AMR" = "#377EB8", "EMR" = "#984EA3", "EUR" = "#4DAF4A", "WPR" = "#FFFF33", "SEAR" = "#FF7F00"),  # Customize your color palette
                    guide = guide_legend(title = "WHO region")) +
  labs(x = "Country", y = "Net potential savings \n(per capita, I$)", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    legend.position = c(0.15, 0.95),  # Coordinates for top right inside the plot area
    legend.justification = c(1, 1),  # Justifies the legend box within its area
    legend.background = element_blank(),  # Transparent background with a border
    legend.box.background = element_blank(),  # No additional background around the items
    legend.margin = margin(5, 5, 5, 5)) 
#annotate("text", x = "HRV", y = 6.2, label = "+", size = 6, color = "black") 

ggsave("plot_costsavingsAdj_70percentTargL.tiff", plot = plotkokk2222, device = "tiff", width =9, height = 7, dpi = 500, units = "in")

#####
######
#######GRAPH WITH ALL Cost-savings (not per capita; raw REAL PRICES from COUntriES) ######

summary_data2 <- summary_data2 %>%
  arrange(net_potential_savings)

# Convert country_iso3_code to a factor with levels ordered by net_potential_savingsPC
summary_data2$country_iso3_code <- factor(summary_data2$country_iso3_code, levels = summary_data2$country_iso3_code)
# Create the plot
plotkokk212 <- ggplot(summary_data2, aes(x = country_iso3_code, y = net_potential_savings, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_manual(values = c("AFR" = "#E41A1C", "AMR" = "#377EB8", "EMR" = "#984EA3", "EUR" = "#4DAF4A", "WPR" = "#FFFF33", "SEAR" = "#FF7F00"),  # Customize your color palette
                    guide = guide_legend(title = "WHO region")) +
  labs(x = "Country", y = "Net potential savings \n(in millions, $int)", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    legend.position = c(0.1, 0.95),  # Coordinates for top right inside the plot area
    legend.justification = c(1, 1),  # Justifies the legend box within its area
    legend.background = element_blank(),  # Transparent background with a border
    legend.box.background = element_blank(),  # No additional background around the items
    legend.margin = margin(5, 5, 5, 5)  # Margin around the legend
  ) 

# Print the plot
print(plotkokk212)
ggsave("plot_access%_costssavingsTOTAL_realC.tiff", plot = plotkokk212, device = "tiff", width =11, height = 7, dpi = 500, units = "in")
getwd()

######
#####Calculating the percentage of savings regarding some countries 'with information' on pharmaceutical spending 2019 ####
summary_data2586<- summary_data2  
pharma_spending <- data.frame(
  Current_prices2019usd = c(1280.764, 937.364, 857.932, 854.49, 834.842, 744.799, 705.604, 703.764, 694.205, 653.863, 
                            644.493, 643.42, 629.029, 611.237, 604.747, 596.293, 595.877, 576.015, 557.859, 557.829, 
                            552.839, 550.426, 510.975, 509.586, 503.608, 495.687, 483.178, 465.958, 433.876, 431.267, 
                            374.421, 373.411, 333.202, 251.537, 145.875, 183, 300.4, 422),
  country_iso3_code = c("USA", "DEU", "CHE", "CAN", "JPN", "FRA", "AUT", "GRC", "ITA", "BEL", "LTU", "KOR", "AUS", 
                        "HUN", "IRL", "SVN", "LUX", "FIN", "ESP", "SVK", "SWE", "CZE", "LVA", "ISL", "PRT", "GBR", 
                        "NOR", "POL", "EST", "NLD", "ISR", "DNK", "CHL", "MEX", "CRI", "COL", "RUS", "ROU")
)

# Merge pharma_spending with ppp_2019_iso by country_iso3_code
pharma_spending <- merge(pharma_spending, ppp_2019_iso, by = "country_iso3_code")

# Merge summary_data2586 with pharma_spending by country_iso3_code
merged_data2586 <- merge(summary_data2586, pharma_spending, by = "country_iso3_code")
merged_data2586 <- merge(summary_data2586, ER_ppp_19WB, by = "country_iso3_code")

# Calculate net_potential_savingsPCadj
merged_data2586 <- merged_data2586 %>%
  mutate(net_potential_savingsPCadj = (net_potential_savingsPC*PPP_rate)/ER_rate)

# Calculate Perc_savingsOnpharmaex
merged_data2586 <- merge(summary_data2586, pharma_spending, by = "country_iso3_code")
merged_data2586 <- merged_data2586 %>%
  mutate(Perc_savingsOnpharmaex = net_potential_savingsPC / Current_prices2019usd)


merged_data2586<- merged_data2586 %>%
  mutate(Perc_savingsOnpharmaex = ifelse(Perc_savingsOnpharmaex > 1, 1, Perc_savingsOnpharmaex))



merged_data2586 <- merged_data2586 %>%
  mutate(Perc_savingsOnpharmaex = Perc_savingsOnpharmaex * 100)

# Create the bar graph
abc0kk<- ggplot(merged_data2586, aes(x = reorder(country_iso3_code, Perc_savingsOnpharmaex), y = Perc_savingsOnpharmaex, fill = WHO_region.x)) +
  geom_bar(stat = "identity", color= "black") +
  coord_flip() +
  labs(x = "Country ISO3 Code", y = "Percentage (%)", title = "") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.25),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  ) +
  scale_fill_manual(values = c("AMR" = "#1f78b4", "EUR" = "#33a02c", "WPR" = "#e31a1c")) +
  guides(fill = guide_legend(title = "WHO Region"))
ggsave("PercentagePharmaExpensPCountryif80acc.tiff", plot = abc0kk, device = "tiff", width =8, height = 10, dpi = 500, units = "in")

export_dataghih <- merged_data2586 %>%
  dplyr::select(country_iso3_code, Current_prices2019usd, Perc_savingsOnpharmaex)

# Export to Excel
write.xlsx(export_dataghih, file = "merged_data2586_export.xlsx")
#####


##---------------------------------------------------------------------------------------------#
# Table cost-savings and prices for procurement using 60, 65, 70, 75, 80%. 
##---------------------------------------------------------------------------------------------#
#######
# Create a new dataframe with calculations for each threshold
# Define the country ISO codes
country_iso3_code <- c(
  'ARE', 'ARG', 'AUS', 'AUT', 'BEL', 'BGD', 'BGR', 'BIH', 'BLR', 'BRA',
  'CAN', 'CHE', 'CHL', 'CHN', 'COL', 'CZE', 'DEU', 'DOM', 'DZA', 'ECU',
  'EGY', 'ESP', 'EST', 'FIN', 'FRA', 'GBR', 'GRC', 'HKG', 'HRV', 'HUN',
  'IDN', 'IND', 'IRL', 'ITA', 'JOR', 'JPN', 'KAZ', 'KOR', 'KWT', 'LBN',
  'LKA', 'LTU', 'LUX', 'LVA', 'MAR', 'MEX', 'MYS', 'NLD', 'NOR', 'NZL',
  'PAK', 'PER', 'PHL', 'POL', 'PRI', 'PRT', 'ROU', 'RUS', 'SAU', 'SGP',
  'SRB', 'SVK', 'SVN', 'SWE', 'THA', 'TUN', 'TUR', 'TWN', 'URY', 'USA',
  'VEN', 'VNM', 'ZAF'
)

# Define the country names
country_names <- c(
  "United Arab Emirates", "Argentina", "Australia", "Austria", "Belgium", "Bangladesh",
  "Bulgaria", "Bosnia and Herzegovina", "Belarus", "Brazil", "Canada", "Switzerland",
  "Chile", "China", "Colombia", "Czech Republic", "Germany", "Dominican Republic",
  "Algeria", "Ecuador", "Egypt", "Spain", "Estonia", "Finland", "France", "United Kingdom",
  "Greece", "Hong Kong", "Croatia", "Hungary", "Indonesia", "India", "Ireland", "Italy",
  "Jordan", "Japan", "Kazakhstan", "South Korea", "Kuwait", "Lebanon", "Sri Lanka",
  "Lithuania", "Luxembourg", "Latvia", "Morocco", "Mexico", "Malaysia", "Netherlands",
  "Norway", "New Zealand", "Pakistan", "Peru", "Philippines", "Poland", "Puerto Rico",
  "Portugal", "Romania", "Russia", "Saudi Arabia", "Singapore", "Serbia", "Slovakia",
  "Slovenia", "Sweden", "Thailand", "Tunisia", "Turkey", "Taiwan", "Uruguay", "United States",
  "Venezuela", "Vietnam", "South Africa"
)

# Define the Prices per DDD for Access Antibiotics
PricesDDDAcces <- c(2.70, 1.71, 0.49, 2.73, 1.01, 0.68, 2.19, 1.54, 1.53, 2.10, 1.09, 1.91, 1.26, 1.87, 1.14, 1.37, 1.34, 4.84, 
           1.55, 1.57, 1.90, 0.61, 1.07, 1.24, 0.91, 1.07, 0.79, 0.59, 11.49, 1.69, 1.57, 1.25, 1.04, 1.30, 1.84, 2.98, 
           1.63, 1.33, 2.44, 1.21, 1.15, 1.21, 0.61, 1.17, 1.64, 3.10, 1.56, 1.02, 1.03, 0.38, 0.78, 1.22, 3.97, 1.34, 
           2.72, 1.08, 1.26, 1.29, 3.15, 0.96, 1.32, 1.43, 2.37, 0.96, 0.97, 1.23, 0.98, 1.28, 1.23, 0.95, 0.90, 1.27, 0.81)


# Define the Prices per DDD for Watch Antibiotics
PricesDDDWatch <- c(4.57, 2.29, 1.48, 9.19, 2.48, 1.42, 3.11, 2.20, 2.76, 2.27, 2.45, 5.13, 1.26, 4.97, 1.45, 2.45, 2.24, 4.78, 
                    3.27, 0.45, 3.48, 2.72, 1.18, 4.04, 5.72, 3.09, 1.19, 2.09, 30.36, 2.70, 5.11, 1.11, 3.72, 3.64, 3.82, 2.60, 
                    2.98, 3.49, 4.38, 3.41, 1.00, 2.13, 0.86, 2.45, 2.93, 5.36, 4.05, 2.97, 2.89, 0.87, 1.87, 1.21, 6.86, 2.10, 
                    6.47, 1.95, 2.69, 1.97, 5.98, 2.61, 2.02, 1.68, 4.81, 4.68, 4.95, 2.83, 1.61, 12.77, 1.49, 5.19, 0.68, 3.04, 2.94)


# Create a dataframe with the specified four variables
dfset_pricesdiff <- data.frame(
  country_iso3_code = country_iso3_code,
  country_name = country_names,
  PricesDDDAcces = PricesDDDAcces,
  PricesDDDWatch = PricesDDDWatch
)
merged_data3_caware_wPERC <- merged_data3_caware_wPERC %>%
  left_join(dfset_pricesdiff %>% dplyr::select(country_iso3_code, PricesDDDAcces, PricesDDDWatch), 
            by = "country_iso3_code")
merged_data3_caware_wPERC$median_price_ww <- merged_data3_caware_wPERC$PricesDDDWatch
merged_data3_caware_wPERC$median_price_wa <- merged_data3_caware_wPERC$PricesDDDAcces
merged_data3_caware_wPERC <- merged_data3_caware_wPERC %>%
  filter(aware_category == "Access")
summary_data299 <- merged_data3_caware_wPERC %>%
  mutate(
    diff_price =  PricesDDDWatch - PricesDDDAcces,    # Difference in price
    gap_to_80 = (80 - Percent_Access) / 100,           # Gap to 80% Access
    gap_to_75 = (75 - Percent_Access) / 100,           # Gap to 75% Access
    gap_to_70 = (70 - Percent_Access) / 100,           # Gap to 70% Access
    gap_to_65 = (65 - Percent_Access) / 100,           # Gap to 65% Access
    gap_to_60 = (60 - Percent_Access) / 100,           # Gap to 60% Access
    
    net_potential_savings_80 = (total_ddd_soldT * gap_to_80) * diff_price,   # Savings at 80%
    net_potential_savings_75 = (total_ddd_soldT * gap_to_75) * diff_price,   # Savings at 75%
    net_potential_savings_70 = (total_ddd_soldT * gap_to_70) * diff_price,   # Savings at 70%
    net_potential_savings_65 = (total_ddd_soldT * gap_to_65) * diff_price,   # Savings at 65%
    net_potential_savings_60 = (total_ddd_soldT * gap_to_60) * diff_price    # Savings at 60%
  ) %>%
  filter(!is.na(Percent_Access))  # Remove rows with NA in Percent_Access

# Convert net potential savings from units to billions
summary_data299 <- summary_data299 %>%
  mutate(
    net_potential_savings_80 = net_potential_savings_80 / 1e9,
    net_potential_savings_75 = net_potential_savings_75 / 1e9,
    net_potential_savings_70 = net_potential_savings_70 / 1e9,
    net_potential_savings_65 = net_potential_savings_65 / 1e9,
    net_potential_savings_60 = net_potential_savings_60 / 1e9
  )

# Add population data and adjust specific populations if needed
summary_data299 <- summary_data299 %>%
  left_join(data_2019 %>% dplyr::select(country_iso3_code, population), by = "country_iso3_code") %>%
  mutate(
    population = if_else(country_iso3_code == "TWN", 23600000, population),
    population = if_else(country_iso3_code == "ARE", 9212000, population)
  )

# Calculate net potential savings per capita for each threshold
summary_data299 <- summary_data299 %>%
  mutate(
    net_potential_savingsPC_80 = (net_potential_savings_80 * 1e9) / population,
    net_potential_savingsPC_75 = (net_potential_savings_75 * 1e9) / population,
    net_potential_savingsPC_70 = (net_potential_savings_70 * 1e9) / population,
    net_potential_savingsPC_65 = (net_potential_savings_65 * 1e9) / population,
    net_potential_savingsPC_60 = (net_potential_savings_60 * 1e9) / population
  )

# Apply specific adjustments if needed
# Arrange the data by net potential savings per capita for the 80% threshold
summary_data299 <- summary_data299 %>%
  arrange(net_potential_savingsPC_80)
# Convert country_iso3_code to a factor with levels ordered by net_potential_savingsPC_80
summary_data299$country_iso3_code <- factor(summary_data299$country_iso3_code, levels = summary_data299$country_iso3_code)
# Display the first few rows of the final dataset
head(summary_data299)
summary_data299 <- summary_data299 %>%
  mutate(ratio_proc = if_else(WB_incomeshort == "HIC", 1.12, 
                              if_else(WB_incomeshort == "LMIC", 1.21, NA_real_)))

#Adjusting numbers 
dataX2 <- data.frame(
  country_iso3_code = c('AUS', 'AUS', 'AUT', 'AUT', 'BEL', 'BEL', 'BGR', 'BGR', 'BRA', 'BRA',
              'CAN', 'CAN', 'CZE', 'CZE', 'DEU', 'DEU', 'EST', 'EST', 'FIN', 'FIN',
              'GBR', 'GBR', 'GRC', 'GRC', 'HRV', 'HRV', 'HUN', 'HUN', 'IRL', 'IRL',
              'LTU', 'LTU', 'LUX', 'LUX', 'LVA', 'LVA', 'MEX', 'MEX', 'POL', 'POL',
              'ROU', 'ROU', 'RUS', 'RUS', 'SAU', 'SAU', 'SVK', 'SVK', 'SVN', 'SVN',
              'SWE', 'SWE', 'TUR', 'TUR', 'ZAF', 'ZAF'),
  Category = c('Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch'),
  Value = c(1.074, 1.075, 1.146, 1.132, 1.159, 1.143, 1.063, 1.056, 1.253, 1.252,
            1.080, 1.080, 1.050, 1.050, 1.162, 1.179, 1.131, 1.131, 1.040, 1.040,
            1.067, 1.078, 1.150, 1.150, 1.063, 1.055, 1.059, 1.048, 1.080, 1.080,
            1.089, 1.080, 1.175, 1.157, 1.241, 1.240, 1.161, 1.150, 1.080, 1.084,
            1.136, 1.134, 1.262, 1.254, 1.149, 1.145, 1.080, 1.099, 1.065, 1.060,
            1.027, 1.027, 1.087, 1.069, 1.300, 1.294)
)

# Calculate the average value per country
average_per_country <- dataX2 %>%
  group_by(country_iso3_code) %>%
  summarize(average_value = mean(Value))

summary_data299 <- summary_data299 %>%
  left_join(average_per_country, by = "country_iso3_code")

# Update ratio_proc in summary_data299 with the average_value only if average_value is not missing
summary_data299 <- summary_data299 %>%
  mutate(ratio_proc = if_else(!is.na(average_value), average_value, ratio_proc))

summary_data299$net_potential_savingsPC_80_proc <- summary_data299$net_potential_savingsPC_80 / summary_data299$ratio_proc
summary_data299$net_potential_savings_80_proc   <- summary_data299$net_potential_savings_80 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_75_proc <- summary_data299$net_potential_savingsPC_75 / summary_data299$ratio_proc
summary_data299$net_potential_savings_75_proc   <- summary_data299$net_potential_savings_75 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_70_proc <- summary_data299$net_potential_savingsPC_70 / summary_data299$ratio_proc
summary_data299$net_potential_savings_70_proc   <- summary_data299$net_potential_savings_70 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_65_proc <- summary_data299$net_potential_savingsPC_65 / summary_data299$ratio_proc
summary_data299$net_potential_savings_65_proc   <- summary_data299$net_potential_savings_65 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_60_proc <- summary_data299$net_potential_savingsPC_60 / summary_data299$ratio_proc
summary_data299$net_potential_savings_60_proc   <- summary_data299$net_potential_savings_60 / summary_data299$ratio_proc







#######

##---------------------------------------------------------------------------------------------#
# Table cost-savings and prices for procurement using 60, 65, 70, 75, 80%. 
# BUT USING EXTERNAL REFERENCE PRICING ACROSS WHO REGIONS
##---------------------------------------------------------------------------------------------#
#######
# Create a new dataframe with calculations for each threshold
# Define the country ISO codes
country_iso3_code <- c(
  'ARE', 'ARG', 'AUS', 'AUT', 'BEL', 'BGD', 'BGR', 'BIH', 'BLR', 'BRA',
  'CAN', 'CHE', 'CHL', 'CHN', 'COL', 'CZE', 'DEU', 'DOM', 'DZA', 'ECU',
  'EGY', 'ESP', 'EST', 'FIN', 'FRA', 'GBR', 'GRC', 'HKG', 'HRV', 'HUN',
  'IDN', 'IND', 'IRL', 'ITA', 'JOR', 'JPN', 'KAZ', 'KOR', 'KWT', 'LBN',
  'LKA', 'LTU', 'LUX', 'LVA', 'MAR', 'MEX', 'MYS', 'NLD', 'NOR', 'NZL',
  'PAK', 'PER', 'PHL', 'POL', 'PRI', 'PRT', 'ROU', 'RUS', 'SAU', 'SGP',
  'SRB', 'SVK', 'SVN', 'SWE', 'THA', 'TUN', 'TUR', 'TWN', 'URY', 'USA',
  'VEN', 'VNM', 'ZAF'
)

# Define the country names
country_names <- c(
  "United Arab Emirates", "Argentina", "Australia", "Austria", "Belgium", "Bangladesh",
  "Bulgaria", "Bosnia and Herzegovina", "Belarus", "Brazil", "Canada", "Switzerland",
  "Chile", "China", "Colombia", "Czech Republic", "Germany", "Dominican Republic",
  "Algeria", "Ecuador", "Egypt", "Spain", "Estonia", "Finland", "France", "United Kingdom",
  "Greece", "Hong Kong", "Croatia", "Hungary", "Indonesia", "India", "Ireland", "Italy",
  "Jordan", "Japan", "Kazakhstan", "South Korea", "Kuwait", "Lebanon", "Sri Lanka",
  "Lithuania", "Luxembourg", "Latvia", "Morocco", "Mexico", "Malaysia", "Netherlands",
  "Norway", "New Zealand", "Pakistan", "Peru", "Philippines", "Poland", "Puerto Rico",
  "Portugal", "Romania", "Russia", "Saudi Arabia", "Singapore", "Serbia", "Slovakia",
  "Slovenia", "Sweden", "Thailand", "Tunisia", "Turkey", "Taiwan", "Uruguay", "United States",
  "Venezuela", "Vietnam", "South Africa"
)


who_regions <- c(
  "EMR", "AMR", "WPR", "EUR", "EUR", "SEAR",
  "EUR", "EUR", "EUR", "AMR", "AMR", "EUR",
  "AMR", "WPR", "AMR", "EUR", "EUR", "AMR",
  "AFR", "AMR", "EMR", "EUR", "EUR", "EUR", "EUR", "EUR",
  "EUR", "WPR", "EUR", "EUR", "SEAR", "SEAR", "EUR", "EUR",
  "EMR", "WPR", "EUR", "WPR", "EMR", "EMR", "SEAR",
  "EUR", "EUR", "EUR", "AFR", "AMR", "WPR", "EUR",
  "EUR", "WPR", "EMR", "AMR", "WPR", "EUR", "AMR",
  "EUR", "EUR", "EUR", "EMR", "WPR", "EUR", "EUR",
  "EUR", "EUR", "SEAR", "AFR", "EUR", "WPR", "AMR", "AMR",
  "AMR", "WPR", "AFR"
)

PricesDDDAcces  <- c(
  1.84, 1.26, 1.28, 1.28, 1.28, 1.15,
  1.28, 1.28, 1.28, 1.26, 1.26, 1.28,
  1.26, 1.28, 1.26, 1.28, 1.28, 1.26,
  1.23, 1.26, 1.84, 1.28, 1.28, 1.28, 1.28, 1.28,
  1.28, 1.28, 1.28, 1.28, 1.15, 1.15, 1.28, 1.28,
  1.84, 1.28, 1.28, 1.28, 1.84, 1.84, 1.15,
  1.28, 1.28, 1.28, 1.23, 1.26, 1.28, 1.28,
  1.28, 1.28, 1.84, 1.26, 1.28, 1.28, 1.26,
  1.28, 1.28, 1.28, 1.84, 1.28, 1.28, 1.28,
  1.28, 1.28, 1.15, 1.23, 1.28, 1.28, 1.26, 1.26,
  1.26, 1.28, 1.23
)


PricesDDDWatch  <- c(
  3.48, 2.27, 3.04, 2.71, 2.71, 1.42,
  2.71, 2.71, 2.71, 2.27, 2.27, 2.71,
  2.27, 3.04, 2.27, 2.71, 2.71, 2.27,
  2.94, 2.27, 3.48, 2.71, 2.71, 2.71, 2.71, 2.71,
  2.71, 3.04, 2.71, 2.71, 1.42, 1.42, 2.71, 2.71,
  3.48, 3.04, 2.71, 3.04, 3.48, 3.48, 1.42,
  2.71, 2.71, 2.71, 2.94, 2.27, 3.04, 2.71,
  2.71, 3.04, 3.48, 2.27, 3.04, 2.71, 2.27,
  2.71, 2.71, 2.71, 3.48, 3.04, 2.71, 2.71,
  2.71, 2.71, 1.42, 2.94, 2.71, 3.04, 2.27, 2.27,
  2.27, 3.04, 2.94
)

#AFR	1·23
#AMR	1·26
#EMR	1·84
#EUR	1·28
#SEAR	1·15
#WPR	1·28

#AFR 2·94
#AMR 2·27
#EMR 3·48
#EUR 2·71
#SEAR 1·42
#WPR 3·04



# Create a dataframe with the specified four variables
dfset_pricesdiff <- data.frame(
  country_iso3_code = country_iso3_code,
  country_name = country_names,
  who_regions = who_regions,
  PricesDDDAcces = PricesDDDAcces,
  PricesDDDWatch = PricesDDDWatch
)
merged_data3_caware_wPERC <- merged_data3_caware_wPERC %>%
  left_join(dfset_pricesdiff %>% dplyr::select(country_iso3_code, PricesDDDAcces, PricesDDDWatch), 
            by = "country_iso3_code")
merged_data3_caware_wPERC$median_price_ww <- merged_data3_caware_wPERC$PricesDDDWatch
merged_data3_caware_wPERC$median_price_wa <- merged_data3_caware_wPERC$PricesDDDAcces
merged_data3_caware_wPERC <- merged_data3_caware_wPERC %>%
  filter(aware_category == "Access")
summary_data299 <- merged_data3_caware_wPERC %>%
  mutate(
    diff_price =  PricesDDDWatch - PricesDDDAcces,    # Difference in price
    gap_to_80 = (80 - Percent_Access) / 100,           # Gap to 80% Access
    gap_to_75 = (75 - Percent_Access) / 100,           # Gap to 75% Access
    gap_to_70 = (70 - Percent_Access) / 100,           # Gap to 70% Access
    gap_to_65 = (65 - Percent_Access) / 100,           # Gap to 65% Access
    gap_to_60 = (60 - Percent_Access) / 100,           # Gap to 60% Access
    
    net_potential_savings_80 = (total_ddd_soldT * gap_to_80) * diff_price,   # Savings at 80%
    net_potential_savings_75 = (total_ddd_soldT * gap_to_75) * diff_price,   # Savings at 75%
    net_potential_savings_70 = (total_ddd_soldT * gap_to_70) * diff_price,   # Savings at 70%
    net_potential_savings_65 = (total_ddd_soldT * gap_to_65) * diff_price,   # Savings at 65%
    net_potential_savings_60 = (total_ddd_soldT * gap_to_60) * diff_price    # Savings at 60%
  ) %>%
  filter(!is.na(Percent_Access))  # Remove rows with NA in Percent_Access

# Convert net potential savings from units to billions
summary_data299 <- summary_data299 %>%
  mutate(
    net_potential_savings_80 = net_potential_savings_80 / 1e9,
    net_potential_savings_75 = net_potential_savings_75 / 1e9,
    net_potential_savings_70 = net_potential_savings_70 / 1e9,
    net_potential_savings_65 = net_potential_savings_65 / 1e9,
    net_potential_savings_60 = net_potential_savings_60 / 1e9
  )

# Add population data and adjust specific populations if needed
summary_data299 <- summary_data299 %>%
  left_join(data_2019 %>% dplyr::select(country_iso3_code, population), by = "country_iso3_code") %>%
  mutate(
    population = if_else(country_iso3_code == "TWN", 23600000, population),
    population = if_else(country_iso3_code == "ARE", 9212000, population)
  )

# Calculate net potential savings per capita for each threshold
summary_data299 <- summary_data299 %>%
  mutate(
    net_potential_savingsPC_80 = (net_potential_savings_80 * 1e9) / population,
    net_potential_savingsPC_75 = (net_potential_savings_75 * 1e9) / population,
    net_potential_savingsPC_70 = (net_potential_savings_70 * 1e9) / population,
    net_potential_savingsPC_65 = (net_potential_savings_65 * 1e9) / population,
    net_potential_savingsPC_60 = (net_potential_savings_60 * 1e9) / population
  )

# Apply specific adjustments if needed
# Arrange the data by net potential savings per capita for the 80% threshold
summary_data299 <- summary_data299 %>%
  arrange(net_potential_savingsPC_80)
# Convert country_iso3_code to a factor with levels ordered by net_potential_savingsPC_80
summary_data299$country_iso3_code <- factor(summary_data299$country_iso3_code, levels = summary_data299$country_iso3_code)
# Display the first few rows of the final dataset
head(summary_data299)
summary_data299 <- summary_data299 %>%
  mutate(ratio_proc = if_else(WB_incomeshort == "HIC", 1.12, 
                              if_else(WB_incomeshort == "LMIC", 1.21, NA_real_)))

#Adjusting numbers 
dataX2 <- data.frame(
  country_iso3_code = c('AUS', 'AUS', 'AUT', 'AUT', 'BEL', 'BEL', 'BGR', 'BGR', 'BRA', 'BRA',
                        'CAN', 'CAN', 'CZE', 'CZE', 'DEU', 'DEU', 'EST', 'EST', 'FIN', 'FIN',
                        'GBR', 'GBR', 'GRC', 'GRC', 'HRV', 'HRV', 'HUN', 'HUN', 'IRL', 'IRL',
                        'LTU', 'LTU', 'LUX', 'LUX', 'LVA', 'LVA', 'MEX', 'MEX', 'POL', 'POL',
                        'ROU', 'ROU', 'RUS', 'RUS', 'SAU', 'SAU', 'SVK', 'SVK', 'SVN', 'SVN',
                        'SWE', 'SWE', 'TUR', 'TUR', 'ZAF', 'ZAF'),
  Category = c('Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch'),
  Value = c(1.074, 1.075, 1.146, 1.132, 1.159, 1.143, 1.063, 1.056, 1.253, 1.252,
            1.080, 1.080, 1.050, 1.050, 1.162, 1.179, 1.131, 1.131, 1.040, 1.040,
            1.067, 1.078, 1.150, 1.150, 1.063, 1.055, 1.059, 1.048, 1.080, 1.080,
            1.089, 1.080, 1.175, 1.157, 1.241, 1.240, 1.161, 1.150, 1.080, 1.084,
            1.136, 1.134, 1.262, 1.254, 1.149, 1.145, 1.080, 1.099, 1.065, 1.060,
            1.027, 1.027, 1.087, 1.069, 1.300, 1.294)
)

# Calculate the average value per country
average_per_country <- dataX2 %>%
  group_by(country_iso3_code) %>%
  summarize(average_value = mean(Value))

summary_data299 <- summary_data299 %>%
  left_join(average_per_country, by = "country_iso3_code")

# Update ratio_proc in summary_data299 with the average_value only if average_value is not missing
summary_data299 <- summary_data299 %>%
  mutate(ratio_proc = if_else(!is.na(average_value), average_value, ratio_proc))

summary_data299$net_potential_savingsPC_80_proc <- summary_data299$net_potential_savingsPC_80 / summary_data299$ratio_proc
summary_data299$net_potential_savings_80_proc   <- summary_data299$net_potential_savings_80 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_75_proc <- summary_data299$net_potential_savingsPC_75 / summary_data299$ratio_proc
summary_data299$net_potential_savings_75_proc   <- summary_data299$net_potential_savings_75 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_70_proc <- summary_data299$net_potential_savingsPC_70 / summary_data299$ratio_proc
summary_data299$net_potential_savings_70_proc   <- summary_data299$net_potential_savings_70 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_65_proc <- summary_data299$net_potential_savingsPC_65 / summary_data299$ratio_proc
summary_data299$net_potential_savings_65_proc   <- summary_data299$net_potential_savings_65 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_60_proc <- summary_data299$net_potential_savingsPC_60 / summary_data299$ratio_proc
summary_data299$net_potential_savings_60_proc   <- summary_data299$net_potential_savings_60 / summary_data299$ratio_proc







#######

##---------------------------------------------------------------------------------------------#
# Table cost-savings and prices for procurement using 60, 65, 70, 75, 80%. 
# BUT USING EXTERNAL REFERENCE PRICING ACROSS WHO REGIONS
##---------------------------------------------------------------------------------------------#
#######
country_iso3_code <- c("DZA", "ARG", "AUS", "AUT", "BGD", "BLR", "BEL", "BIH", "BRA", "BGR", "CAN", "CHL", "CHN", 
                       "COL", "HRV", "CZE", "DOM", "ECU", "EGY", "EST", "FIN", "FRA", "DEU", "GRC", "HKG", "HUN", 
                       "IND", "IDN", "IRL", "ITA", "JPN", "JOR", "KAZ", "KOR", "KWT", "LVA", "LBN", "LTU", "LUX", 
                       "MYS", "MEX", "MAR", "NLD", "NZL", "NOR", "PAK", "PER", "PHL", "POL", "PRT", "PRI", "ROU", 
                       "RUS", "SAU", "SRB", "SGP", "SVK", "SVN", "ZAF", "ESP", "LKA", "SWE", "CHE", "TWN", "THA", 
                       "TUN", "TUR", "ARE", "GBR", "URY", "USA", "VEN", "VNM")
PricesDDDAcces <- c(1.54, 1.66, 0.46, 1.69, 0.5, 1.44, 0.92, 1.54, 2.13, 1.96, 1.09, 1.24, 1.51, 1.04, 11.31, 1.32, 
                  4.76, 1.52, 1.43, 1.02, 1.25, 0.9, 0.96, 0.79, 0.58, 1.68, 1.33, 1.17, 0.89, 1.28, 1.55, 1.83, 
                  1.63, 1.01, 2.44, 1.05, 1.19, 1.13, 0.59, 1.26, 3.05, 1.59, 0.88, 0.34, 1.11, 0.67, 1.27, 3.18, 
                  1.43, 1.03, 2.6, 1.08, 1.2, 3.12, 1.31, 0.95, 1.18, 2.23, 0.82, 0.61, 1.12, 0.87, 1.79, 0.9, 1.07, 
                  1.24, 0.9, 2.7, 0.98, 1.05, 0.89, 0.9, 0.98)
PricesDDDWatch<- c(2.61, 2.34, 1.78, 10.38, 1.44, 2.71, 2.55, 2.17, 2.15, 2.9, 2.19, 0.91, 5.12, 1.19, 30.43, 2.29, 
                 4.34, 0.35, 4.15, 1.14, 5.92, 7.46, 2.18, 0.94, 2.16, 2.73, 1.22, 5.95, 4.24, 4.03, 3.09, 3.65, 3.01, 
                 4.68, 4.46, 2.5, 2.93, 2.12, 0.76, 4.1, 5.49, 3.03, 2.97, 0.94, 6.09, 2.28, 1.01, 6.69, 2.04, 1.82, 
                 5.89, 2.84, 1.85, 6.06, 1.86, 2.43, 1.45, 4.73, 2.15, 2.33, 1, 9.63, 5.67, 12.6, 10.78, 2.39, 1.36, 
                 4.32, 4.73, 1.43, 2.58, 0.7, 2.2)

# Create a matrix
data_matrix <- cbind(country_iso3_code = country_iso3_code, PricesDDDAccess = PricesDDDAcces, PricesDDDWatch = PricesDDDWatch)
dfset_pricesdiff <- data.frame(country_iso3_code = country_iso3_code, PricesDDDAcces = PricesDDDAcces, PricesDDDWatch = PricesDDDWatch)




# Create a dataframe with the specified four variables
merged_data3_caware_wPERC <- merged_data3_caware_wPERC %>%
  left_join(dfset_pricesdiff %>% dplyr::select(country_iso3_code, PricesDDDAcces, PricesDDDWatch), 
            by = "country_iso3_code")
merged_data3_caware_wPERC$median_price_ww <- merged_data3_caware_wPERC$PricesDDDWatch
merged_data3_caware_wPERC$median_price_wa <- merged_data3_caware_wPERC$PricesDDDAcces
merged_data3_caware_wPERC <- merged_data3_caware_wPERC %>%
  filter(aware_category == "Access")
summary_data299 <- merged_data3_caware_wPERC %>%
  mutate(
    diff_price =  PricesDDDWatch - PricesDDDAcces,    # Difference in price
    gap_to_80 = (80 - Percent_Access) / 100,           # Gap to 80% Access
    gap_to_75 = (75 - Percent_Access) / 100,           # Gap to 75% Access
    gap_to_70 = (70 - Percent_Access) / 100,           # Gap to 70% Access
    gap_to_65 = (65 - Percent_Access) / 100,           # Gap to 65% Access
    gap_to_60 = (60 - Percent_Access) / 100,           # Gap to 60% Access
    
    net_potential_savings_80 = (total_ddd_soldT * gap_to_80) * diff_price,   # Savings at 80%
    net_potential_savings_75 = (total_ddd_soldT * gap_to_75) * diff_price,   # Savings at 75%
    net_potential_savings_70 = (total_ddd_soldT * gap_to_70) * diff_price,   # Savings at 70%
    net_potential_savings_65 = (total_ddd_soldT * gap_to_65) * diff_price,   # Savings at 65%
    net_potential_savings_60 = (total_ddd_soldT * gap_to_60) * diff_price    # Savings at 60%
  ) %>%
  filter(!is.na(Percent_Access))  # Remove rows with NA in Percent_Access

# Convert net potential savings from units to billions
summary_data299 <- summary_data299 %>%
  mutate(
    net_potential_savings_80 = net_potential_savings_80 / 1e9,
    net_potential_savings_75 = net_potential_savings_75 / 1e9,
    net_potential_savings_70 = net_potential_savings_70 / 1e9,
    net_potential_savings_65 = net_potential_savings_65 / 1e9,
    net_potential_savings_60 = net_potential_savings_60 / 1e9
  )

# Add population data and adjust specific populations if needed
summary_data299 <- summary_data299 %>%
  left_join(data_2019 %>% dplyr::select(country_iso3_code, population), by = "country_iso3_code") %>%
  mutate(
    population = if_else(country_iso3_code == "TWN", 23600000, population),
    population = if_else(country_iso3_code == "ARE", 9212000, population)
  )

# Calculate net potential savings per capita for each threshold
summary_data299 <- summary_data299 %>%
  mutate(
    net_potential_savingsPC_80 = (net_potential_savings_80 * 1e9) / population,
    net_potential_savingsPC_75 = (net_potential_savings_75 * 1e9) / population,
    net_potential_savingsPC_70 = (net_potential_savings_70 * 1e9) / population,
    net_potential_savingsPC_65 = (net_potential_savings_65 * 1e9) / population,
    net_potential_savingsPC_60 = (net_potential_savings_60 * 1e9) / population
  )

# Apply specific adjustments if needed
# Arrange the data by net potential savings per capita for the 80% threshold
summary_data299 <- summary_data299 %>%
  arrange(net_potential_savingsPC_80)
# Convert country_iso3_code to a factor with levels ordered by net_potential_savingsPC_80
summary_data299$country_iso3_code <- factor(summary_data299$country_iso3_code, levels = summary_data299$country_iso3_code)
# Display the first few rows of the final dataset
head(summary_data299)
summary_data299 <- summary_data299 %>%
  mutate(ratio_proc = if_else(WB_incomeshort == "HIC", 1.12, 
                              if_else(WB_incomeshort == "LMIC", 1.21, NA_real_)))

#Adjusting numbers 
dataX2 <- data.frame(
  country_iso3_code = c('AUS', 'AUS', 'AUT', 'AUT', 'BEL', 'BEL', 'BGR', 'BGR', 'BRA', 'BRA',
                        'CAN', 'CAN', 'CZE', 'CZE', 'DEU', 'DEU', 'EST', 'EST', 'FIN', 'FIN',
                        'GBR', 'GBR', 'GRC', 'GRC', 'HRV', 'HRV', 'HUN', 'HUN', 'IRL', 'IRL',
                        'LTU', 'LTU', 'LUX', 'LUX', 'LVA', 'LVA', 'MEX', 'MEX', 'POL', 'POL',
                        'ROU', 'ROU', 'RUS', 'RUS', 'SAU', 'SAU', 'SVK', 'SVK', 'SVN', 'SVN',
                        'SWE', 'SWE', 'TUR', 'TUR', 'ZAF', 'ZAF'),
  Category = c('Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch',
               'Access', 'Watch', 'Access', 'Watch', 'Access', 'Watch'),
  Value = c(1.074, 1.075, 1.146, 1.132, 1.159, 1.143, 1.063, 1.056, 1.253, 1.252,
            1.080, 1.080, 1.050, 1.050, 1.162, 1.179, 1.131, 1.131, 1.040, 1.040,
            1.067, 1.078, 1.150, 1.150, 1.063, 1.055, 1.059, 1.048, 1.080, 1.080,
            1.089, 1.080, 1.175, 1.157, 1.241, 1.240, 1.161, 1.150, 1.080, 1.084,
            1.136, 1.134, 1.262, 1.254, 1.149, 1.145, 1.080, 1.099, 1.065, 1.060,
            1.027, 1.027, 1.087, 1.069, 1.300, 1.294)
)

# Calculate the average value per country
average_per_country <- dataX2 %>%
  group_by(country_iso3_code) %>%
  summarize(average_value = mean(Value))

summary_data299 <- summary_data299 %>%
  left_join(average_per_country, by = "country_iso3_code")

# Update ratio_proc in summary_data299 with the average_value only if average_value is not missing
summary_data299 <- summary_data299 %>%
  mutate(ratio_proc = if_else(!is.na(average_value), average_value, ratio_proc))

summary_data299$net_potential_savingsPC_80_proc <- summary_data299$net_potential_savingsPC_80 / summary_data299$ratio_proc
summary_data299$net_potential_savings_80_proc   <- summary_data299$net_potential_savings_80 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_75_proc <- summary_data299$net_potential_savingsPC_75 / summary_data299$ratio_proc
summary_data299$net_potential_savings_75_proc   <- summary_data299$net_potential_savings_75 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_70_proc <- summary_data299$net_potential_savingsPC_70 / summary_data299$ratio_proc
summary_data299$net_potential_savings_70_proc   <- summary_data299$net_potential_savings_70 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_65_proc <- summary_data299$net_potential_savingsPC_65 / summary_data299$ratio_proc
summary_data299$net_potential_savings_65_proc   <- summary_data299$net_potential_savings_65 / summary_data299$ratio_proc
summary_data299$net_potential_savingsPC_60_proc <- summary_data299$net_potential_savingsPC_60 / summary_data299$ratio_proc
summary_data299$net_potential_savings_60_proc   <- summary_data299$net_potential_savings_60 / summary_data299$ratio_proc












#######

#---------------------------------------------------------------------------------------------#
#ACCESS to WATCH Ratio per country GRAPH, EML book and overall #
#---------------------------------------------------------------------------------------------#
#####
weighted_avg_dataMIDAS_prCAwrEt65<- midas_price %>%
  group_by(CEAw) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), aware_category = first(aware_category), country_iso3_code= first(country_iso3_code), WHO_region= first(WHO_region), WB_income=first(WB_income), WB_incomeshort= first(WB_incomeshort), EML_book= first(EML_book), na.rm = TRUE)
filtered_datajij <- weighted_avg_dataMIDAS_prCAwrEt65 %>%
  dplyr::select(aware_category, PriceperDDD_w, country_iso3_code, total_ddd_sold, EML_book, WHO_region, WB_income)

calculate_ratio <- function(data) {
  data %>%
    group_by(country_iso3_code) %>%
    summarise(
      access_total = sum(total_ddd_sold[aware_category == "Access"], na.rm = TRUE),
      watch_total = sum(total_ddd_sold[aware_category == "Watch"], na.rm = TRUE),
      WHO_region= first(WHO_region),
      WB_income= first(WB_income)
    ) %>%
    mutate(access_to_watch_ratio = access_total / watch_total) %>%
    filter(!is.na(access_to_watch_ratio))
}

# Calculate ratios for both EML_book = 0 and EML_book = 1
ratio_data_0 <- filtered_datajij %>%
  filter(EML_book == 0) %>%
  calculate_ratio() %>%
  mutate(EML_book = 0)

ratio_data_1 <- filtered_datajij %>%
  filter(EML_book == 1) %>%
  calculate_ratio() %>%
  mutate(EML_book = 1)

# Combine the data for plotting
combined_data <- bind_rows(ratio_data_0, ratio_data_1)

bar_colors <- c("#56B4E9", "#E69F00")


# Plot the data
b2jih<-ggplot(combined_data, aes(x = access_to_watch_ratio, y = reorder(country_iso3_code, access_to_watch_ratio), fill = as.factor(EML_book))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_vline(xintercept = 1, color = "#D55E00", linetype = "dashed") +
  facet_wrap(~EML_book, ncol = 2, scales = "free_x", labeller = labeller(EML_book = c(`0` = "A", `1` = "B"))) +
  labs(title = "", x = "Access to Watch Ratio", y = "Country ISO3 Code") +
  scale_fill_manual(values = bar_colors, labels = c("Antibiotics included in the EML book", "Antibiotics not included in the EML book")) +
  scale_x_continuous(breaks = seq(0, max(combined_data$access_to_watch_ratio, na.rm = TRUE), by = 0.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank()
  )
ggsave("access_to_watchR_EMLnonEML.tiff", plot = b2jih, device = "tiff", width =12, height = 9, dpi = 500, units = "in")


library(openxlsx)
pivot_tablekokh <- combined_data %>%
  pivot_wider(names_from = EML_book, values_from = access_to_watch_ratio, names_prefix = "EML_book_") %>%
  arrange(country_iso3_code)

# Save to Excel
write.xlsx(pivot_tablekokh, "access_to_watch_dataEMLandnonEML.xlsx")



#All antibiotics: // // // // // // // // // //  // // // // // // // // // //

weighted_avg_dataMIDAS_prCAw2222<- midas_price %>%
  group_by(CAw) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), aware_category = first(aware_category), country_iso3_code= first(country_iso3_code), WHO_region= first(WHO_region), WB_income=first(WB_income), WB_incomeshort= first(WB_incomeshort), na.rm = TRUE)

filtered_datajij2 <- weighted_avg_dataMIDAS_prCAw2222 %>%
  dplyr::select(aware_category, PriceperDDD_w, country_iso3_code, total_ddd_sold, WHO_region, WB_income)


ratio_data_0333 <- filtered_datajij2 %>%
  calculate_ratio()


bar_colors <- c("#56B4E9", "#E69F00", "#F0E442", "#009E73", "#CC79A7", "#0072B2")

# Plot the data
b2jih2 <- ggplot(ratio_data_0333, aes(x = access_to_watch_ratio, y = reorder(country_iso3_code, access_to_watch_ratio), fill = as.factor(WHO_region))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_vline(xintercept = 1, color = "#D55E00", linetype = "solid", size = 1.2) +
  labs(title = "", x = "Access to Watch Ratio", y = "Country ISO-3 Code") +
  scale_fill_manual(values = bar_colors, labels = c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")) +
  scale_x_continuous(breaks = seq(0, max(ratio_data_0333$access_to_watch_ratio, na.rm = TRUE), by = 0.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = c(0.95, 0.05),  # Position legend in bottom right corner
    legend.justification = c(1, 0),  # Adjust justification to align legend correctly
    legend.title = element_text(size = 12, face = "bold", hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "WHO regions"))


ggsave("access_to_watchR_ALLL.tiff", plot = b2jih2, device = "tiff", width =9, height = 9, dpi = 500, units = "in")


library(openxlsx)
pivot_tablekokhe <- ratio_data_0333 %>%
  dplyr::select(country_iso3_code, access_to_watch_ratio, WHO_region, WB_income) %>%
  arrange(country_iso3_code)

# Save to Excel
write.xlsx(pivot_tablekokhe, "access_to_watch_dataALL.xlsx")





#####
#---------------------------------------------------------------------------------------------#
# EXPORT TO SHINY APP : values
#---------------------------------------------------------------------------------------------#
#######
merged_data3_caware_w_a <- weighted_avg_dataMIDAS_prCAw %>%
  filter(aware_category == "Access")
merged_data3_caware_w_w <- weighted_avg_dataMIDAS_prCAw %>%
  filter(aware_category == "Watch")
merged_data3_caware_w_r <- weighted_avg_dataMIDAS_prCAw%>%
  filter(aware_category == "Reserve")

#antibiotic prices per access/watch by country
price_list_access <- merged_data3_caware_w_a %>%
  dplyr::select(country_iso3_code, PriceperDDD_w) %>%  # Select the necessary columns
  arrange(country_iso3_code) %>%  # Sort the data by country ISO code
  pull(PriceperDDD_w)
print(price_list_access)
prices_character <- formatC(price_list_access, format = "f", digits = 10)
# Create a string that formats it as an R vector
prices_codea <- paste("c(", paste(prices_character, collapse = ", "), ")")
print(prices_codea)

price_list_watch <- merged_data3_caware_w_w %>%
  dplyr::select(country_iso3_code, PriceperDDD_w) %>%  # Select the necessary columns
  arrange(country_iso3_code) %>%  # Sort the data by country ISO code
  pull(PriceperDDD_w)
print(price_list_watch)
prices_character <- formatC(price_list_watch, format = "f", digits = 10)
# Create a string that formats it as an R vector
prices_codew <- paste("c(", paste(prices_character, collapse = ", "), ")")
print(prices_codew)


#Proportions of access consumption per country
percent_access_vector <- summary_data %>%
  arrange(country_iso3_code) %>%  # Order by country_iso3_code
  pull(Percent_Access) %>%
  na.omit() 
# Convert the numeric vector to a character vector for formatting
percent_access_vector
# Create a string that formats it as an R vector
percent_access_code <- paste("c(", paste(percent_access_vector, collapse = ", "), ")")
# Print the formatted string
cat(percent_access_code)



#Total DDDs per country
total_DDD_pCountry <- midas_price %>%
  group_by(country_iso3_code) %>%
  summarise(TotalDDD = sum(ddd, na.rm = TRUE)) %>%  # Calculate sum, ignoring NA values
  filter(TotalDDD != 0) %>%  # Remove rows where the sum is zero
  arrange(country_iso3_code) %>%  # Order results by country_iso3_code
  pull(TotalDDD)  
# Create a string that formats it as an R vector
total_DDD_pCountry_code <- paste("c(", paste(total_DDD_pCountry, collapse = ", "), ")")
# Print the formatted string
cat(total_DDD_pCountry_code)


#Exchange rates not PPP to provide both estimates in 2019 USD.
midas_price <- dplyr::left_join(midas_price, ER_ppp_19WB, by = "country_iso3_code") %>%
midas_price$PriceperSU <- (midas_price$PriceperSU * midas_price$PPP_rate)/midas_price$ER_rate


PPP_rate <- midas_price %>%
  group_by(country_iso3_code) %>%
  summarise(PPP_rate = first(PPP_rate)) %>%  
  arrange(country_iso3_code) %>%  # Order results by country_iso3_code
  pull(PPP_rate)  
# Create a string that formats it as an R vector
PPP_rate_list <- paste("c(", paste(PPP_rate, collapse = ", "), ")")
# Print the formatted string
cat(PPP_rate_list)

#######

#--------------------------------------------------- #
#--------------------------------------------------- #
#Descriptive mapping #########
library(ggplot2)
library(rnaturalearth)
library(dplyr)
# Example of WHO region assignments (You should fill these based on actual data)
iso_a3_codes <- c("MAR", "ARE", "ARG", "AUS", "AUT", "BEL", "BGD", "BGR", "BIH", "BLR", "BRA", "CAN", 
                  "CHE", "CHL", "CHN", "COL", "CZE", "DEU", "DOM", "DZA", "ECU", "EGY", "ESP", "EST", 
                  "FIN", "FRA", "GBR", "GRC", "HKG", "HRV", "HUN", "IDN", "IND", "IRL", "ITA", "JOR", 
                  "JPN", "KAZ", "KOR", "KWT", "LBN", "LKA", "LTU", "LUX", "LVA", "MEX", "MYS", "NLD", 
                  "NOR", "NZL", "PAK", "PER", "PHL", "POL", "PRI", "PRT", "ROU", "RUS", "SAU", "SGP", 
                  "SRB", "SVK", "SVN", "SWE", "THA", "TUN", "TUR", "TWN", "URY", "USA", "VEN", "VNM", 
                  "ZAF")

regions <- c("EMRO", "EMRO", "AMRO", "WPRO", "EURO", "EURO", "SEARO", "EURO", "EURO", "EURO", "AMRO", "AMRO",
             "EURO", "AMRO", "WPRO", "AMRO", "EURO", "EURO", "AMRO", "AFRO", "AMRO", "EMRO", "EURO", "EURO", 
             "EURO", "EURO", "EURO", "EURO", "WPRO", "EURO", "EURO", "SEARO", "SEARO", "EURO", "EURO", "EMRO", 
             "WPRO", "EURO", "WPRO", "EMRO", "EMRO", "SEARO", "EURO", "EURO", "EURO", "AMRO", "WPRO", "EURO", 
             "EURO", "WPRO", "EMRO", "AMRO", "WPRO", "EURO", "AMRO", "EURO", "EURO", "EURO", "EMRO", "WPRO", 
             "EURO", "EURO", "EURO", "EURO", "WPRO", "AFRO", "EURO", "WPRO", "AMRO", "AMRO", "AMRO", "WPRO", 
             "AFRO")
who_regions <- data.frame(
  iso_a3 = iso_a3_codes,
  region = regions
)

# Merge with natural earth world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
world$iso_a3[world$admin == "France"] <- "FRA"
world$iso_a3[world$admin == "Norway"] <- "NOR"
world_map <- left_join(world, who_regions, by = "iso_a3")
world_map$region[is.na(world_map$region)] <- "Not Covered"

# Define a color palette for the WHO regions
colors <- c("EMRO" = "#1f77b4", "AMRO" = "#ff7f0e", "WPRO" = "#2ca02c", "EURO" = "#d62728",
            "SEARO" = "#9467bd", "AFRO" = "#8c564b", "Not Covered" = "grey80")

# Create the map with appropriate colors
abc0<- ggplot(data = world_map) +
  geom_sf(aes(fill = region), color = "black", size = 0.25) + 
  scale_fill_manual(values = colors, name = "WHO Region") +
  theme_minimal() +
  labs(title = "Countries covered by IQVIA, by WHO region",
       caption = "")
#ggsave("countriesWHoreginc.tiff", plot = abc0, device = "tiff", width =10, height = 7, dpi = 500, units = "in")

#WB income group
library(WDI)
# Fetch income level data for all countries
income_data <- WDI(indicator = "NY.GNP.PCAP.CD", extra = TRUE, cache = NULL)

# Assume the correct column name is found and proceed with data preparation
income_levels <- income_data %>%
  select(iso2c, income) %>%
  unique()

# Convert ISO2C to ISO3C codes
library(countrycode)
iso_conversion <- countrycode::countrycode(income_levels$iso2c, origin = 'iso2c', destination = 'iso3c')
income_levels$iso3c <- iso_conversion

# Drop the original 2-letter column
income_levels <- income_levels %>% select(-iso2c) %>% distinct()
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge world data with income levels
world$iso_a3[world$admin == "France"] <- "FRA"
world$iso_a3[world$admin == "Norway"] <- "NOR"
world$iso_a3[world$admin == "Czechia"] <- "CHE"
world$iso_a3[world$admin == "Venezuela, RB"] <- "VEN"
world_map <- left_join(world, income_levels, by = c("iso_a3" = "iso3c"))
world_map$income[is.na(world_map$income)] <- "Not Classified"
world_map$income[!world_map$iso_a3 %in% iso_a3_codes] <- "Not Classified"
world_map$income[world_map$iso_a3=="VEN"]<- "Upper middle income"
world_map$income[world_map$iso_a3=="VNM"]<- "Lower middle income"
colors <- c(
  "High income" = "#377eb8",
  "Upper middle income" = "#ff7f0e",
  "Lower middle income" = "#4daf4a",
  "Low income" = "#e41a1c",
  "Not Classified" = "grey80"
)

# Plot the map with the corrected information and colors
abc99<- ggplot(data = world_map) +
  geom_sf(aes(fill = income), color = "black", size = 0.25) +
  scale_fill_manual(values = colors, name = "Income Level") +
  theme_minimal() +
  labs(title = "Countries covered by IQVIA, by income group",
       caption = "")

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
#ggsave("countriesINClev.tiff", plot = abc99, device = "tiff", width =10, height = 7, dpi = 500, units = "in")
#getwd()

library(cowplot)
combined_plotkok <- plot_grid(abc99 + ggtitle("A"), abc0 + ggtitle("B"), ncol = 1, labels = c("", ""))
ggsave("FigureWBandWHO_allincluded.tiff", plot = combined_plotkok, device = "tiff", width =10, height = 10, dpi = 1000, units = "in")


#########
#--------------------------------------------------- #
#--------------------------------------------------- #

#--------------------------------------------------- #
#--------------------------------------------------- #
###PRICE ratio Access-to-Watch #####
weighted_avg_dataMIDAS_prCAw<- midas_price %>%
  group_by(CAw) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), aware_category = first(aware_category), country_iso3_code= first(country_iso3_code), WHO_region= first(WHO_region), WB_income=first(WB_income), WB_incomeshort= first(WB_incomeshort), na.rm = TRUE)

# Filter the data for 'Access' and 'Watch' categories
filtered_data <- weighted_avg_dataMIDAS_prCAw %>%
  filter(aware_category %in% c("Access", "Watch"))

# Calculate the price ratio
price_ratio_data <- filtered_data %>%
  group_by(country_iso3_code) %>%
  summarise(
    Access_Price = sum(PriceperDDD_w[aware_category == "Access"], na.rm = TRUE),
    Watch_Price = sum(PriceperDDD_w[aware_category == "Watch"], na.rm = TRUE), WB_income= first(WB_income), WHO_region= first(WHO_region)
  ) %>%
  mutate(Price_Ratio = Access_Price / Watch_Price) %>%
  dplyr::select(country_iso3_code, Price_Ratio, WHO_region, WB_income) %>%
  filter(!is.na(Price_Ratio))

# Display the resulting dataframe
print(price_ratio_data)


# Create the graph with Lancet style
# Create the graph with Lancet style
access_watch_ratio_plot <- ggplot(price_ratio_data, aes(x = Price_Ratio, y = reorder(country_iso3_code, Price_Ratio))) +
  geom_bar(stat = "identity", aes(fill = Price_Ratio > 1), color = "black") +
  scale_fill_manual(
    name = "Price Ratio",
    values = c("TRUE" = "#E69F00", "FALSE" = "#009E73"), 
    labels = c("Access-to-Watch Ratio <1", "Access-to-Watch Ratio >1")
  ) +
  geom_vline(xintercept = 1, color = "#D55E00", linetype = "solid", size = 1) +
  scale_x_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, by = 0.5)) +
  labs(title = "", x = "Price Ratio (Access/Watch)", y = "Country ISO-3 Code") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.position = c(0.8, 0.2),  # Position legend inside the plot area
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

# Display the graph
print(access_watch_ratio_plot)



# Calculate statistics for WHO_region
who_region_stats <- price_ratio_data %>%
  group_by(WHO_region) %>%
  summarise(
    Mean_ratio = mean(Price_Ratio, na.rm = TRUE),
    p25 = quantile(Price_Ratio, 0.25, na.rm = TRUE),
    p75 = quantile(Price_Ratio, 0.75, na.rm = TRUE),
    IQR = IQR(Price_Ratio, na.rm = TRUE)
  ) %>%
  mutate(Group = "WHO Region") %>%
  rename(`Region / Income Levels` = WHO_region)

# Calculate statistics for WB_income
income_level_stats <- price_ratio_data %>%
  group_by(WB_income) %>%
  summarise(
    Mean_ratio = mean(Price_Ratio, na.rm = TRUE),
    p25 = quantile(Price_Ratio, 0.25, na.rm = TRUE),
    p75 = quantile(Price_Ratio, 0.75, na.rm = TRUE),
    IQR = IQR(Price_Ratio, na.rm = TRUE)
  ) %>%
  mutate(Group = "Income Levels") %>%
  rename(`Region / Income Levels` = WB_income)

# Combine the two tables into one
combined_stats2 <- bind_rows(who_region_stats, income_level_stats) %>%
  dplyr::select(Group, `Region / Income Levels`, everything())

# Display the resulting dataframe
print(combined_stats2)

library(dplyr)
library(knitr)
library(kableExtra)
# Display the combined table
combined_stats2 %>%
  kable(format = "html", table.attr = "class='table table-bordered'", caption = "Price Ratio Statistics by WHO Region and Income Levels") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)



setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("access_watch_ratio_PRICE_plot.tiff", plot = access_watch_ratio_plot, device = "tiff", width =7, height = 11, dpi = 1000, units = "in")

weighted_avg_dataMIDAS_prCAwrEt65<- midas_price %>%
  group_by(CEAw) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditure= sum(total_expenditure), total_ddd_sold = sum(ddd), aware_category = first(aware_category), country_iso3_code= first(country_iso3_code), WHO_region= first(WHO_region), WB_income=first(WB_income), WB_incomeshort= first(WB_incomeshort), EML_book= first(EML_book), na.rm = TRUE)


# Filter for EML_book=0 and calculate price ratios
price_ratio_data_0 <- weighted_avg_dataMIDAS_prCAwrEt65 %>%
  filter(EML_book == 0) %>%
  group_by(country_iso3_code) %>%
  summarise(
    Access_Price = mean(PriceperDDD_w[aware_category == "Access"], na.rm = TRUE),
    Watch_Price = mean(PriceperDDD_w[aware_category == "Watch"], na.rm = TRUE)
  ) %>%
  mutate(Price_Ratio = Access_Price / Watch_Price) %>%
  dplyr::select(country_iso3_code, Price_Ratio) %>%
  filter(!is.na(Price_Ratio))

# Filter for EML_book=1 and calculate price ratios
price_ratio_data_1 <- weighted_avg_dataMIDAS_prCAwrEt65 %>%
  filter(EML_book == 1) %>%
  group_by(country_iso3_code) %>%
  summarise(
    Access_Price = mean(PriceperDDD_w[aware_category == "Access"], na.rm = TRUE),
    Watch_Price = mean(PriceperDDD_w[aware_category == "Watch"], na.rm = TRUE), WB_income= first(WB_income), WHO_region= first(WHO_region) 
  ) %>%
  mutate(Price_Ratio = Access_Price / Watch_Price) %>%
  dplyr::select(country_iso3_code, Price_Ratio, WB_income, WHO_region) %>%
  filter(!is.na(Price_Ratio))

# Display the resulting dataframes
print(price_ratio_data_0)
print(price_ratio_data_1)



# Create the graph with Lancet style
access_watch_ratio_plot_12 <- ggplot(price_ratio_data_1, aes(x = Price_Ratio, y = reorder(country_iso3_code, Price_Ratio))) +
  geom_bar(stat = "identity", aes(fill = Price_Ratio > 1), color = "black") +
  scale_fill_manual(
    name = "Price Ratio",
    values = c("TRUE" = "#E69F00", "FALSE" = "#009E73"), 
    labels = c("Access-to-Watch Ratio < 1", "Access-to-Watch Ratio > 1")
  ) +
  geom_vline(xintercept = 1, color = "#D55E00", linetype = "solid", size = 1) +
  scale_x_continuous(limits = c(0, 4.5), breaks = seq(0, 4.5, by = 0.5)) +
  labs(title = "Access-to-Watch Price Ratio (included on the EML book)", x = "Price Ratio (Access/Watch)", y = "Country ISO3 Code") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.position = c(0.8, 0.9),  # Position legend inside the plot area
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

# Display the graph
print(access_watch_ratio_plot_12)


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("access_watch_ratio_PRICE_plotEML.tiff", plot = access_watch_ratio_plot_12, device = "tiff", width =7, height = 11, dpi = 1000, units = "in")



# Calculate statistics for WHO_region
who_region_stats <- price_ratio_data_1 %>%
  group_by(WHO_region) %>%
  summarise(
    Mean_ratio = mean(Price_Ratio, na.rm = TRUE),
    p25 = quantile(Price_Ratio, 0.25, na.rm = TRUE),
    p75 = quantile(Price_Ratio, 0.75, na.rm = TRUE),
    IQR = IQR(Price_Ratio, na.rm = TRUE)
  ) %>%
  mutate(Group = "WHO Region") %>%
  rename(`Region / Income Levels` = WHO_region)

# Calculate statistics for WB_income
income_level_stats <- price_ratio_data_1 %>%
  group_by(WB_income) %>%
  summarise(
    Mean_ratio = mean(Price_Ratio, na.rm = TRUE),
    p25 = quantile(Price_Ratio, 0.25, na.rm = TRUE),
    p75 = quantile(Price_Ratio, 0.75, na.rm = TRUE),
    IQR = IQR(Price_Ratio, na.rm = TRUE)
  ) %>%
  mutate(Group = "Income Levels") %>%
  rename(`Region / Income Levels` = WB_income)

# Combine the two tables into one
combined_stats <- bind_rows(who_region_stats, income_level_stats) %>%
  dplyr::select(Group, `Region / Income Levels`, everything())

# Display the resulting dataframe
print(combined_stats)

library(dplyr)
library(knitr)
library(kableExtra)
# Display the combined table
combined_stats %>%
  kable(format = "html", table.attr = "class='table table-bordered'", caption = "Price Ratio Statistics by WHO Region and Income Levels") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)




######
#--------------------------------------------------- #
#--------------------------------------------------- #

#-------------------------------------------------------------#
#EXTERNAL: Data validation with OpenPrescribing UK.  ---------#
#-------------------------------------------------------------#
#######
library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(writexl)
fetch_tariff <- function(bnf_code) {
  base_url <- "https://openprescribing.net/api/1.0/"
  endpoint <- paste0("tariff/?format=json&bnf_code=", bnf_code)
  url <- paste0(base_url, endpoint)
  
  response <- GET(url)
  
  if (status_code(response) == 200) {
    data <- content(response, as = "text")
    df <- fromJSON(data, flatten = TRUE)
    return(df)
  } else {
    stop("Failed to fetch data")
  }
}
# Example BNF codes for antibiotics
antibiotics_bnf_codes <- c("0501013B0", "0501070T0")  # Replace with actual BNF codes
# Fetch tariff data for each antibiotic
tariff_data <- lapply(antibiotics_bnf_codes, function(code) {
  fetch_tariff(code)
})
# Combine the results into a single data frame
combined_tariff_data <- do.call(rbind, tariff_data)
# View the combined data
head(combined_tariff_data)
combined_tariff_data2019 <- combined_tariff_data[substr(combined_tariff_data$date, 1, 4) == "2019", ]
# Splitting the vmpp column into multiple new columns
combined_tariff_data2019 <- combined_tariff_data2019 %>%
  tidyr::separate(vmpp, into = c("Antibiotic_Name", "Dosage", "Format_Quantity"), sep = " ", extra = "merge", remove = FALSE)
# Further separate the Format_Quantity if it's consistently formatted as "[number] [format]"
combined_tariff_data2019 <- combined_tariff_data2019 %>%
  tidyr::separate(Format_Quantity, into = c("Quantity", "Format"), sep = "\\s(?=[^\\s]+$)")

antibiotics <- c("Aciclovir", "Amoxicillin", "Ampicillin", "Azithromycin", 
                 "Cefaclor", "Cefadroxil", "Cefalexin", "Cefixime", "Cefradine", 
                 "Ceftriaxone", "Cefuroxime", "Chloramphenicol", "Ciprofloxacin", 
                 "Clarithromycin", "Clindamycin", "Co-amoxiclav", "Doxycycline", 
                 "Erythromycin", "Flucloxacillin", "Gentamicin", "Linezolid", 
                 "Meropenem", "Metronidazole", "Minocycline", "Neomycin", 
                 "Nitrofurantoin", "Norfloxacin", "Penicillamine", "Piperacillin", 
                 "Rifabutin", "Rifampicin", "Sulfasalazine", "Tetracycline", 
                 "Trimethoprim", "Vancomycin")

# Filter the dataframe to include only rows where Antibiotic_Name is one of the antibiotics
combined_tariff_data2019a <- combined_tariff_data2019 %>%
  filter(Antibiotic_Name %in% antibiotics)
DDD_oral <- c(5, 1, 2, 0.5, 1, 2, 2, 0.4, 2, NA, 1.5, 3, 1, 1, 1.8, 1.5, 0.1, 1, 3, NA, 1.2, NA, 1.5, 0.1, NA, 0.3, 0.8, NA, NA, NA, 0.6, 2, 1, 0.2, NA) #DDD in grams
DDD_parenteral <- c(10, NA, 6, NA, NA, NA, NA, NA, NA, 2, 3, NA, 0.8, NA, 2.7, NA, NA, NA, NA, 0.24, 1.2, 3, 0.5, NA, NA, NA, NA, NA, 12, 0.3, NA, NA, NA, NA, 2) #DDD in grams

antibiotic_dataf <- data.frame(
  Antibiotic_Name = antibiotics,
  DDD_Oral = DDD_oral,
  DDD_Parenteral = DDD_parenteral
)

merged_data <- combined_tariff_data2019a %>%
  left_join(antibiotic_dataf, by = "Antibiotic_Name")

merged_data <- merged_data %>%
  mutate(dosage = as.character(Dosage)) %>%  # Ensure dosage is character type for manipulation
  separate(dosage, into = c("Dosage_Amount", "Dosage_Unit"), sep = "(?<=\\d)(?=[a-zA-Z])", convert = TRUE)

merged_data <- merged_data %>%
  mutate(
    Dosage_Amount = as.numeric(as.character(Dosage_Amount)),
    Dosage_Unit = as.character(Dosage_Unit)
  ) %>%
  mutate(
    Dosage_Amount = ifelse(Dosage_Unit == "g" & !is.na(Dosage_Amount), Dosage_Amount * 1000, Dosage_Amount),
    Dosage_Unit = ifelse(Dosage_Unit == "g", "mg", Dosage_Unit)
  )

write_xlsx(merged_data, path = "merged_data.xlsx")

filtered_midas_price <- midas_price %>%
  filter(country == "united kingdom" & adila_antimicrobials %in% antibiotics)
write_xlsx(filtered_midas_price, path = "filtered_midas_priceUK.xlsx")
#######
#-------------------------------------------------------------#
#Antibiotic treatment course; Oral antibiotics:
#-------------------------------------------------------------#
#######
library(tibble)
# Create a dataframe
antibiotics_data <- tibble(
  adila_antimicrobials = c(
    "Amoxicillin", 
    "Amoxicillin+Clavulanic acid",
    "Doxycycline", 
    "Metronidazole", 
    "Nitrofurantoin", 
    "Co-trimoxazole",
    "Ciprofloxacin", 
    "Azithromycin", 
    "Cefalexin", 
    "Clindamycin", 
    "Cloxacillin", 
    "Phenoxymethylpenicillin", 
    "Trimephoprim", 
    "Cefixime", 
    "Clarithomycin", 
    "Linezolid"
  ),
  Treatmentddd = c(
    7.5,
    7.5,
    10,
    10, 
    5, 
     4, 
    14,
    6.666666667,
    3.75, 
    42, 
    5, 
    10, 
    3, 
    3, 
    10, 
    10
  )
)

weighted_avg_dataMIDAS_prCAA <- midas_price %>%
  group_by(CAA) %>%
  summarise(PriceperDDD_w = sum(total_expenditure) / sum(ddd), total_expenditureCAA= sum(total_expenditure), total_ddd_soldCAA = sum(ddd), aware_category=first(aware_category), country_iso3_code=first(country_iso3_code), WHO_region= first(WHO_region), WB_income= first(WB_income), route_of_administration=first(route_of_administration), adila_antimicrobials=first(adila_antimicrobials), na.rm = TRUE)

weighted_avg_dataMIDAS_prCAA_c <- weighted_avg_dataMIDAS_prCAA %>%
  filter(route_of_administration == "Oral")

weighted_avg_dataMIDAS_prCAA_c <- weighted_avg_dataMIDAS_prCAA_c %>%
  left_join(antibiotics_data, by = "adila_antimicrobials")%>%
  filter(!is.na(Treatmentddd))

weighted_avg_dataMIDAS_prCAA_c$cost_courses <- weighted_avg_dataMIDAS_prCAA_c$Treatmentddd * weighted_avg_dataMIDAS_prCAA_c$PriceperDDD_w


#GRAPHS ---


library(ggthemes)
# List of unique values in adila_antimicrobials
unique_antimicrobials <- unique(weighted_avg_dataMIDAS_prCAA_c$adila_antimicrobials)

theme_lancet_custom <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA)
    )
}

# Plot for Amoxicillin
plot_data_amoxicillin <- weighted_avg_dataMIDAS_prCAA_c %>%
  filter(adila_antimicrobials == "Amoxicillin") %>%
  arrange(cost_courses)

cost_course_amoxicillin<-ggplot(plot_data_amoxicillin, aes(x = reorder(country_iso3_code, cost_courses), y = cost_courses, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("AFR" = "#1b9e77", "AMR" = "#d95f02", "EMR" = "#7570b3", "EUR" = "#377eb8", "SEAR" = "#66a61e", "WPR" = "#e6ab02")) +
  #scale_y_continuous(breaks = seq(0, 33, by = 1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(
    title = "",
    x = "Country ISO-3 Code",
    y = "Cost course for Amoxicillin 500mg every 8 hours for 5 days (int$)",
    fill = "WHO Region"
  )

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("cost_course_amoxicillin.tiff", plot = cost_course_amoxicillin, device = "tiff", width =13, height = 8, dpi = 1000, units = "in")




# Plot for Amoxicillin+Clavulanic acid
plot_data_AmoxicillinClavulanicacid <- weighted_avg_dataMIDAS_prCAA_c %>%
  filter(adila_antimicrobials == "Amoxicillin+Clavulanic acid") %>%
  arrange(cost_courses)

cost_course_amoxicillincluvacid<-ggplot(plot_data_AmoxicillinClavulanicacid, aes(x = reorder(country_iso3_code, cost_courses), y = cost_courses, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("AFR" = "#1b9e77", "AMR" = "#d95f02", "EMR" = "#7570b3", "EUR" = "#377eb8", "SEAR" = "#66a61e", "WPR" = "#e6ab02")) +
  #scale_y_continuous(breaks = seq(0, 28, by = 2)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(
    title = "",
    x = "Country ISO-3 Code",
    y = "Cost course for Amoxicillin+Clavulanic acid 500mg + 125mg every 8 hours for 5 days (int$)",
    fill = "WHO Region"
  )


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("cost_course_amoxicillincluvacid.tiff", plot = cost_course_amoxicillincluvacid, device = "tiff", width =13, height = 8, dpi = 1000, units = "in")






# Plot for Doxycycline
plot_data_Doxycycline <- weighted_avg_dataMIDAS_prCAA_c %>%
  filter(adila_antimicrobials == "Doxycycline") %>%
  arrange(cost_courses)

cost_course_Doxycycline<-ggplot(plot_data_Doxycycline, aes(x = reorder(country_iso3_code, cost_courses), y = cost_courses, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("AFR" = "#1b9e77", "AMR" = "#d95f02", "EMR" = "#7570b3", "EUR" = "#377eb8", "SEAR" = "#66a61e", "WPR" = "#e6ab02")) +
  #scale_y_continuous(breaks = seq(0, 28, by = 2)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(
    title = "",
    x = "Country ISO-3 Code",
    y = "Cost course for Doxycycline 100mg every 12 hours for 5 days (int$)",
    fill = "WHO Region"
  )


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("cost_course_Doxycycline.tiff", plot = cost_course_Doxycycline, device = "tiff", width =13, height = 8, dpi = 1000, units = "in")

# Plot for Nitrofurantoin
plot_data_Nitrofurantoin <- weighted_avg_dataMIDAS_prCAA_c %>%
  filter(adila_antimicrobials == "Nitrofurantoin") %>%
  arrange(cost_courses)

cost_course_Nitrofurantoin<-ggplot(plot_data_Nitrofurantoin, aes(x = reorder(country_iso3_code, cost_courses), y = cost_courses, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("AFR" = "#1b9e77", "AMR" = "#d95f02", "EMR" = "#7570b3", "EUR" = "#377eb8", "SEAR" = "#66a61e", "WPR" = "#e6ab02")) +
  #scale_y_continuous(breaks = seq(0, 28, by = 2)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(
    title = "",
    x = "Country ISO-3 Code",
    y = "Cost course for Nitrofurantoin 50mg every 6 hours for 5 days (int$)",
    fill = "WHO Region"
  )


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("cost_course_Nitrofurantoin.tiff", plot = cost_course_Nitrofurantoin, device = "tiff", width =13, height = 8, dpi = 1000, units = "in")

# Plot for Ciprofloxacin
plot_data_Ciprofloxacin<- weighted_avg_dataMIDAS_prCAA_c %>%
  filter(adila_antimicrobials == "Ciprofloxacin") %>%
  arrange(cost_courses)

cost_course_Ciprofloxacin<-ggplot(plot_data_Ciprofloxacin, aes(x = reorder(country_iso3_code, cost_courses), y = cost_courses, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("AFR" = "#1b9e77", "AMR" = "#d95f02", "EMR" = "#7570b3", "EUR" = "#377eb8", "SEAR" = "#66a61e", "WPR" = "#e6ab02")) +
  #scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(
    title = "",
    x = "Country ISO-3 Code",
    y = "Cost course for Ciprofloxacin 500mg every 12 hours for 7 days (int$)",
    fill = "WHO Region"
  )


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("cost_course_Ciprofloxacin.tiff", plot = cost_course_Ciprofloxacin, device = "tiff", width =13, height = 8, dpi = 1000, units = "in")

# Plot for Azithromycin
plot_data_Azithromycin<- weighted_avg_dataMIDAS_prCAA_c %>%
  filter(adila_antimicrobials == "Azithromycin") %>%
  arrange(cost_courses)

cost_course_Azithromycin<-ggplot(plot_data_Azithromycin, aes(x = reorder(country_iso3_code, cost_courses), y = cost_courses, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("AFR" = "#1b9e77", "AMR" = "#d95f02", "EMR" = "#7570b3", "EUR" = "#377eb8", "SEAR" = "#66a61e", "WPR" = "#e6ab02")) +
  #scale_y_continuous(breaks = seq(0, 26, by = 2)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(
    title = "",
    x = "Country ISO-3 Code",
    y = "Cost course for Azithromycin 500mg single dose for 4 days (int$)",
    fill = "WHO Region"
  )


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("cost_course_Azithromycin.tiff", plot = cost_course_Azithromycin, device = "tiff", width =13, height = 8, dpi = 1000, units = "in")



# Plot for Cefalexin
plot_data_Cefalexin<- weighted_avg_dataMIDAS_prCAA_c %>%
  filter(adila_antimicrobials == "Cefalexin") %>%
  arrange(cost_courses)

cost_course_Cefalexin<-ggplot(plot_data_Cefalexin, aes(x = reorder(country_iso3_code, cost_courses), y = cost_courses, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("AFR" = "#1b9e77", "AMR" = "#d95f02", "EMR" = "#7570b3", "EUR" = "#377eb8", "SEAR" = "#66a61e", "WPR" = "#e6ab02")) +
  #scale_y_continuous(breaks = seq(0, 26, by = 2)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(
    title = "",
    x = "Country ISO-3 Code",
    y = "Cost course for Cefalexin 500mg every 8 hours for 5 days (int$)",
    fill = "WHO Region"
  )


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("cost_course_Cefalexin.tiff", plot = cost_course_Cefalexin, device = "tiff", width =13, height = 8, dpi = 1000, units = "in")



# Plot for Clindamycin
plot_data_Clindamycin<- weighted_avg_dataMIDAS_prCAA_c %>%
  filter(adila_antimicrobials == "Clindamycin") %>%
  arrange(cost_courses)

cost_course_Clindamycin<-ggplot(plot_data_Clindamycin, aes(x = reorder(country_iso3_code, cost_courses), y = cost_courses, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("AFR" = "#1b9e77", "AMR" = "#d95f02", "EMR" = "#7570b3", "EUR" = "#377eb8", "SEAR" = "#66a61e", "WPR" = "#e6ab02")) +
  #scale_y_continuous(breaks = seq(0, 190, by = 10)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(
    title = "",
    x = "Country ISO-3 Code",
    y = "Cost course for Clindamycin 600mg every 8 hours for 4 weeks (int$)",
    fill = "WHO Region"
  )


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("cost_course_Clindamycin.tiff", plot = cost_course_Clindamycin, device = "tiff", width =13, height = 8, dpi = 1000, units = "in")


# Plot for Phenoxymethylpenicillin
plot_data_Phenoxymethylpenicillin<- weighted_avg_dataMIDAS_prCAA_c %>%
  filter(adila_antimicrobials == "Phenoxymethylpenicillin") %>%
  arrange(cost_courses)

cost_course_Phenoxymethylpenicillin<-ggplot(plot_data_Phenoxymethylpenicillin, aes(x = reorder(country_iso3_code, cost_courses), y = cost_courses, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("AFR" = "#1b9e77", "AMR" = "#d95f02", "EMR" = "#7570b3", "EUR" = "#377eb8", "SEAR" = "#66a61e", "WPR" = "#e6ab02")) +
  #scale_y_continuous(breaks = seq(0, 30, by = 2)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(
    title = "",
    x = "Country ISO-3 Code",
    y = "Cost course for Phenoxymethylpenicillin 500mg every 6 hours for 5 days (int$)",
    fill = "WHO Region"
  )


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("cost_course_Phenoxymethylpenicillin.tiff", plot = cost_course_Phenoxymethylpenicillin, device = "tiff", width =13, height = 8, dpi = 1000, units = "in")


# Plot for Cefixime
plot_data_Cefixime<- weighted_avg_dataMIDAS_prCAA_c %>%
  filter(adila_antimicrobials == "Cefixime") %>%
  arrange(cost_courses)

cost_course_Cefixime<-ggplot(plot_data_Cefixime, aes(x = reorder(country_iso3_code, cost_courses), y = cost_courses, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("AFR" = "#1b9e77", "AMR" = "#d95f02", "EMR" = "#7570b3", "EUR" = "#377eb8", "SEAR" = "#66a61e", "WPR" = "#e6ab02")) +
  #scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(
    title = "",
    x = "Country ISO-3 Code",
    y = "Cost course for Cefixime 400mg single dose for 3 days (int$)",
    fill = "WHO Region"
  )


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("cost_course_Cefixime.tiff", plot = cost_course_Cefixime, device = "tiff", width =13, height = 8, dpi = 1000, units = "in")

# Plot for Linezolid
plot_data_Linezolid<- weighted_avg_dataMIDAS_prCAA_c %>%
  filter(adila_antimicrobials == "Linezolid") %>%
  arrange(cost_courses)

cost_course_Linezolid<-ggplot(plot_data_Linezolid, aes(x = reorder(country_iso3_code, cost_courses), y = cost_courses, fill = WHO_region)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("AFR" = "#1b9e77", "AMR" = "#d95f02", "EMR" = "#7570b3", "EUR" = "#377eb8", "SEAR" = "#66a61e", "WPR" = "#e6ab02")) +
  scale_y_continuous(breaks = seq(0, 10000, by = 500)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(
    title = "",
    x = "Country ISO-3 Code",
    y = "Cost course for Linezolid 600mg every 12 hours for 10 days (int$)",
    fill = "WHO Region"
  )


setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("cost_course_Linezolid.tiff", plot = cost_course_Linezolid, device = "tiff", width =13, height = 8, dpi = 1000, units = "in")


#SUMMAry sTATS TABLE for ATB treatment courses:

dataframes <- list(
  plot_data_amoxicillin = plot_data_amoxicillin,
  plot_data_AmoxicillinClavulanicacid = plot_data_AmoxicillinClavulanicacid,
  plot_data_Doxycycline = plot_data_Doxycycline,
  plot_data_Nitrofurantoin = plot_data_Nitrofurantoin,
  plot_data_Ciprofloxacin = plot_data_Ciprofloxacin,
  plot_data_Azithromycin = plot_data_Azithromycin,
  plot_data_Cefalexin = plot_data_Cefalexin,
  plot_data_Clindamycin = plot_data_Clindamycin,
  plot_data_Phenoxymethylpenicillin = plot_data_Phenoxymethylpenicillin,
  plot_data_Cefixime = plot_data_Cefixime,
  plot_data_Linezolid = plot_data_Linezolid
)

# Function to compute summary statistics
compute_summary <- function(df) {
  median <- median(df$cost_courses, na.rm = TRUE)
  p25 <- quantile(df$cost_courses, 0.25, na.rm = TRUE)
  p75 <- quantile(df$cost_courses, 0.75, na.rm = TRUE)
  iqr <- IQR(df$cost_courses, na.rm = TRUE)
  return(data.frame(Median = median, p25 = p25, p75 = p75, IQR = iqr))
}

# Compute summary for each dataframe
summary_table22g <- bind_rows(lapply(dataframes, compute_summary), .id = "Dataframe")

# Print the summary table
print(summary_table22g)
write_xlsx(summary_table22g, "summary_table.xlsx")





# List of dataframes
dataframes <- list(
  plot_data_amoxicillin = plot_data_amoxicillin,
  plot_data_AmoxicillinClavulanicacid = plot_data_AmoxicillinClavulanicacid,
  plot_data_Doxycycline = plot_data_Doxycycline,
  plot_data_Nitrofurantoin = plot_data_Nitrofurantoin,
  plot_data_Ciprofloxacin = plot_data_Ciprofloxacin,
  plot_data_Azithromycin = plot_data_Azithromycin,
  plot_data_Cefalexin = plot_data_Cefalexin,
  plot_data_Clindamycin = plot_data_Clindamycin,
  plot_data_Phenoxymethylpenicillin = plot_data_Phenoxymethylpenicillin,
  plot_data_Cefixime = plot_data_Cefixime,
  plot_data_Linezolid = plot_data_Linezolid
)

# Initialize the combined dataframe with the first dataframe in the list
combined_data <- dataframes[[1]] %>%
  dplyr::select(country_iso3_code, cost_courses) %>%
  rename(cost_course_plot_data_amoxicillin = cost_courses)

# Merge each subsequent dataframe
for (name in names(dataframes)[-1]) {
  combined_data <- combined_data %>%
    full_join(dataframes[[name]] %>%
                dplyr::select(country_iso3_code, cost_courses) %>%
                rename(!!paste0("cost_course_", name) := cost_courses), 
              by = "country_iso3_code")
}

# Print the combined dataframe
print(combined_data)
write_xlsx(combined_data, "country_treatmentcourses.xlsx")

#######

wide_dataPov_oral <- weighted_avg_dataMIDAS_prCAA_c %>%
  pivot_wider(
    id_cols = country_iso3_code, # Ensures data is grouped by this key
    names_from = adila_antimicrobials,           # Unique antimicrobial subcategories
    values_from = cost_courses  # Values to populate the new columns
  )
# View the transformed data
View(wide_dataPov_oral)

###MODEL CDF Income for 7-day ATB courses for unknown sepsis adults/neonates per year#####
# Load necessary packages
library(readxl)
library(dplyr)
library(purrr)
library(fitdistrplus)
library(GB2)
library(GB2group)
#https://cran.r-project.org/web/packages/GB2group/GB2group.pdf

# Load the WIID data from the specific sheet
# Define the antibiotic course columns (which are prices)
country_iso3_code <- c("ARE", "ARG", "AUS", "AUT", "BEL", "BGD", "BGR", "BIH", "BLR", "BRA", 
                       "CAN", "CHE", "CHL", "CHN", "COL", "CZE", "DEU", "DOM", "DZA", "ECU", 
                       "EGY", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HKG", "HRV", "HUN", 
                       "IDN", "IND", "IRL", "ITA", "JOR", "JPN", "KAZ", "KOR", "KWT", "LBN", 
                       "LKA", "LTU", "LUX", "LVA", "MAR", "MEX", "MYS", "NLD", "NOR", "NZL", 
                       "PAK", "PER", "PHL", "POL", "PRI", "PRT", "ROU", "RUS", "SAU", "SGP", 
                       "SRB", "SVK", "SVN", "SWE", "THA", "TUN", "TUR", "TWN", "URY", "USA", 
                       "VEN", "VNM", "ZAF")

# Create a vector for GDP PPP 2019 values
gdp_ppp_2019 <- c(74035, 20482, 51885, 55896, 54098, 5139, 24576, 15964, 19997, 16784, 
                  50001, 73703, 26318, 16804, 16264, 40793, 54075, 18535, 13800, 11582, 
                  12668, 41650, 38906, 49032, 48173, 47905, 30252, 62839, 29460, 35942, 
                  13036, 7034, 85267, 43795, 10794, 44227, 28849, 44652, 71263, 11264, 
                  12564, 39610, 117558, 35701, 9351, 19421, 30049, 58846, 63863, 44943, 
                  5705, 13933, 10080, 34057, 39915, 35000, 31173, 28181, 47817, 97057, 
                  18564, 37789, 40742, 53652, 19499, 11973, 28041, 47800, 23790, 65297, 
                  5443, 8131, 13446)

# Create a dataframe
gdp_ppp_df <- data.frame(country_iso3_code, gdp_ppp_2019)


course_columns <- c("Amoxicillin+Clavulanic acid", "Amoxicillin", "Azithromycin", "Cefalexin", "Cefixime", "Ciprofloxacin", "Cloxacillin", "Doxycycline", "Phenoxymethylpenicillin", "Clindamycin", "Linezolid")
wiid_data <- readxl::read_excel("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_Projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/WIID_28NOV2023.xlsx", sheet = "data_wiib")
poverty_lines<- read.csv(file= "/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/OneDrive_24/data_povertyline.csv")

#DATA MANAGEMENT AND TEST DATA:
# Normalize income share to sum to 1
wiid_data <- wiid_data %>%
  group_by(country_iso3_code) %>%
  mutate(income_share = income_share / sum(income_share))

merged_data <- dplyr::left_join(wiid_data, data_2019, by = "country_iso3_code")
merged_data <-   dplyr::left_join(wiid_data, data_2019, by = "country_iso3_code") 
merged_data$gini <- as.numeric(merged_data$gini)
merged_data$gini <- merged_data$gini/100
merged_data <- dplyr::left_join(merged_data, ER_ppp_19WB, by = "country_iso3_code")
merged_data$GDP_pc <- as.numeric(merged_data$GDP_pc_PPP19) 
#PERHAPS WE SHOULD UPDATE THE BELOW TO DAILY GDP_pc, rather than an annual figure?
merged_data$GDP_pc <- merged_data$GDP_pc

merged_data <- merged_data %>%
  dplyr::select(country_iso3_code, income_share, group, GDP_pc, gini)

# Merge in PPP data to adjust GDP per capita
merged_data <- dplyr::left_join(merged_data, ppp_2019_iso, by = "country_iso3_code")
# Adjust the GDP per capita by the PPP rate to align it with the 2019 USD PPP antibiotic prices
merged_data <- merged_data %>%
  mutate(GDP_pc_adjusted = GDP_pc)
# Merge `wiid_data` and `new_atm_sepsis` based on country code
merged_data <- dplyr::left_join(merged_data, wide_dataPov_oral, by = "country_iso3_code")
# Select relevant columns including the PPP-adjusted GDP per capita
merged_data <- merged_data %>%
  dplyr::select(country_iso3_code, income_share, GDP_pc_adjusted, gini, all_of(course_columns))


merged_data <- merged_data %>%
  mutate(
    gini = case_when(
      country_iso3_code == "BGD" ~ 0.576,
      country_iso3_code == "KOR" ~ 0.32,
      TRUE ~ gini  # Keep the original gini value for other countries
    )
  )

# Assuming merged_data already exists and contains a column country_iso3_code
merged_data$GDP_pc_adjusted[merged_data$country_iso3_code == "VEN"] <- 2624.41

# Define a function to fit the GB2 model
fit_gb2_model <- function(data) {
  fitgroup.gb2(y = data$income_share, gini.e = data$gini[1], pc.inc = data$GDP_pc_adjusted[1])
}
# Define a function to calculate the CDF at a given threshold
calculate_cdf <- function(fit, threshold) {
  if (is.null(fit) || length(fit$omd.estimation) == 0) {
    return(NA)
  }
  tryCatch({
    params <- fit$omd.estimation["Coef.", ]
    shape1 <- params["a"]
    shape2 <- params["b"]
    shape3 <- params["p"]
    rate <- params["q"]
    cdf_value <- pgb2(threshold, shape1, shape2, shape3, rate)
    return(cdf_value)
  }, error = function(e) {
    warning(paste("Error calculating CDF for threshold:", threshold, ":", e$message))
    return(NA)
  })
}

# Fit the GB2 model and calculate the CDF for each country
result <- merged_data %>%
  group_by(country_iso3_code) %>%
  summarize(gb2_fit = list(fit_gb2_model(cur_data())))

# Add antibiotic course prices to the result
new_atm_sepsis <- wide_dataPov_oral %>%
  left_join(ppp_2019_iso, by = "country_iso3_code")
result <- result %>%
  left_join(new_atm_sepsis, by = "country_iso3_code")
unique_gdp_data <- merged_data %>%
  dplyr::select(country_iso3_code, GDP_pc_adjusted) %>%
  distinct(country_iso3_code, .keep_all = TRUE)

result <- result %>%
  left_join(unique_gdp_data, by = "country_iso3_code")


# Define the list of course variables
course_variables <- c("Amoxicillin+Clavulanic acid", "Amoxicillin", "Azithromycin", "Cefalexin", "Cefixime", "Ciprofloxacin", "Cloxacillin", "Doxycycline", "Phenoxymethylpenicillin", "Clindamycin", "Linezolid")

# Loop over each variable and create a new dataframe
for (course in course_variables) {
  # Create a dynamic name for the result dataframe
  result_name <- paste0("result_", course)
  # Create the new dataframe with the respective course variable
  assign(result_name, 
         result %>%
           mutate(
             # Calculate treatment cost as a percentage of GDP per capita
             treatment_cost_as_percent_income = (!!sym(course) / GDP_pc_adjusted) * 100,
             
             # Create thresholds for 10%, 20%, 30%, and 40% of income spent on treatment
             threshold_10pct = treatment_cost_as_percent_income * 10,  # Represents spending 10% of income
             threshold_20pct = treatment_cost_as_percent_income * 5,   # Represents spending 20% of income
             threshold_30pct = treatment_cost_as_percent_income * (10 / 3),  # Represents spending 30% of income
             threshold_40pct = treatment_cost_as_percent_income * 2.5,  # Represents spending 40% of income
             
             # Calculate the percentage of the population whose spending exceeds 10%, 20%, 30%, and 40% of their income
             percentage_below_10pct = purrr::map2_dbl(gb2_fit, threshold_10pct, ~ calculate_cdf(..1, ..2) * 100),
             percentage_below_20pct = purrr::map2_dbl(gb2_fit, threshold_20pct, ~ calculate_cdf(..1, ..2) * 100),
             percentage_below_30pct = purrr::map2_dbl(gb2_fit, threshold_30pct, ~ calculate_cdf(..1, ..2) * 100),
             percentage_below_40pct = purrr::map2_dbl(gb2_fit, threshold_40pct, ~ calculate_cdf(..1, ..2) * 100)
           ) %>%
           # Select only the necessary columns
           dplyr::select(country_iso3_code, WB_incomeshort, WHO_region, 
                         percentage_below_10pct, percentage_below_20pct, 
                         percentage_below_30pct, percentage_below_40pct)
  )
}


#GRAPHS:

# Function to prepare and label the data for each treatment
prepare_data_for_plot <- function(data, treatment_label) {
  # Filter for countries with at least 1% in percentage_below_10pct
  filtered_data <- data %>%
    filter(percentage_below_10pct >= 0.00001) %>%
    # Gather the percentage columns for easier plotting
    gather(key = "threshold", value = "percentage", 
           percentage_below_10pct, 
           percentage_below_20pct, 
           percentage_below_30pct, 
           percentage_below_40pct) %>%
    mutate(threshold = factor(threshold, 
                              levels = c("percentage_below_40pct", 
                                         "percentage_below_30pct", 
                                         "percentage_below_20pct", 
                                         "percentage_below_10pct"),
                              labels = c("40%, OOPE", "30%, CHE", "20%, CHE", "10%")),
           treatment = treatment_label) # Add treatment label for grouping
  return(filtered_data)
}

# Prepare data for each treatment
data1 <- prepare_data_for_plot(result_Amoxicillin, "Treatment group 1: Amoxicillin")
data2 <- prepare_data_for_plot(`result_Amoxicillin+Clavulanic acid`, "Treatment group 2: Amoxicillin+Clavulanic acid")
data3 <- prepare_data_for_plot(result_Azithromycin, "Treatment group 3: Azithromycin")
data4 <- prepare_data_for_plot(result_Cefalexin , "Treatment group 4: Cefalexin ")
data5 <- prepare_data_for_plot(result_Cefixime , "Treatment group 5: Cefixime ")
data6 <- prepare_data_for_plot(result_Ciprofloxacin, "Treatment group 6: Ciprofloxacin")
data7 <- prepare_data_for_plot(result_Cloxacillin, "Treatment group 7: Cloxacillin")
data8 <- prepare_data_for_plot(result_Doxycycline, "Treatment group 8: Doxycycline ")
data9 <- prepare_data_for_plot(result_Phenoxymethylpenicillin , "Treatment group 9: Phenoxymethylpenicillin")
data10 <- prepare_data_for_plot(result_Clindamycin , "Treatment group 10: Clindamycin")
data11 <- prepare_data_for_plot(result_Linezolid, "Treatment group 11: Linezolid ")


# Combine all datasets
combined_data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11)

# Add a blank line between treatments by adding an empty row
add_blank_lines <- function(data, treatment_label) {
  empty_row <- tibble(
    country_iso3_code = "",  # Empty country to create space
    threshold = "10%",  # Pick one of the threshold values
    percentage = 0,  # Set to 0 for no actual bar
    treatment = treatment_label  # Ensure it's aligned with the proper section
  )
  return(bind_rows(data, empty_row))
}

# Loop over each treatment and manually add blank lines between groups
combined_data_with_blanks <- bind_rows(
  add_blank_lines(data1, "Treatment group 1: Amoxicillin"),
  add_blank_lines(data2, "Treatment group 2: Amoxicillin+Clavulanic acid"),
  add_blank_lines(data3, "Treatment group 3: Azithromycin"),
  add_blank_lines(data4, "Treatment group 4: Cefalexin"),
  add_blank_lines(data5, "Treatment group 5: Cefixime"),
  add_blank_lines(data6, "Treatment group 6: Ciprofloxacin"),
  add_blank_lines(data7, "Treatment group 7: Cloxacillin"),
  add_blank_lines(data8, "Treatment group 8: Doxycycline "),
  add_blank_lines(data9, "Treatment group 9: Phenoxymethylpenicillin"),
  add_blank_lines(data10, "Treatment group 10: Clindamycin"),
  add_blank_lines(data11, "Treatment group 11: Linezolid")
)


# Create the stacked bar plot with inverted axis and subtitles for treatments
combined_data_with_blanks$percentage <- ifelse(combined_data_with_blanks$percentage > 100, 100, combined_data_with_blanks$percentage)
library(dplyr)

# Function to cap the values and recalculate the lower thresholds
recalculate_thresholds <- function(data) {
  data <- data %>%
    group_by(country_iso3_code, treatment) %>%
    arrange(desc(threshold)) %>%
    mutate(
      cumulative_sum = cumsum(percentage),
      # Adjust values based on the cumulative sum, handling 40% and 30% thresholds
      percentage_adjusted = case_when(
        cumulative_sum > 100 & threshold == "30%, CHE" ~ 100 - sum(percentage[threshold == "40%, OOPE"]),  # Adjust 30% to fit within 100% after 40%
        cumulative_sum > 100 & threshold == "20%, CHE" ~ 100 - sum(percentage[threshold %in% c("40%, OOPE", "30%, CHE")]),  # Adjust 20% to fit within 100% after 40% and 30%
        cumulative_sum > 100 & threshold == "10%" ~ 100 - sum(percentage[threshold %in% c("40%, OOPE", "30%, CHE", "20%, CHE")]),  # Adjust 10% to fit within 100% after higher thresholds
        TRUE ~ percentage  # Keep original percentage if no adjustment is needed
      ),
      # Ensure values are capped at 0 if recalculated percentage goes negative
      percentage_adjusted = pmax(0, percentage_adjusted)
    ) %>%
    ungroup()
  
  return(data)
}

# Apply the function to your dataset
combined_data_with_blanks_adjusted <- recalculate_thresholds(combined_data_with_blanks)

# Create the stacked bar plot with recalculated values
adult_treatment1 <- ggplot(combined_data_with_blanks_adjusted, aes(y = reorder(country_iso3_code, percentage_adjusted), x = percentage_adjusted, fill = threshold)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +  # Add black contour
  facet_wrap(~ treatment, scales = "free_y", ncol = 1, strip.position = "top") +
  scale_fill_manual(
    values = c("#F0E442", "#E69F00", "#D55E00", "#0072B2", "#56B4E9", "#009E73", "#CC79A7", "#999999", "#000000", "#F7D1D1", "#A6D854"), 
    name = "Income Threshold"
  ) +
  labs(x = "Percentage of the population (%)", y = "Country ISO-3 code", title = "") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1, vjust = 1, size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.text.x = element_text(hjust = 0, face = "bold", size = 11),
    strip.placement = "outside"
  ) +
  # coord_cartesian(xlim = c(0, 100)) +  # Ensure the x-axis stays between 0 and 100%
  guides(fill = guide_legend(title = "Spending threshold", position = "inside")) +
  theme(legend.position = c(0.9, 0.82))  # Position the legend inside

# Save the plot as a TIFF file
ggsave("adult_treatment1_cheOOPE_oral.tiff", plot = adult_treatment1, device = "tiff", width = 7, height = 10, dpi = 500, units = "in")



#####

####POVERTY LINES AND EXTREME POVErty: IMPOVERISHMENT #########
library(purrr)
poverty_lines<- read.csv(file= "/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/OneDrive_24/data_povertyline.csv")
new_atm_sepsis <- merge(new_atm_sepsis, poverty_lines, by = "country_iso3_code", all.x = TRUE)
#CHECK THIS HERE BELOW:
#new_atm_sepsis <- new_atm_sepsis[new_atm_sepsis$WB_incomeshort == "LMIC", ]



# Normalize income share to sum to 1
wiid_data <- wiid_data %>%
  group_by(country_iso3_code) %>%
  mutate(income_share = income_share / sum(income_share))

# Merge relevant datasets
merged_data <- dplyr::left_join(wiid_data, data_2019, by = "country_iso3_code") %>%
  mutate(
    gini = as.numeric(gini) / 100,  # Convert Gini to a proportion
    GDP_pc = as.numeric(GDP_pc)     # Ensure GDP per capita is numeric
  ) %>%
  dplyr::select(country_iso3_code, income_share, group, GDP_pc, gini)

# Merge in PPP data to adjust GDP per capita
merged_data <- dplyr::left_join(merged_data, ppp_2019_iso, by = "country_iso3_code") %>%
  mutate(GDP_pc_adjusted = GDP_pc )
merged_data <- dplyr::left_join(merged_data, poverty_lines, by = "country_iso3_code") %>%
  mutate(GDP_pc_adjusted = GDP_ppp_2017)

merged_data <- dplyr::left_join(merged_data, ER_ppp_19WB, by = "country_iso3_code")
merged_data$GDP_pc_adjusted <- as.numeric(merged_data$GDP_pc_PPP19.x)

# Merge with new_atm_sepsis data (which includes antibiotic course prices)
merged_data <- dplyr::left_join(merged_data, new_atm_sepsis, by = "country_iso3_code")

# Apply Gini adjustments where necessary
merged_data <- merged_data %>%
  mutate(
    gini = case_when(
      country_iso3_code == "BGD" ~ 0.576,
      country_iso3_code == "KOR" ~ 0.32,
      TRUE ~ gini
    )
  )

merged_data$GDP_pc_adjusted[merged_data$country_iso3_code == "VEN"] <- 2624.41
# Define the function to fit the GB2 model
fit_gb2_model <- function(data) {
  fitgroup.gb2(y = data$income_share, gini.e = data$gini[1], pc.inc = data$GDP_pc_adjusted[1])
}

# Define the function to calculate the CDF at a given threshold
calculate_cdf <- function(fit, threshold) {
  if (is.null(fit) || length(fit$omd.estimation) == 0) {
    return(NA)
  }
  tryCatch({
    params <- fit$omd.estimation["Coef.", ]
    shape1 <- params["a"]
    shape2 <- params["b"]
    shape3 <- params["p"]
    rate <- params["q"]
    cdf_value <- pgb2(threshold, shape1, shape2, shape3, rate)
    return(cdf_value)
  }, error = function(e) {
    warning(paste("Error calculating CDF for threshold:", threshold, ":", e$message))
    return(NA)
  })
}

# Fit the GB2 model for each country
resultgb2 <- merged_data %>%
  group_by(country_iso3_code) %>%
  summarize(gb2_fit = list(fit_gb2_model(cur_data())))
# Add antibiotic course prices and merge necessary data
result <- resultgb2 %>%
  left_join(new_atm_sepsis, by = "country_iso3_code") %>%
  left_join(dplyr::select(merged_data, country_iso3_code, GDP_pc_adjusted), by = "country_iso3_code") %>%
  left_join(dplyr::select(poverty_lines, country_iso3_code, nat_povery_line), by = "country_iso3_code")
result$nat_povery_line<- result$nat_povery_line.y
result$WB_incomeshort<- result$WB_incomeshort
result$WHO_region<- result$WHO_region


# Calculate the proportion of the population that could fall under extreme poverty for each treatment
result_with_poverty <- result %>%
  mutate(
    # Calculate the extreme poverty adjusted by treatment costs
    extreme_poverty_treatment1 = (nat_povery_line)*365 + (Clindamycin),
    extreme_poverty_treatment2 = (nat_povery_line)*365 + (Linezolid),
    poverty_line_PL = (nat_povery_line)*365,
    # Convert GDP_pc_adjusted to daily GDP per capita
    #GDP_pc_daily = GDP_ppp_2017 / 365,
    
    # Calculate the proportion of the population that could fall under extreme poverty after each treatment
    proportion_poverty_treatment1 = purrr::map2_dbl(gb2_fit, extreme_poverty_treatment1 , ~ calculate_cdf(..1, ..2) * 100),
    proportion_poverty_treatment2 = purrr::map2_dbl(gb2_fit, extreme_poverty_treatment2 , ~ calculate_cdf(..1, ..2) * 100),
    proportion_poverty_PL = purrr::map2_dbl(gb2_fit, poverty_line_PL , ~ calculate_cdf(..1, ..2) * 100)
    
  ) %>%
  # Select only the necessary columns for output
  dplyr::select(
    country_iso3_code, WB_incomeshort, WHO_region,
    proportion_poverty_treatment1, proportion_poverty_treatment2,
    proportion_poverty_PL
  )
result_with_poverty <- result_with_poverty %>%
  distinct(country_iso3_code, .keep_all = TRUE)
# View the result
View(result_with_poverty)




#GRAPH ........

library(ggsci) 
result_with_poverty <- result_with_poverty %>%
  mutate(WB_incomeshort = case_when(
    country_iso3_code %in% c("THA", "MYS", "SRB") ~ "LMIC",
    TRUE ~ WB_incomeshort
  ))
result_with_poverty <- result_with_poverty[result_with_poverty$WB_incomeshort == "LMIC", ]

# Step 1: Separate out the Poverty Line values for each country and rename the column for clarity
poverty_line_values <- result_with_poverty %>%
  dplyr::select(country_iso3_code, proportion_poverty_PL) %>%
  rename(poverty_line_value = proportion_poverty_PL)

# Step 2: Subtract Poverty Line values from all treatment proportions
adjusted_data <- result_with_poverty %>%
  left_join(poverty_line_values, by = "country_iso3_code") %>%  # Join Poverty Line values with renamed column
  mutate(
    proportion_poverty_treatment1 = proportion_poverty_treatment1 - poverty_line_value,
    proportion_poverty_treatment2 = proportion_poverty_treatment2 - poverty_line_value
  ) %>%
  dplyr::select(-poverty_line_value)  # Remove the Poverty Line column after subtraction

# Step 3: Convert data back to long format
poverty_long <- adjusted_data %>%
  pivot_longer(
    cols = starts_with("proportion_poverty_treatment"),
    names_to = "treatment_type",
    values_to = "proportion"
  )

adjusted_data <- adjusted_data %>%
  filter(
    !is.na(proportion_poverty_treatment1) |
      !is.na(proportion_poverty_treatment2) 
  )

# Step 4: Convert data back to long format
poverty_long <- adjusted_data %>%
  pivot_longer(
    cols = starts_with("proportion_poverty_treatment"),
    names_to = "treatment_type",
    values_to = "proportion"
  )


# Step 3: Find the maximum proportion for each country and reorder by this max value
poverty_long <- poverty_long %>%
  group_by(country_iso3_code) %>%
  mutate(max_proportion = max(proportion, na.rm = TRUE)) %>%  # Calculate max proportion for each country
  ungroup() %>%
  arrange(desc(max_proportion), country_iso3_code) %>%  # Order countries by max proportion
  mutate(country_iso3_code = factor(country_iso3_code, levels = unique(country_iso3_code)))  # Preserve order for plotting

# Step 4: Arrange treatment_type by proportion within each country to ensure proper stacking order
poverty_long <- poverty_long %>%
  group_by(country_iso3_code) %>%
  arrange(desc(proportion), .by_group = TRUE) %>%  # Sort treatment_type by proportion within each country
  ungroup()

# Define custom colors for each treatment type
custom_colors <- c("proportion_poverty_treatment1" = "#1b9e77",  # Dark green
                   "proportion_poverty_treatment2" = "#d95f02")

# Step 5: Plot with customized settings
poverty_line_expos<- ggplot(poverty_long, aes(x = country_iso3_code, y = proportion, fill = treatment_type)) +
  geom_bar(stat = "identity", position = "identity", color = "black", alpha = 0.7) +  # black border for bars
  scale_fill_manual(values = custom_colors, labels = c("Treatment 1: Clindamycin", "Treatment 2: Linezolid")) +
  coord_flip() +
  labs(x = "Country ISO-3 code", y = "Percentage of the population at risk of falling below the national poverty line \ndue to out-of-pocket costs for hypothetical oral treatments") +
  scale_y_continuous(limits = c(0, 32), breaks = seq(0, 30, by = 5)) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = c(0.65, 0.85),  # Position legend inside plot, top-right
    legend.title = element_blank(),   # Remove legend title
    legend.box.background = element_rect(fill = NA, color = "white"),  # White background and contour for legend box
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines for cleaner look
  )

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("poverty_line_exposORAL.tiff", plot = poverty_line_expos, device = "tiff", width =7, height = 9, dpi = 1000, units = "in")

#######



##------------------------------------------------------------#
# OOPE and CHE annual for antibiotic treatment for unknown sepsis
#-------------------------------------------------------------#
#Calculations and summary stats for cost sepsis#####
#weighted_avg_dataMIDAS_prCA 
# Step 1: Filter the dataframe for the specified antimicrobials
antimicrobialsSEPS <- c("Amikacin", "Colistin", "Meropenem", "Gentamicin", "Cefotaxime", "Ceftriaxone", "Piperacillin+Tazobactam", "Benzylpenicillin", "Ampicillin")
new_atm_sepsis <- weighted_avg_dataMIDAS_prCA[weighted_avg_dataMIDAS_prCA$adila_antimicrobials %in% antimicrobialsSEPS, ]
# Step 2: Identify missing PriceperDDD_w for each country and antibiotic combination
#Calculating the average cost of an UKNOWN sepsis treatment, average 70 kg.
new_atm_sepsis  <- new_atm_sepsis[, c("WB_incomeshort", "country_iso3_code", "adila_antimicrobials", "PriceperDDD_w")]
new_atm_sepsis <- new_atm_sepsis %>%
  pivot_wider(names_from = adila_antimicrobials, values_from = PriceperDDD_w)
new_atm_sepsis$Colistin[new_atm_sepsis$country_iso3_code == "CHN"] <- NA

#ATB COURSES SEPSIS BELOW, calculation for an average person weighting 70kg
new_atm_sepsis$tr_1_ceft_amikacin_pday<- new_atm_sepsis$Ceftriaxone*1 + new_atm_sepsis$Amikacin*((15*70)/(1000))
new_atm_sepsis$tr_1_cefotx_amikacin_pday<- new_atm_sepsis$Cefotaxime*1.5 + new_atm_sepsis$Amikacin*((15*70)/(1000))
new_atm_sepsis$tr_1_ceft_amikacin_pcour<- new_atm_sepsis$tr_1_ceft_amikacin_pday*7
new_atm_sepsis$tr_1_cefotx_amikacin_pcour<- new_atm_sepsis$tr_1_cefotx_amikacin_pday*7
new_atm_sepsis$tr_1_ceft_gentamic_pday<- new_atm_sepsis$Ceftriaxone*1 + new_atm_sepsis$Gentamicin*((5*70)/(240))
new_atm_sepsis$tr_1_cefotx_gentamic_pday<- new_atm_sepsis$Cefotaxime*1.5 + new_atm_sepsis$Gentamicin*((5*70)/(240))
new_atm_sepsis$tr_1_ceft_gentamic_pcour<- new_atm_sepsis$tr_1_ceft_gentamic_pday*7
new_atm_sepsis$tr_1_cefotx_gentamic_pcour<- new_atm_sepsis$tr_1_cefotx_gentamic_pday*7
new_atm_sepsis$tr_2_Piptaz_pday<- new_atm_sepsis$`Piperacillin+Tazobactam`*1.29 #+ new_atm_sepsis$Amikacin*((15*70)/(1000))
new_atm_sepsis$tr_2_Piptaz_pcour<- new_atm_sepsis$tr_2_Piptaz_pday*7  
new_atm_sepsis$tr_3_Merop_pday<- new_atm_sepsis$Meropenem*2 
new_atm_sepsis$tr_3_Meropn_pcour<- new_atm_sepsis$tr_3_Merop_pday*7
new_atm_sepsis$tr_4_Colis_pday<- new_atm_sepsis$Colistin*1 + new_atm_sepsis$Colistin*1/7
new_atm_sepsis$tr_4_Colis_pcour<- new_atm_sepsis$tr_4_Colis_pday*7  

new_atm_sepsis <- new_atm_sepsis %>%
  mutate(
    tr_1_ceft_amikacingen_pday = if_else(
      !is.na(Ceftriaxone) & (!is.na(Amikacin) | !is.na(Gentamicin)),  # Check if both Ceftriaxone and either Amikacin or Gentamicin are not NA
      Ceftriaxone * 1 + if_else(
        !is.na(Amikacin), 
        Amikacin * ((15 * 70) / 1000),  # If Amikacin is available
        Gentamicin * ((5 * 70) / 240)   # If Amikacin is NA, use Gentamicin
      ),
      NA_real_  # If the condition is not met, return NA
    )
  )
new_atm_sepsis$tr_1_ceft_amikacingen_pcour<- new_atm_sepsis$tr_1_ceft_amikacingen_pday*7



#ATB courses for Children 3.5 kg.
new_atm_sepsis$tr_1_amp_gen_pday_child<- new_atm_sepsis$Ampicillin*0.088 + new_atm_sepsis$Gentamicin*0.109
new_atm_sepsis$tr_1_benz_gen_pday_child<- new_atm_sepsis$Benzylpenicillin*0.088 + new_atm_sepsis$Gentamicin*0.109
new_atm_sepsis$tr_2_ceftr_pday_child<- new_atm_sepsis$Ceftriaxone*0.140
new_atm_sepsis$tr_2_cefot_pday_child<- new_atm_sepsis$Cefotaxime*0.131
new_atm_sepsis$tr_3_piptaz_pday_child<- new_atm_sepsis$`Piperacillin+Tazobactam`*0.075 #+ new_atm_sepsis$Amikacin*0.053
new_atm_sepsis$tr_4_meropenem_pday_child<- new_atm_sepsis$Meropenem*0.140
new_atm_sepsis$tr_5_colistin_pday_child<- new_atm_sepsis$Colistin*0.044

new_atm_sepsis <- new_atm_sepsis %>%
  mutate(
    tr_1_ampbenz_gen_pday_child = if_else(
      !is.na(Gentamicin) & (!is.na(Benzylpenicillin) | !is.na(Ampicillin)),  
      new_atm_sepsis$Gentamicin*0.109 + if_else(
        !is.na(Ampicillin), 
        new_atm_sepsis$Ampicillin*0.088,  # If Ampicillin is available
        new_atm_sepsis$Benzylpenicillin*0.088   # If Ampicillin is NA, use benzy
      ),
      NA_real_  # If the condition is not met, return NA
    )
  )
new_atm_sepsis$tr_1_ampbenz_gen_pcour_child<- new_atm_sepsis$tr_1_ampbenz_gen_pday_child*7



new_atm_sepsis$tr_1_amp_gen_pcour_child <-  new_atm_sepsis$tr_1_amp_gen_pday_child*7
new_atm_sepsis$tr_1_benz_gen_pcour_child <- new_atm_sepsis$tr_1_benz_gen_pday_child*7
new_atm_sepsis$tr_2_ceftr_pcour_child <- new_atm_sepsis$tr_2_ceftr_pday_child*7
new_atm_sepsis$tr_2_cefot_pcour_child <- new_atm_sepsis$tr_2_cefot_pday_child*7
new_atm_sepsis$tr_3_piptaz_pcour_child <- new_atm_sepsis$tr_3_piptaz_pday_child*7
new_atm_sepsis$tr_4_meropenem_pcour_child<- new_atm_sepsis$tr_4_meropenem_pday_child*7
new_atm_sepsis$tr_5_colistin_pcour_child<- new_atm_sepsis$tr_5_colistin_pday_child*7

#
country_iso3_code <- c("ITA", "BEL", "AUS", "URY", "LUX", "MYS", "POL", "CHL", "KOR", "AUT", 
                       "ESP", "PRI", "JPN", "CZE", "KWT", "LVA", "CHE", "FRA", "FIN", "CAN", 
                       "GBR", "HKG", "SWE", "HUN", "PRT", "IRL", "ARE", "USA", "NOR", "NZL", 
                       "EST", "NLD", "SGP", "DEU", "GRC", "SAU", "THA", "LTU", "HRV", "SRB", 
                       "DZA", "IDN", "PAK", "VNM", "TUR", "BLR", "MAR", "SVK", "TUN", "ARG", 
                       "DOM", "PER", "RUS", "MEX", "ROU", "CHN", "LKA", "TWN", "PHL", "ECU", 
                       "IND", "VEN", "COL", "BGD", "JOR", "ZAF", "LBN", "BGR", "EGY", "SVN", 
                       "BRA", "KAZ", "BIH")

WHO_region <- c("EURO", "EURO", "WPRO", "AMRO", "EURO", "WPRO", "EURO", "AMRO", "WPRO", "EURO", 
                "EURO", "AMRO", "WPRO", "EURO", "EMRO", "EURO", "EURO", "EURO", "EURO", "AMRO", 
                "EURO", "WPRO", "EURO", "EURO", "EURO", "EMRO", "AMRO", "EURO", "WPRO", "EURO", 
                "EURO", "WPRO", "EURO", "EURO", "EURO", "EMRO", "SEARO", "EURO", "EURO", "EURO", 
                "AFRO", "SEARO", "EMRO", "WPRO", "EURO", "EURO", "AFRO", "EURO", "AFRO", "AMRO", 
                "AMRO", "AMRO", "EURO", "AMRO", "EURO", "WPRO", "SEARO", "WPRO", "WPRO", "AMRO", 
                "SEARO", "AMRO", "AMRO", "SEARO", "EMRO", "AFRO", "EMRO", "EURO", "AFRO", "EURO", 
                "AMRO", "EURO", "EURO")

WB_incomefull <- c(
  "HIC",  # ITA - High Income Country
  "HIC",  # BEL - High Income Country
  "HIC",  # AUS - High Income Country
  "UMIC", # URY - Upper Middle Income Country
  "HIC",  # LUX - High Income Country
  "UMIC", # MYS - Upper Middle Income Country
  "HIC",  # POL - High Income Country
  "HIC",  # CHL - High Income Country
  "HIC",  # KOR - High Income Country
  "HIC",  # AUT - High Income Country
  "HIC",  # ESP - High Income Country
  "UMIC", # PRI - Upper Middle Income Country
  "HIC",  # JPN - High Income Country
  "HIC",  # CZE - High Income Country
  "HIC",  # KWT - High Income Country
  "HIC",  # LVA - High Income Country
  "HIC",  # CHE - High Income Country
  "HIC",  # FRA - High Income Country
  "HIC",  # FIN - High Income Country
  "HIC",  # CAN - High Income Country
  "HIC",  # GBR - High Income Country
  "HIC",  # HKG - High Income Country
  "HIC",  # SWE - High Income Country
  "HIC",  # HUN - High Income Country
  "HIC",  # PRT - High Income Country
  "HIC",  # IRL - High Income Country
  "HIC",  # ARE - High Income Country
  "HIC",  # USA - High Income Country
  "HIC",  # NOR - High Income Country
  "HIC",  # NZL - High Income Country
  "HIC",  # EST - High Income Country
  "HIC",  # NLD - High Income Country
  "HIC",  # SGP - High Income Country
  "HIC",  # DEU - High Income Country
  "HIC",  # GRC - High Income Country
  "HIC",  # SAU - High Income Country
  "UMIC", # THA - Upper Middle Income Country
  "HIC",  # LTU - High Income Country
  "HIC",  # HRV - High Income Country
  "UMIC", # SRB - Upper Middle Income Country
  "LMIC", # DZA - Lower Middle Income Country
  "LMIC", # IDN - Lower Middle Income Country
  "LMIC", # PAK - Lower Middle Income Country
  "LMIC", # VNM - Lower Middle Income Country
  "UMIC", # TUR - Upper Middle Income Country
  "UMIC", # BLR - Upper Middle Income Country
  "LMIC", # MAR - Lower Middle Income Country
  "HIC",  # SVK - High Income Country
  "UMIC", # TUN - Upper Middle Income Country
  "UMIC", # ARG - Upper Middle Income Country
  "UMIC", # DOM - Upper Middle Income Country
  "UMIC", # PER - Upper Middle Income Country
  "UMIC", # RUS - Upper Middle Income Country
  "UMIC", # MEX - Upper Middle Income Country
  "UMIC", # ROU - Upper Middle Income Country
  "UMIC", # CHN - Upper Middle Income Country
  "UMIC", # LKA - Upper Middle Income Country
  "HIC",  # TWN - High Income Country
  "UMIC", # PHL - Upper Middle Income Country
  "UMIC", # ECU - Upper Middle Income Country
  "LMIC", # IND - Lower Middle Income Country
  "UMIC", # VEN - Upper Middle Income Country
  "UMIC", # COL - Upper Middle Income Country
  "LMIC", # BGD - Lower Middle Income Country
  "UMIC", # JOR - Upper Middle Income Country
  "UMIC", # ZAF - Upper Middle Income Country
  "UMIC", # LBN - Upper Middle Income Country
  "UMIC", # BGR - Upper Middle Income Country
  "LMIC", # EGY - Lower Middle Income Country
  "HIC",  # SVN - High Income Country
  "UMIC", # BRA - Upper Middle Income Country
  "UMIC", # KAZ - Upper Middle Income Country
  "UMIC"  # BIH - Upper Middle Income Country
)

# create a dtaframe with WHO region to then merge it to existing dataset.
df_tomerge_whoreg <- data.frame(country_iso3_code = country_iso3_code, WHO_region = WHO_region, WB_incomefull= WB_incomefull)

new_atm_sepsis <- merge(new_atm_sepsis, df_tomerge_whoreg, by = "country_iso3_code", all.x = TRUE)
new_atm_sepsis$extreme_poverty <- 2.5
new_atm_sepsis$povertyline <- ifelse(new_atm_sepsis$WB_incomefull == "LMIC", 3.65, 
                                     ifelse(new_atm_sepsis$WB_incomefull == "UMIC", 6.85, NA))

#ADULTS pday:       tr_1_ceft_amikacingen_pday   tr_2_Piptaz_pday tr_3_Merop_pday tr_4_Colis_pday
#ADULTS pcourse:    tr_1_ceft_amikacingen_pcour   tr_2_Piptaz_pcour tr_3_Meropn_pcour tr_4_Colis_pcour
#CHILDREN pday:     tr_1_ampbenz_gen_pday_child  tr_2_ceftr_pday_child   tr_3_piptaz_pday_child  tr_4_meropenem_pday_child tr_5_colistin_pday_child
#CHILDREN pcourse:  tr_1_ampbenz_gen_pcour_child  tr_2_ceftr_pcour_child  tr_3_piptaz_pcour_child  tr_4_meropenem_pcour_child tr_5_colistin_pcour_child
# List of course columns to summarize
course_columns <- c("tr_1_ceft_amikacingen_pcour","tr_2_Piptaz_pcour", "tr_3_Meropn_pcour", "tr_4_Colis_pcour")

course_columns2<- c("tr_1_ampbenz_gen_pcour_child", "tr_2_ceftr_pcour_child", "tr_3_piptaz_pcour_child",  "tr_4_meropenem_pcour_child", "tr_5_colistin_pcour_child")

# Create a function to calculate summary statistics
calculate_summary <- function(df, group_var, course_columns) {
  df %>%
    group_by(!!sym(group_var)) %>%
    summarise(across(all_of(course_columns), 
                     list(
                       Median = ~median(., na.rm = TRUE),
                       p25th = ~quantile(., 0.25, na.rm = TRUE),
                       p75th = ~quantile(., 0.75, na.rm = TRUE),
                       IQR = ~IQR(., na.rm = TRUE)
                     ), 
                     .names = "{col}_{fn}"))
}

# Summary by WB_incomeshort
summary_by_income_adult <- calculate_summary(new_atm_sepsis, "WB_incomeshort", course_columns)
summary_by_region_adult <- calculate_summary(new_atm_sepsis, "WHO_region", course_columns)
summary_by_income_child <- calculate_summary(new_atm_sepsis, "WB_incomeshort", course_columns2)
summary_by_region_child <- calculate_summary(new_atm_sepsis, "WHO_region", course_columns2)
#####

###MODEL CDF Income for 7-day ATB courses for unknown sepsis adults/neonates per year#####
# Load necessary packages
library(readxl)
library(dplyr)
library(purrr)
library(fitdistrplus)
library(GB2)
library(GB2group)
#https://cran.r-project.org/web/packages/GB2group/GB2group.pdf

# Load the WIID data from the specific sheet
# Define the antibiotic course columns (which are prices)
country_iso3_code <- c("ARE", "ARG", "AUS", "AUT", "BEL", "BGD", "BGR", "BIH", "BLR", "BRA", 
                       "CAN", "CHE", "CHL", "CHN", "COL", "CZE", "DEU", "DOM", "DZA", "ECU", 
                       "EGY", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HKG", "HRV", "HUN", 
                       "IDN", "IND", "IRL", "ITA", "JOR", "JPN", "KAZ", "KOR", "KWT", "LBN", 
                       "LKA", "LTU", "LUX", "LVA", "MAR", "MEX", "MYS", "NLD", "NOR", "NZL", 
                       "PAK", "PER", "PHL", "POL", "PRI", "PRT", "ROU", "RUS", "SAU", "SGP", 
                       "SRB", "SVK", "SVN", "SWE", "THA", "TUN", "TUR", "TWN", "URY", "USA", 
                       "VEN", "VNM", "ZAF")

# Create a vector for GDP PPP 2019 values
gdp_ppp_2019 <- c(74035, 20482, 51885, 55896, 54098, 5139, 24576, 15964, 19997, 16784, 
                  50001, 73703, 26318, 16804, 16264, 40793, 54075, 18535, 13800, 11582, 
                  12668, 41650, 38906, 49032, 48173, 47905, 30252, 62839, 29460, 35942, 
                  13036, 7034, 85267, 43795, 10794, 44227, 28849, 44652, 71263, 11264, 
                  12564, 39610, 117558, 35701, 9351, 19421, 30049, 58846, 63863, 44943, 
                  5705, 13933, 10080, 34057, 39915, 35000, 31173, 28181, 47817, 97057, 
                  18564, 37789, 40742, 53652, 19499, 11973, 28041, 47800, 23790, 65297, 
                  5443, 8131, 13446)

# Create a dataframe
gdp_ppp_df <- data.frame(country_iso3_code, gdp_ppp_2019)


course_columns <- c("tr_1_ceft_amikacingen_pcour","tr_2_Piptaz_pcour", "tr_3_Meropn_pcour", "tr_4_Colis_pcour")
wiid_data <- readxl::read_excel("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_Projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/WIID_28NOV2023.xlsx", sheet = "data_wiib")
poverty_lines<- read.csv(file= "/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/OneDrive_24/data_povertyline.csv")

#DATA MANAGEMENT AND TEST DATA:
# Normalize income share to sum to 1
wiid_data <- wiid_data %>%
  group_by(country_iso3_code) %>%
  mutate(income_share = income_share / sum(income_share))

merged_data <- dplyr::left_join(wiid_data, data_2019, by = "country_iso3_code")
merged_data <-   dplyr::left_join(wiid_data, data_2019, by = "country_iso3_code") 
merged_data$gini <- as.numeric(merged_data$gini)
merged_data$gini <- merged_data$gini/100
merged_data <- dplyr::left_join(merged_data, ER_ppp_19WB, by = "country_iso3_code")
merged_data$GDP_pc <- as.numeric(merged_data$GDP_pc_PPP19) 
#PERHAPS WE SHOULD UPDATE THE BELOW TO DAILY GDP_pc, rather than an annual figure?
merged_data$GDP_pc <- merged_data$GDP_pc

merged_data <- merged_data %>%
  dplyr::select(country_iso3_code, income_share, group, GDP_pc, gini)

# Merge in PPP data to adjust GDP per capita
merged_data <- dplyr::left_join(merged_data, ppp_2019_iso, by = "country_iso3_code")
# Adjust the GDP per capita by the PPP rate to align it with the 2019 USD PPP antibiotic prices
merged_data <- merged_data %>%
  mutate(GDP_pc_adjusted = GDP_pc)
# Merge `wiid_data` and `new_atm_sepsis` based on country code
merged_data <- dplyr::left_join(merged_data, new_atm_sepsis, by = "country_iso3_code")
# Select relevant columns including the PPP-adjusted GDP per capita
merged_data <- merged_data %>%
  dplyr::select(country_iso3_code, income_share, GDP_pc_adjusted, gini, all_of(course_columns))


merged_data <- merged_data %>%
  mutate(
    gini = case_when(
      country_iso3_code == "BGD" ~ 0.576,
      country_iso3_code == "KOR" ~ 0.32,
      TRUE ~ gini  # Keep the original gini value for other countries
    )
  )

# Assuming merged_data already exists and contains a column country_iso3_code
merged_data$GDP_pc_adjusted[merged_data$country_iso3_code == "VEN"] <- 2624.41

# Define a function to fit the GB2 model
fit_gb2_model <- function(data) {
  fitgroup.gb2(y = data$income_share, gini.e = data$gini[1], pc.inc = data$GDP_pc_adjusted[1])
}
# Define a function to calculate the CDF at a given threshold
calculate_cdf <- function(fit, threshold) {
  if (is.null(fit) || length(fit$omd.estimation) == 0) {
    return(NA)
  }
  tryCatch({
    params <- fit$omd.estimation["Coef.", ]
    shape1 <- params["a"]
    shape2 <- params["b"]
    shape3 <- params["p"]
    rate <- params["q"]
    cdf_value <- pgb2(threshold, shape1, shape2, shape3, rate)
    return(cdf_value)
  }, error = function(e) {
    warning(paste("Error calculating CDF for threshold:", threshold, ":", e$message))
    return(NA)
  })
}

# Fit the GB2 model and calculate the CDF for each country
result <- merged_data %>%
  group_by(country_iso3_code) %>%
  summarize(gb2_fit = list(fit_gb2_model(cur_data())))

# Add antibiotic course prices to the result
new_atm_sepsis <- new_atm_sepsis %>%
  left_join(ppp_2019_iso, by = "country_iso3_code")
result <- result %>%
  left_join(new_atm_sepsis, by = "country_iso3_code")
unique_gdp_data <- merged_data %>%
  dplyr::select(country_iso3_code, GDP_pc_adjusted) %>%
  distinct(country_iso3_code, .keep_all = TRUE)

result <- result %>%
  left_join(unique_gdp_data, by = "country_iso3_code")

# Calculate the percentage of the population below each threshold (20%, 30%, and 40% of annual income)
resultAmikgen_adult1 <- result %>%
  mutate(
    # Calculate treatment cost as a percentage of GDP per capita
    treatment_cost_as_percent_income = (tr_1_ceft_amikacingen_pcour / GDP_pc_adjusted) * 100,
    
    # Create thresholds for 10%, 20%, 30%, and 40% of income spent on treatment
    threshold_10pct = treatment_cost_as_percent_income * 10,  # Represents spending 10% of income
    threshold_20pct = treatment_cost_as_percent_income * 5,   # Represents spending 20% of income
    threshold_30pct = treatment_cost_as_percent_income * (10 / 3),  # Represents spending 30% of income
    threshold_40pct = treatment_cost_as_percent_income * 2.5,  # Represents spending 40% of income
    
    # Calculate the percentage of the population whose spending exceeds 10%, 20%, 30%, and 40% of their income
    percentage_below_10pct = purrr::map2_dbl(gb2_fit, threshold_10pct, ~ calculate_cdf(..1, ..2) * 100),
    percentage_below_20pct = purrr::map2_dbl(gb2_fit, threshold_20pct, ~ calculate_cdf(..1, ..2) * 100),
    percentage_below_30pct = purrr::map2_dbl(gb2_fit, threshold_30pct, ~ calculate_cdf(..1, ..2) * 100),
    percentage_below_40pct = purrr::map2_dbl(gb2_fit, threshold_40pct, ~ calculate_cdf(..1, ..2) * 100)
  )%>%
  # Select only the necessary columns
  dplyr::select(country_iso3_code, WB_incomeshort.x, WHO_region.x, 
                percentage_below_10pct, percentage_below_20pct, 
                percentage_below_30pct, percentage_below_40pct)
# Display the result
print(result)


# Define the list of course variables
course_variables <- c("tr_1_ceft_amikacingen_pcour", "tr_2_Piptaz_pcour", "tr_3_Meropn_pcour", "tr_4_Colis_pcour")

# Loop over each variable and create a new dataframe
for (course in course_variables) {
  
  # Create a dynamic name for the result dataframe
  result_name <- paste0("result_", course)
  
  # Create the new dataframe with the respective course variable
  assign(result_name, 
         result %>%
           mutate(
             # Calculate treatment cost as a percentage of GDP per capita
             treatment_cost_as_percent_income = (!!sym(course) / GDP_pc_adjusted) * 100,
             
             # Create thresholds for 10%, 20%, 30%, and 40% of income spent on treatment
             threshold_10pct = treatment_cost_as_percent_income * 10,  # Represents spending 10% of income
             threshold_20pct = treatment_cost_as_percent_income * 5,   # Represents spending 20% of income
             threshold_30pct = treatment_cost_as_percent_income * (10 / 3),  # Represents spending 30% of income
             threshold_40pct = treatment_cost_as_percent_income * 2.5,  # Represents spending 40% of income
             
             # Calculate the percentage of the population whose spending exceeds 10%, 20%, 30%, and 40% of their income
             percentage_below_10pct = purrr::map2_dbl(gb2_fit, threshold_10pct, ~ calculate_cdf(..1, ..2) * 100),
             percentage_below_20pct = purrr::map2_dbl(gb2_fit, threshold_20pct, ~ calculate_cdf(..1, ..2) * 100),
             percentage_below_30pct = purrr::map2_dbl(gb2_fit, threshold_30pct, ~ calculate_cdf(..1, ..2) * 100),
             percentage_below_40pct = purrr::map2_dbl(gb2_fit, threshold_40pct, ~ calculate_cdf(..1, ..2) * 100)
           ) %>%
           # Select only the necessary columns
           dplyr::select(country_iso3_code, WB_incomeshort.x, WHO_region.x, 
                         percentage_below_10pct, percentage_below_20pct, 
                         percentage_below_30pct, percentage_below_40pct)
  )
}

course_variables2<- c("tr_1_ampbenz_gen_pcour_child", "tr_2_ceftr_pcour_child", "tr_3_piptaz_pcour_child",  "tr_4_meropenem_pcour_child", "tr_5_colistin_pcour_child")

# Loop over each variable and create a new dataframe
for (course in course_variables2) {
  
  # Create a dynamic name for the result dataframe
  result_name <- paste0("result_", course)
  
  # Create the new dataframe with the respective course variable
  assign(result_name, 
         result %>%
           mutate(
             # Calculate treatment cost as a percentage of GDP per capita
             treatment_cost_as_percent_income = (!!sym(course) / GDP_pc_adjusted) * 100,
             
             # Create thresholds for 10%, 20%, 30%, and 40% of income spent on treatment
             threshold_10pct = treatment_cost_as_percent_income * 10,  # Represents spending 10% of income
             threshold_20pct = treatment_cost_as_percent_income * 5,   # Represents spending 20% of income
             threshold_30pct = treatment_cost_as_percent_income * (10 / 3),  # Represents spending 30% of income
             threshold_40pct = treatment_cost_as_percent_income * 2.5,  # Represents spending 40% of income
             
             # Calculate the percentage of the population whose spending exceeds 10%, 20%, 30%, and 40% of their income
             percentage_below_10pct = purrr::map2_dbl(gb2_fit, threshold_10pct, ~ calculate_cdf(..1, ..2) * 100),
             percentage_below_20pct = purrr::map2_dbl(gb2_fit, threshold_20pct, ~ calculate_cdf(..1, ..2) * 100),
             percentage_below_30pct = purrr::map2_dbl(gb2_fit, threshold_30pct, ~ calculate_cdf(..1, ..2) * 100),
             percentage_below_40pct = purrr::map2_dbl(gb2_fit, threshold_40pct, ~ calculate_cdf(..1, ..2) * 100)
           ) %>%
           # Select only the necessary columns
           dplyr::select(country_iso3_code, WB_incomeshort.x, WHO_region.x, 
                         percentage_below_10pct, percentage_below_20pct, 
                         percentage_below_30pct, percentage_below_40pct)
  )
}

#Results:
#result_tr_1_ceft_amikacingen_pcour result_tr_2_Piptaz_pcour result_tr_3_Meropn_pcour result_tr_4_Colis_pcour
#result_tr_1_ampbenz_gen_pcour_child result_tr_2_ceftr_pcour_child result_tr_3_piptaz_pcour_child result_tr_4_meropenem_pcour_child result_tr_5_colistin_pcour_child")

#GRAPHS:

# Function to prepare and label the data for each treatment
prepare_data_for_plot <- function(data, treatment_label) {
  # Filter for countries with at least 1% in percentage_below_10pct
  filtered_data <- data %>%
    filter(percentage_below_10pct >= 0.00001) %>%
    # Gather the percentage columns for easier plotting
    gather(key = "threshold", value = "percentage", 
           percentage_below_10pct, 
           percentage_below_20pct, 
           percentage_below_30pct, 
           percentage_below_40pct) %>%
    mutate(threshold = factor(threshold, 
                              levels = c("percentage_below_40pct", 
                                         "percentage_below_30pct", 
                                         "percentage_below_20pct", 
                                         "percentage_below_10pct"),
                              labels = c("40%, OOPE", "30%, CHE", "20%, CHE", "10%")),
           treatment = treatment_label) # Add treatment label for grouping
  return(filtered_data)
}

# Prepare data for each treatment
data1 <- prepare_data_for_plot(result_tr_1_ceft_amikacingen_pcour, "Treatment group 1: Ceftriaxone + Amikacin/Gentamicin")
data2 <- prepare_data_for_plot(result_tr_2_Piptaz_pcour, "Treatment group 2: Piperacillin + Tazobactam")
data3 <- prepare_data_for_plot(result_tr_3_Meropn_pcour, "Treatment group 3: Meropenem")
data4 <- prepare_data_for_plot(result_tr_4_Colis_pcour, "Treatment group 4: Colistin")

# Combine all datasets
combined_data <- bind_rows(data1, data2, data3, data4)

# Add a blank line between treatments by adding an empty row
add_blank_lines <- function(data, treatment_label) {
  empty_row <- tibble(
    country_iso3_code = "",  # Empty country to create space
    threshold = "10%",  # Pick one of the threshold values
    percentage = 0,  # Set to 0 for no actual bar
    treatment = treatment_label  # Ensure it's aligned with the proper section
  )
  return(bind_rows(data, empty_row))
}

# Loop over each treatment and manually add blank lines between groups
combined_data_with_blanks <- bind_rows(
  add_blank_lines(data1, "Treatment group 1: Ceftriaxone + Amikacin/Gentamicin"),
  add_blank_lines(data2, "Treatment group 2: Piperacillin + Tazobactam"),
  add_blank_lines(data3, "Treatment group 3: Meropenem"),
  add_blank_lines(data4, "Treatment group 4: Colistin")
)

# Create the stacked bar plot with inverted axis and subtitles for treatments
combined_data_with_blanks$percentage <- ifelse(combined_data_with_blanks$percentage > 100, 100, combined_data_with_blanks$percentage)
library(dplyr)

# Function to cap the values and recalculate the lower thresholds
recalculate_thresholds <- function(data) {
  data <- data %>%
    group_by(country_iso3_code, treatment) %>%
    arrange(desc(threshold)) %>%
    mutate(
      cumulative_sum = cumsum(percentage),
      # Adjust values based on the cumulative sum, handling 40% and 30% thresholds
      percentage_adjusted = case_when(
        cumulative_sum > 100 & threshold == "30%, CHE" ~ 100 - sum(percentage[threshold == "40%, OOPE"]),  # Adjust 30% to fit within 100% after 40%
        cumulative_sum > 100 & threshold == "20%, CHE" ~ 100 - sum(percentage[threshold %in% c("40%, OOPE", "30%, CHE")]),  # Adjust 20% to fit within 100% after 40% and 30%
        cumulative_sum > 100 & threshold == "10%" ~ 100 - sum(percentage[threshold %in% c("40%, OOPE", "30%, CHE", "20%, CHE")]),  # Adjust 10% to fit within 100% after higher thresholds
        TRUE ~ percentage  # Keep original percentage if no adjustment is needed
      ),
      # Ensure values are capped at 0 if recalculated percentage goes negative
      percentage_adjusted = pmax(0, percentage_adjusted)
    ) %>%
    ungroup()
  
  return(data)
}

# Apply the function to your dataset
combined_data_with_blanks_adjusted <- recalculate_thresholds(combined_data_with_blanks)

# Create the stacked bar plot with recalculated values
adult_treatment1 <- ggplot(combined_data_with_blanks_adjusted, aes(y = reorder(country_iso3_code, percentage_adjusted), x = percentage_adjusted, fill = threshold)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +  # Add black contour
  facet_wrap(~ treatment, scales = "free_y", ncol = 1, strip.position = "top") +
  scale_fill_manual(values = c( "#F0E442", "#E69F00","#D55E00","#0072B2"),   
                    name = "Income Threshold") +
  labs(x = "Percentage of the population (%)", y = "Country ISO-3 code", title = "") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1, vjust = 1, size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.text.x = element_text(hjust = 0, face = "bold", size = 11),
    strip.placement = "outside"
  ) +
  #coord_cartesian(xlim = c(0, 100)) +  # Ensure the x-axis stays between 0 and 100% 
  guides(fill = guide_legend(title = "Spending threshold", position = "inside")) +
  theme(legend.position = c(0.9, 0.82))  # Position the legend inside

# Save the plot as a TIFF file
ggsave("adult_treatment1_cheOOPE.tiff", plot = adult_treatment1, device = "tiff", width = 7, height = 10, dpi = 500, units = "in")



#GRAPH 2 for children/neonates:
#result_tr_1_ampbenz_gen_pcour_child result_tr_2_ceftr_pcour_child result_tr_3_piptaz_pcour_child result_tr_4_meropenem_pcour_child result_tr_5_colistin_pcour_child")

# Prepare data for each treatment
data1 <- prepare_data_for_plot(result_tr_1_ampbenz_gen_pcour_child, "Treatment group 1: Ampicillin/Benzylpenicillin + Gentamicin")
data2 <- prepare_data_for_plot(result_tr_2_ceftr_pcour_child, "Treatment group 2: Ceftriaxone")
data3 <- prepare_data_for_plot(result_tr_3_piptaz_pcour_child, "Treatment group 3: Piperacillin + Tazobactam")
data4 <- prepare_data_for_plot(result_tr_4_meropenem_pcour_child, "Treatment group 4: Meropenem")
data5 <- prepare_data_for_plot(result_tr_5_colistin_pcour_child, "Treatment group 5: Colistin")

# Combine all datasets
combined_data <- bind_rows(data1, data2, data3, data4, data5)

# Add a blank line between treatments by adding an empty row
add_blank_lines <- function(data, treatment_label) {
  empty_row <- tibble(
    country_iso3_code = "",  # Empty country to create space
    threshold = "10%",  # Pick one of the threshold values
    percentage = 0,  # Set to 0 for no actual bar
    treatment = treatment_label  # Ensure it's aligned with the proper section
  )
  return(bind_rows(data, empty_row))
}

# Loop over each treatment and manually add blank lines between groups
combined_data_with_blanks <- bind_rows(
  add_blank_lines(data1, "Treatment group 1: Ampicillin/Benzylpenicillin + Gentamicin"),
  add_blank_lines(data2, "Treatment group 2: Ceftriaxone"),
  add_blank_lines(data3, "Treatment group 3: Piperacillin + Tazobactam"),
  add_blank_lines(data4, "Treatment group 4: Meropenem"),
  add_blank_lines(data5, "Treatment group 5: Colistin")
)
combined_data_with_blanks$percentage <- ifelse(combined_data_with_blanks$percentage > 100, 100, combined_data_with_blanks$percentage)

# Create the stacked bar plot with inverted axis and subtitles for treatments
combined_data_with_blanks_adjusted <- recalculate_thresholds(combined_data_with_blanks)
children_treatment1 <- ggplot(combined_data_with_blanks_adjusted, aes(y = reorder(country_iso3_code, percentage_adjusted), x = percentage_adjusted, fill = threshold)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +  # Add black contour
  facet_wrap(~ treatment, scales = "free_y", ncol = 1, strip.position = "top") +
  scale_fill_manual(values = c( "#F0E442", "#E69F00","#D55E00","#0072B2"),   
                    name = "Income Threshold") +
  labs(x = "Percentage of the population (%)", y = "Country ISO-3 code", title = "") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1, vjust = 1, size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.text.x = element_text(hjust = 0, face = "bold", size = 11),
    strip.placement = "outside"
  ) +
  coord_cartesian(xlim = c(0, 100)) +  # Ensure the x-axis stays between 0 and 100% 
  guides(fill = guide_legend(title = "Spending threshold", position = "inside")) +
  theme(legend.position = c(0.9, 0.82))  # Position the legend inside

#adult_treatment1
ggsave("Children_treatment1_cheOOPE.tiff", plot = children_treatment1, device = "tiff", width =7, height = 11, dpi = 500, units = "in")














#####


####POVERTY LINES AND EXTREME POVErty: IMPOVERISHMENT #########
library(purrr)
poverty_lines<- read.csv(file= "/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/OneDrive_24/data_povertyline.csv")
new_atm_sepsis <- merge(new_atm_sepsis, poverty_lines, by = "country_iso3_code", all.x = TRUE)
#CHECK THIS HERE BELOW:
#new_atm_sepsis <- new_atm_sepsis[new_atm_sepsis$WB_incomeshort == "LMIC", ]



# Normalize income share to sum to 1
wiid_data <- wiid_data %>%
  group_by(country_iso3_code) %>%
  mutate(income_share = income_share / sum(income_share))

# Merge relevant datasets
merged_data <- dplyr::left_join(wiid_data, data_2019, by = "country_iso3_code") %>%
  mutate(
    gini = as.numeric(gini) / 100,  # Convert Gini to a proportion
    GDP_pc = as.numeric(GDP_pc)     # Ensure GDP per capita is numeric
  ) %>%
  dplyr::select(country_iso3_code, income_share, group, GDP_pc, gini)

# Merge in PPP data to adjust GDP per capita
merged_data <- dplyr::left_join(merged_data, ppp_2019_iso, by = "country_iso3_code") %>%
  mutate(GDP_pc_adjusted = GDP_pc )
merged_data <- dplyr::left_join(merged_data, poverty_lines, by = "country_iso3_code") %>%
  mutate(GDP_pc_adjusted = GDP_ppp_2017)

merged_data <- dplyr::left_join(merged_data, ER_ppp_19WB, by = "country_iso3_code")
merged_data$GDP_pc_adjusted <- as.numeric(merged_data$GDP_pc_PPP19.x)

# Merge with new_atm_sepsis data (which includes antibiotic course prices)
merged_data <- dplyr::left_join(merged_data, new_atm_sepsis, by = "country_iso3_code")

# Apply Gini adjustments where necessary
merged_data <- merged_data %>%
  mutate(
    gini = case_when(
      country_iso3_code == "BGD" ~ 0.576,
      country_iso3_code == "KOR" ~ 0.32,
      TRUE ~ gini
    )
  )

merged_data$GDP_pc_adjusted[merged_data$country_iso3_code == "VEN"] <- 2624.41
# Define the function to fit the GB2 model
fit_gb2_model <- function(data) {
  fitgroup.gb2(y = data$income_share, gini.e = data$gini[1], pc.inc = data$GDP_pc_adjusted[1])
}

# Define the function to calculate the CDF at a given threshold
calculate_cdf <- function(fit, threshold) {
  if (is.null(fit) || length(fit$omd.estimation) == 0) {
    return(NA)
  }
  tryCatch({
    params <- fit$omd.estimation["Coef.", ]
    shape1 <- params["a"]
    shape2 <- params["b"]
    shape3 <- params["p"]
    rate <- params["q"]
    cdf_value <- pgb2(threshold, shape1, shape2, shape3, rate)
    return(cdf_value)
  }, error = function(e) {
    warning(paste("Error calculating CDF for threshold:", threshold, ":", e$message))
    return(NA)
  })
}

# Fit the GB2 model for each country
resultgb2 <- merged_data %>%
  group_by(country_iso3_code) %>%
  summarize(gb2_fit = list(fit_gb2_model(cur_data())))
# Add antibiotic course prices and merge necessary data
result <- resultgb2 %>%
  left_join(new_atm_sepsis, by = "country_iso3_code") %>%
  left_join(dplyr::select(merged_data, country_iso3_code, GDP_pc_adjusted), by = "country_iso3_code") %>%
  left_join(dplyr::select(poverty_lines, country_iso3_code, nat_povery_line), by = "country_iso3_code")
result$nat_povery_line<- result$nat_povery_line.y
result$WB_incomeshort<- result$WB_incomeshort.y
result$WHO_region<- result$WHO_region.y


# Calculate the proportion of the population that could fall under extreme poverty for each treatment
result_with_poverty <- result %>%
  mutate(
    # Calculate the extreme poverty adjusted by treatment costs
    extreme_poverty_treatment1 = (nat_povery_line)*365 + (tr_1_ceft_amikacingen_pcour),
    extreme_poverty_treatment2 = (nat_povery_line)*365 + (tr_2_Piptaz_pcour),
    extreme_poverty_treatment3 = (nat_povery_line)*365 + (tr_3_Meropn_pcour),
    extreme_poverty_treatment4 = (nat_povery_line)*365 + (tr_4_Colis_pcour),
    poverty_line_PL = (nat_povery_line)*365,
    # Convert GDP_pc_adjusted to daily GDP per capita
    #GDP_pc_daily = GDP_ppp_2017 / 365,
    
    # Calculate the proportion of the population that could fall under extreme poverty after each treatment
    proportion_poverty_treatment1 = purrr::map2_dbl(gb2_fit, extreme_poverty_treatment1 , ~ calculate_cdf(..1, ..2) * 100),
    proportion_poverty_treatment2 = purrr::map2_dbl(gb2_fit, extreme_poverty_treatment2 , ~ calculate_cdf(..1, ..2) * 100),
    proportion_poverty_treatment3 = purrr::map2_dbl(gb2_fit, extreme_poverty_treatment3 , ~ calculate_cdf(..1, ..2) * 100),
    proportion_poverty_treatment4 = purrr::map2_dbl(gb2_fit, extreme_poverty_treatment4 , ~ calculate_cdf(..1, ..2) * 100),
    proportion_poverty_PL = purrr::map2_dbl(gb2_fit, poverty_line_PL , ~ calculate_cdf(..1, ..2) * 100)
    
  ) %>%
  # Select only the necessary columns for output
  dplyr::select(
    country_iso3_code, WB_incomeshort, WHO_region,
    proportion_poverty_treatment1, proportion_poverty_treatment2,
    proportion_poverty_treatment3, proportion_poverty_treatment4, proportion_poverty_PL
  )
result_with_poverty <- result_with_poverty %>%
  distinct(country_iso3_code, .keep_all = TRUE)
# View the result
View(result_with_poverty)




#GRAPH ........

library(ggsci) 
result_with_poverty <- result_with_poverty %>%
  mutate(WB_incomeshort = case_when(
    country_iso3_code %in% c("THA", "MYS", "SRB") ~ "LMIC",
    TRUE ~ WB_incomeshort
  ))
result_with_poverty <- result_with_poverty[result_with_poverty$WB_incomeshort == "LMIC", ]

# Step 1: Separate out the Poverty Line values for each country and rename the column for clarity
poverty_line_values <- result_with_poverty %>%
  dplyr::select(country_iso3_code, proportion_poverty_PL) %>%
  rename(poverty_line_value = proportion_poverty_PL)

# Step 2: Subtract Poverty Line values from all treatment proportions
adjusted_data <- result_with_poverty %>%
  left_join(poverty_line_values, by = "country_iso3_code") %>%  # Join Poverty Line values with renamed column
  mutate(
    proportion_poverty_treatment1 = proportion_poverty_treatment1 - poverty_line_value,
    proportion_poverty_treatment2 = proportion_poverty_treatment2 - poverty_line_value,
    proportion_poverty_treatment3 = proportion_poverty_treatment3 - poverty_line_value,
    proportion_poverty_treatment4 = proportion_poverty_treatment4 - poverty_line_value
  ) %>%
  dplyr::select(-poverty_line_value)  # Remove the Poverty Line column after subtraction

# Step 3: Convert data back to long format
poverty_long <- adjusted_data %>%
  pivot_longer(
    cols = starts_with("proportion_poverty_treatment"),
    names_to = "treatment_type",
    values_to = "proportion"
  )

adjusted_data <- adjusted_data %>%
  filter(
    !is.na(proportion_poverty_treatment1) |
      !is.na(proportion_poverty_treatment2) |
      !is.na(proportion_poverty_treatment3) |
      !is.na(proportion_poverty_treatment4)
  )

# Step 4: Convert data back to long format
poverty_long <- adjusted_data %>%
  pivot_longer(
    cols = starts_with("proportion_poverty_treatment"),
    names_to = "treatment_type",
    values_to = "proportion"
  )


# Step 3: Find the maximum proportion for each country and reorder by this max value
poverty_long <- poverty_long %>%
  group_by(country_iso3_code) %>%
  mutate(max_proportion = max(proportion, na.rm = TRUE)) %>%  # Calculate max proportion for each country
  ungroup() %>%
  arrange(desc(max_proportion), country_iso3_code) %>%  # Order countries by max proportion
  mutate(country_iso3_code = factor(country_iso3_code, levels = unique(country_iso3_code)))  # Preserve order for plotting

# Step 4: Arrange treatment_type by proportion within each country to ensure proper stacking order
poverty_long <- poverty_long %>%
  group_by(country_iso3_code) %>%
  arrange(desc(proportion), .by_group = TRUE) %>%  # Sort treatment_type by proportion within each country
  ungroup()

# Define custom colors for each treatment type
custom_colors <- c("proportion_poverty_treatment1" = "#1b9e77",  # Dark green
                   "proportion_poverty_treatment2" = "#d95f02",  # Orange
                   "proportion_poverty_treatment3" = "#7570b3",  # Purple
                   "proportion_poverty_treatment4" = "#e7298a")  # Pink

# Step 5: Plot with customized settings
poverty_line_expos<- ggplot(poverty_long, aes(x = country_iso3_code, y = proportion, fill = treatment_type)) +
  geom_bar(stat = "identity", position = "identity", color = "black", alpha = 0.7) +  # black border for bars
  scale_fill_manual(values = custom_colors, labels = c("Group 1: Ceftriaxone+Amikacin/Gentamicin", "Group 2: Piperacillin+Tazobactam", "Group 3: Meropenem", "Group 4: Colistin")) +
  coord_flip() +
  labs(x = "Country ISO-3 code", y = "Percentage of the population at risk of falling below the national poverty line \ndue to out-of-pocket costs for sepsis treatment of unknown causative agent") +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = c(0.65, 0.85),  # Position legend inside plot, top-right
    legend.title = element_blank(),   # Remove legend title
    legend.box.background = element_rect(fill = NA, color = "white"),  # White background and contour for legend box
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines for cleaner look
  )

setwd("/Users/lsh1807578/CISS Dropbox/kasim allel henriquez/B_projects/0_UniversityofOxford/ADILA/0_Figuresoutputs/")
ggsave("poverty_line_expos.tiff", plot = poverty_line_expos, device = "tiff", width =7, height = 9, dpi = 1000, units = "in")

#######

pop_adjusted_povertynumb <- merge(result_with_poverty, data_2019, by = "country_iso3_code")
View(pop_adjusted_povertynumb)


#-------------------------------------------------------------#
# NoteS: Price ratio using Pharma14 ex-factory/retail is found in other script HEA_analysis:  (8) Calculations.
#-------------------------------------------------------------#



