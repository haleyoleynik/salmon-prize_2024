#### Salmon Prize Data Processing ####
### author: Lilian Hart ###
# date last modified: 06/04/24

require(tidyverse)
require(dplyr)
require(here)

dir.dat <- here("data", "brood_tables_renamed")
dir.out <- here("data", "data_product")

#### Read in data ####
b_ala.dat <- read.csv(paste0(dir.dat, "/Bristol_Alagnak_BT.csv"))
b_ege.dat <- read.csv(paste0(dir.dat, "/Bristol_Egegik_BT.csv"))
b_igu.dat <- read.csv(paste0(dir.dat, "/Bristol_Igushik_BT.csv"))
b_kvi.dat <- read.csv(paste0(dir.dat, "/Bristol_Kvichak_BT.csv"))
b_nak.dat <- read.csv(paste0(dir.dat, "/Bristol_Naknek_BT.csv"))
b_nus.dat <- read.csv(paste0(dir.dat, "/Bristol_Nushagak_BT.csv"))
b_uga.dat <- read.csv(paste0(dir.dat, "/Bristol_Ugashik_BT.csv"))
b_woo.dat <- read.csv(paste0(dir.dat, "/Bristol_Wood_BT.csv"))

columbia.dat <- read.csv(paste0(dir.dat, "/Columbia_BT.csv"))

fraser.dat <- read.csv(paste0(dir.dat, "/Fraser_BT.csv"))

#### Create new empty dataframes ####
#(Wide Brood Table)
col_names <- c("System", "River", "BroodYear", "Total_Recruits", "AgeClass_0.1", 
               "AgeClass_0.2", "AgeClass_0.3", "AgeClass_0.4", "AgeClass_0.5",
               "AgeClass_1.1", "AgeClass_1.2", "AgeClass_1.3", "AgeClass_1.4",
               "AgeClass_1.5", "AgeClass_2.1", "AgeClass_2.2", "AgeClass_2.3",
               "AgeClass_2.4", "AgeClass_3.1", "AgeClass_3.2", "AgeClass_3.3",
               "AgeClass_3.4")

brood.df <- data.frame(matrix(nrow = 0, ncol = length(col_names))) 
colnames(brood.df) <- col_names
brood.df <- brood.df %>% mutate(across(System:River, as.character)) %>%
  mutate(across(BroodYear:AgeClass_3.4, as.numeric))

#(Wide Return Rable)
col_namesB <- c("System", "River", "ReturnYear", "Total_Returns", "AgeClass_0.1", 
               "AgeClass_0.2", "AgeClass_0.3", "AgeClass_0.4", "AgeClass_0.5",
               "AgeClass_1.1", "AgeClass_1.2", "AgeClass_1.3", "AgeClass_1.4",
               "AgeClass_1.5", "AgeClass_2.1", "AgeClass_2.2", "AgeClass_2.3",
               "AgeClass_2.4", "AgeClass_3.1", "AgeClass_3.2", "AgeClass_3.3",
               "AgeClass_3.4")

return.df <- data.frame(matrix(nrow = 0, ncol = length(col_namesB))) 
colnames(return.df) <- col_namesB
return.df <- return.df %>% mutate(across(System:River, as.character)) %>%
  mutate(across(ReturnYear:AgeClass_3.4, as.numeric))

#### Bristol Bay ####
b_ala.dat$River <- "Alagnak"
b_ege.dat$River <- "Egegik"
b_igu.dat$River <- "Igushik"
b_kvi.dat$River <- "Kvichak"
b_nus.dat$River <- "Nushagak"
b_nak.dat$River <- "Naknek"
b_uga.dat$River <- "Ugashik"
b_woo.dat$River <- "Wood"

# Merge dataframes with rowbind
bb.dat <- rbind(b_ala.dat, b_ege.dat, b_igu.dat, b_kvi.dat, b_nus.dat,
                b_nak.dat, b_uga.dat, b_woo.dat)
# Add system info
bb.dat$System <- "Bristol Bay"

# Reorder column name order and select desired columns
bb.out <- bb.dat %>% select(System, River, Brood.Year, Recruits,
                            everything()) %>% 
  select(-c(Escapement, R.S))
colnames(bb.out) <- col_names 

## Save Wide Brood Table dataframe as .csv
write.csv(bb.out, paste0(dir.out, "/Bristol_Bay_Brood_Table.csv"))

### Bristol Bay Wide Return Table ###
# Remove Total_Recruits column so that join will work
bb2 <- bb.out %>% select(-Total_Recruits)

# Collapse by age
bb2 <- bb2 %>% pivot_longer(cols = starts_with("AgeCl"), names_to = "Age",
                            values_to = "Returns")
# Calculate and Add Return Year
bb2 <- bb2 %>% mutate(ReturnYear = case_when(
  Age == "AgeClass_0.1" ~ BroodYear + 2,
  Age == "AgeClass_0.2" ~ BroodYear + 3,
  Age == "AgeClass_0.3" ~ BroodYear + 4,
  Age == "AgeClass_0.4" ~ BroodYear + 5,
  Age == "AgeClass_0.5" ~ BroodYear + 6,
  Age == "AgeClass_1.1" ~ BroodYear + 3,
  Age == "AgeClass_1.2" ~ BroodYear + 4,
  Age == "AgeClass_1.3" ~ BroodYear + 5,
  Age == "AgeClass_1.4" ~ BroodYear + 6,
  Age == "AgeClass_1.5" ~ BroodYear + 7,
  Age == "AgeClass_2.1" ~ BroodYear + 4,
  Age == "AgeClass_2.2" ~ BroodYear + 5,
  Age == "AgeClass_2.3" ~ BroodYear + 6,
  Age == "AgeClass_2.4" ~ BroodYear + 7,
  Age == "AgeClass_3.1" ~ BroodYear + 5,
  Age == "AgeClass_3.2" ~ BroodYear + 6,
  Age == "AgeClass_3.3" ~ BroodYear + 7,
  Age == "AgeClass_3.4" ~ BroodYear + 8))

# Spread ages back, grouped by Return Year
bb2 <- bb2 %>% select(-c(BroodYear)) 
bb2 <- bb2 %>% group_by(ReturnYear) %>% spread(Age, Returns)

# Remove dates before 1963 and after 2023
bb2 <- bb2 %>% filter(ReturnYear < 2024 & ReturnYear > 1962)

# Add Escapement back in using a join, referencing original brood year as the calendar year
#esc <- bb2 %>% select(River, BroodYear, Escapement) %>% distinct()
#bb6 <- left_join(bb5, esc, by = c("River" = "River", "ReturnYear" = "BroodYear"))

# Add in Total_Returns column
#bb6 <- data.frame(bb6)
#bb6 <- bb6 %>% mutate(Total_Returns = rowSums(.[4:21]))
bb2 <- data.frame(bb2)
bb2 <- bb2 %>% mutate(Total_Returns = rowSums(.[4:21], na.rm = TRUE))
# Standardize column order
bb2 <- bb2 %>% select(all_of(col_namesB))

# Export Wide Return Table
write.csv(bb2, paste0(dir.out, "/Bristol_Bay_Return_Table.csv"))
  
#### Reformat Columbia River data ####
c.dat <- columbia.dat
c.dat$System <- "Columbia River"
c.dat$River <- "Bonneville Lock & Dam"

## Rename columns ##
# The year is actually the Return Year and negligible fishing happens.
# I conclude that Bon.Count can then represent the total run size (Total_Returns),
c.dat <- c.dat %>% rename(Total_Returns = Bon.Count, AgeClass_1.1 = X1.1,
                          AgeClass_1.2 = X1.2, AgeClass_1.3= X1.3, AgeClass_2.1 = X2.1,
                          AgeClass_2.2 = X2.2, AgeClass_2.3 = X2.3, AgeClass_3.1 = X3.1,
                          AgeClass_3.2 = X3.2, AgeClass_3.3 = X3.3, AgeClass_4.1 = X4.1,
                          AgeClass_4.2 = X4.2)
## Expand dataframe to standardized format (Wide Return Table) using join ##
# Re-classify year first
c.out <- left_join(c.dat, return.df)
# Reorder colnames using select
c.out <- c.out %>% select(all_of(col_namesB))
# Replace NAs with zeros
c.out[is.na(c.out)] <- 0
# Save Wide Return table as .csv
write.csv(c.out, paste0(dir.out, "/Columbia_Return_Table.csv"))

#### Reformat to Wide Brood Table ####
### Calculate Brood Year ###
# Remove Total_Returns to make join possible
cbrood <- c.out %>% select(-Total_Returns)
# Collapse by age
cbrood <- cbrood %>% pivot_longer(cols = starts_with("AgeCl"), names_to = "Age",
                            values_to = "Recruits")
# Calculate and Add Brood Year
cbrood <- cbrood %>% mutate(BroodYear = case_when(
  Age == "AgeClass_0.1" ~ ReturnYear - 2,
  Age == "AgeClass_0.2" ~ ReturnYear - 3,
  Age == "AgeClass_0.3" ~ ReturnYear - 4,
  Age == "AgeClass_0.4" ~ ReturnYear - 5,
  Age == "AgeClass_0.5" ~ ReturnYear - 6,
  Age == "AgeClass_1.1" ~ ReturnYear - 3,
  Age == "AgeClass_1.2" ~ ReturnYear - 4,
  Age == "AgeClass_1.3" ~ ReturnYear - 5,
  Age == "AgeClass_1.4" ~ ReturnYear - 6,
  Age == "AgeClass_1.5" ~ ReturnYear - 7,
  Age == "AgeClass_2.1" ~ ReturnYear - 4,
  Age == "AgeClass_2.2" ~ ReturnYear - 5,
  Age == "AgeClass_2.3" ~ ReturnYear - 6,
  Age == "AgeClass_2.4" ~ ReturnYear - 7,
  Age == "AgeClass_3.1" ~ ReturnYear - 5,
  Age == "AgeClass_3.2" ~ ReturnYear - 6,
  Age == "AgeClass_3.3" ~ ReturnYear - 7,
  Age == "AgeClass_3.4" ~ ReturnYear - 8))

## Spread ages back, grouped by Brood Year
# Remove Return Year column first
cb2 <- cbrood %>% select(-ReturnYear)
cb3 <- cb2 %>% group_by(Age,BroodYear) %>% 
  pivot_wider(names_from = Age, values_from = Recruits)

## Calculate Total Recruits
cb3 <- data.frame(cb3)
cb3 <- cb3 %>% mutate(Total_Recruits = rowSums(.[4:21], na.rm = TRUE))
# Remove recruitment calculations for years 2016 and onwards 
# (full reconstruction not possible yet)
cb3 <- cb3 %>%
  mutate(Total_Recruits = case_when(BroodYear > 2015 ~ NA,
                                    TRUE ~ as.numeric(Total_Recruits)))

# Reorder columns and sort by brood year
cb3 <- cb3 %>% select(all_of(col_names)) %>% arrange(BroodYear)
# Remove years before 1980. Seems like that brood year corresponds to 
# the earliest, oldest fish caught. See raw table, where in 1986 returns were 
# recorded for age 2.3 fish.
cb3 <- cb3 %>% filter(BroodYear >= 1980)
# Analysts might also want to disregard total recruit values for 1980, 1981, and
# 1982 (change to NA), because full age values could not be captured given
# that sampling began in 1985.
cb3 <- cb3 %>% mutate(Total_Recruits = case_when(BroodYear < 1983 ~ NA,
                                                 TRUE ~ as.numeric(Total_Recruits)))

# Save Wide Brood Table to .csv
write.csv(cb3, paste0(dir.out, "/Columbia_Brood_Table.csv"))

#### Reformat Fraser River data ####
fras.dat <- fraser.dat
fras.dat$System <- "Fraser River"
# Rename existing columns 
fras.dat <- fras.dat %>% rename(River = production_stock_name,
                                BroodYear = broodyr)
# Trim data, then spread to wide format by age
fras.dat <- fras.dat %>% select(System, River, BroodYear, age, num_recruits) %>%
  group_by(BroodYear) %>% spread(age, num_recruits)
# Examine data structure
#head(fras.dat)

# Rename age class columns
fras.dat <- fras.dat %>% rename(AgeClass_0.1 = `21`, AgeClass_0.2 = `31`, 
                                AgeClass_1.1 = `32`, AgeClass_0.3 = `41`,
                                AgeClass_1.2 = `42`, AgeClass_2.1 = `43`,
                                AgeClass_0.4 = `51`, AgeClass_1.3 = `52`,
                                AgeClass_2.2 = `53`, AgeClass_1.4 = `62`,
                                AgeClass_2.3 = `63`, AgeClass_3.2 = `64`,
                                AgeClass_1.5 = `72`)
# Save back to dataframe format
fras.dat <- as.data.frame(fras.dat)

# Left join with standardized dataframe to add in missing columns
fras.out <- left_join(fras.dat, brood.df)
# Reorder colnames using select
fras.out2 <- fras.out %>% select(all_of(col_names))
# Calculate and populate total # recruits by brood year (sum across age classes)
fras.out2 <- fras.out2 %>% mutate(Total_Recruits = rowSums(.[5:22], na.rm = TRUE))

# Remove recruitment calculations for years 2016 and onwards 
# (full reconstruction not possible yet)
fras.out2 <- fras.out2 %>%
  mutate(Total_Recruits = case_when(BroodYear > 2015 ~ NA,
                             TRUE ~ as.numeric(Total_Recruits)))
# Save Wide Brood Table as .csv
write.csv(fras.out2, paste0(dir.out, "/Fraser_Brood_Table.csv"))

### Reformat Fraser R Data to Wide Return Table ###
# Remove Total_Recruits column so that join will work
fr2 <- fras.out2 %>% select(-Total_Recruits)

# Collapse by age
fr2 <- fr2 %>% pivot_longer(cols = starts_with("AgeCl"), names_to = "Age",
                            values_to = "Returns")
# Calculate and Add Return Year
# Convert
fr2 <- fr2 %>% mutate(ReturnYear = case_when(
  Age == "AgeClass_0.1" ~ BroodYear + 2,
  Age == "AgeClass_0.2" ~ BroodYear + 3,
  Age == "AgeClass_0.3" ~ BroodYear + 4,
  Age == "AgeClass_0.4" ~ BroodYear + 5,
  Age == "AgeClass_0.5" ~ BroodYear + 6,
  Age == "AgeClass_1.1" ~ BroodYear + 3,
  Age == "AgeClass_1.2" ~ BroodYear + 4,
  Age == "AgeClass_1.3" ~ BroodYear + 5,
  Age == "AgeClass_1.4" ~ BroodYear + 6,
  Age == "AgeClass_1.5" ~ BroodYear + 7,
  Age == "AgeClass_2.1" ~ BroodYear + 4,
  Age == "AgeClass_2.2" ~ BroodYear + 5,
  Age == "AgeClass_2.3" ~ BroodYear + 6,
  Age == "AgeClass_2.4" ~ BroodYear + 7,
  Age == "AgeClass_3.1" ~ BroodYear + 5,
  Age == "AgeClass_3.2" ~ BroodYear + 6,
  Age == "AgeClass_3.3" ~ BroodYear + 7,
  Age == "AgeClass_3.4" ~ BroodYear + 8))

# Spread ages back, grouped by Return Year
fr2 <- fr2 %>% select(-c(BroodYear)) 
fr2 <- fr2 %>% group_by(ReturnYear) %>% spread(Age, Returns)

# Remove dates before 1951 and after 2023
fr2 <- fr2 %>% filter(ReturnYear < 2024 & ReturnYear > 1950)

# Add in Total_Returns column
fr2 <- data.frame(fr2)
fr2 <- fr2 %>% mutate(Total_Returns = rowSums(.[4:21], na.rm = TRUE))
# Convert NAs to zeros
# Replace NAs with zeros
fr2[is.na(fr2)] <- 0

# Reorder columns
fr2 <- fr2 %>% select(all_of(col_namesB))

# Save Wide Return table out
write.csv(fr2, paste0(dir.out, "/Fraser_Return_Table.csv"))

#### Compile data for all systems using rowbind ####
## Return table data ##
sys.out <- rbind(bb2, c.out, fr2)
# Inspect new dataframe
unique(sys.out$System)
unique(sys.out$ReturnYear)
head(sys.out)
# Save dataframe as.csv
write.csv(sys.out, paste0(dir.out, "/Bristol_Columbia_Fraser_Returns_combined.csv"))

## Brood table data ##
brood.out <- rbind(bb.out, cb3, fras.out2)
write.csv(brood.out, paste0(dir.out, "/Bristol_Columbia_Fraser_BroodT_combined.csv"))
