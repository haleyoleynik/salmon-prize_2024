#### Salmon Prize Data Processing ####
### author: Lilian Hart ###
# date last modified: 04/22/24

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

#### Create new empty dataframe (Short Version A) ####
col_names <- c("System", "Stock", "BroodYear", "Escapement", "Recruits", "AgeClass_0.1", 
               "AgeClass_0.2", "AgeClass_0.3", "AgeClass_0.4", "AgeClass_0.5",
               "AgeClass_1.1", "AgeClass_1.2", "AgeClass_1.3", "AgeClass_1.4",
               "AgeClass_1.5", "AgeClass_2.1", "AgeClass_2.2", "AgeClass_2.3",
               "AgeClass_2.4", "AgeClass3.1", "AgeClass_3.2", "AgeClass_3.3",
               "AgeClass_3.4")
brood.df <- data.frame(matrix(nrow = 0, ncol = length(col_names))) 
colnames(brood.df) <- col_names
# Convert data types by column
brood.df[, 1:3] <- sapply(brood.df[, 1:3], as.character)
brood.df[, 4:23] <- sapply(brood.df[, 4:23], as.numeric)
#### Create single Bristol Bay dataframe (Short Version A) ####
b_ala.dat$Stock <- "Alagnak"
b_ege.dat$Stock <- "Egegik"
b_igu.dat$Stock <- "Igushik"
b_kvi.dat$Stock <- "Kvichak"
b_nus.dat$Stock <- "Nushagak"
b_nak.dat$Stock <- "Naknek"
b_uga.dat$Stock <- "Ugashik"
b_woo.dat$Stock <- "Wood"

# Merge dataframes with rowbind
bb.dat <- rbind(b_ala.dat, b_ege.dat, b_igu.dat, b_kvi.dat, b_nus.dat,
                b_nak.dat, b_uga.dat, b_woo.dat)
# Add system info
bb.dat$System <- "Bristol_Bay"

## Export via standardized dataframe format ##
# Reorder column name order
bb.out <- bb.dat %>% select(System, Stock, Brood.Year, Escapement, Recruits, 
                            everything()) %>% select(-R.S)
colnames(bb.out) <- col_names 

# Save dataframe as .csv
write.csv(bb.out, paste0(dir.out, "/Bristol_Bay_Brood_Table.csv"))

#### Reformat Columbia River data ####
c.dat <- columbia.dat
c.dat$System <- "Columbia"
c.dat$Stock <- "Bonneville"
# Rename columns (Bon.Count values equal the sum across age classes, so renamed to Recruits)
c.dat <- c.dat %>% rename(Recruits = Bon.Count, AgeClass_1.1 = X1.1,
                          AgeClass_1.2 = X1.2, AgeClass_1.3= X1.3, AgeClass_2.1 = X2.1,
                          AgeClass_2.2 = X2.2, AgeClass_2.3 = X2.3, AgeClass_3.1 = X3.1,
                          AgeClass_3.2 = X3.2, AgeClass_3.3 = X3.3, AgeClass_4.1 = X4.1,
                          AgeClass_4.2 = X4.2)
## Expand dataframe to standardized format (Short Version A) using join ##
# Re-classify brood year
c.dat$BroodYear <- as.character(c.dat$BroodYear)
c.out <- left_join(c.dat, brood.df)
# Reorder colnames using select
c.out2 <- c.out %>% select(all_of(col_names))
# Replace NAs with zeros
c.out2[is.na(c.out2)] <- 0
# Replace Escapement zeros with NA
c.out2$Escapement <- NA
# Save dataframe as .csv
write.csv(c.out2, paste0(dir.out, "/Columbia_Brood_Table.csv"))

#### Reformat Fraser River data ####
fras.dat <- fraser.dat
fras.dat$System <- "Fraser"
# Rename existing columns (Renamed total_broodyr_spawners to Escapement
# based on description in metadata, "Documentation of Fraser River sockeye 
# spawner and recruit data": "Total number of spawners estimated to have 
# returned to the spawning grounds in the brood year. Includes both adults and jacks.")
fras.dat <- fras.dat %>% rename(Stock = production_stock_name,
                                BroodYear = broodyr, 
                                Escapement = total_broodyr_spawners)
# Trim data, then spread to wide format by age
fras.dat <- fras.dat %>% select(System, Stock, BroodYear, Escapement, age, num_recruits) %>%
  group_by(BroodYear) %>% spread(age, num_recruits)
# Examine data structure
#head(fras.dat)

# Rename age class columns
fras.dat <- fras.dat %>% rename(AgeClass_1.1 = `21`, AgeClass_1.2 = `31`, 
                                AgeClass_2.1 = `32`, AgeClass_1.3 = `41`,
                                AgeClass_2.2 = `42`, AgeClass_3.1 = `43`,
                                AgeClass_1.4 = `51`, AgeClass_2.3 = `52`,
                                AgeClass_3.2 = `53`, AgeClass_2.4 = `62`,
                                AgeClass_3.3 = `63`, AgeClass_4.2 = `64`,
                                AgeClass_2.5 = `72`)
# Save back to dataframe format
fras.dat <- as.data.frame(fras.dat)

# Check: are any fish in age class 2.5?
unique(fras.dat$AgeClass_2.5) # 0 and NA, so no
unique(fras.dat$AgeClass_2.1) #200+ unique values
# Drop age class 2.5
fras.dat <- fras.dat %>% select(-c(AgeClass_2.5))
# Change data structure of brood year for consistency
fras.dat$BroodYear <- as.character(fras.dat$BroodYear)
# Left join with standardized dataframe to add in missing columns
fras.out <- left_join(fras.dat, brood.df)
# Replace age class NAs with zeros
fras.out[is.na(fras.out)] <- 0
# Reorder colnames using select
fras.out2 <- fras.out %>% select(all_of(col_names))
# Calculate and populate total # recruits by brood year (sum across age classes)
fras.out2 <- fras.out2 %>% mutate(sum = rowSums(.[6:23]))
fras.out2$Recruits <- fras.out2$sum
fras.out3 <- fras.out2 %>% select(-sum)
# Save dataframe as .csv
write.csv(fras.out3, paste0(dir.out, "/Fraser_Brood_Table.csv"))

##### Save final Short Version A dataframe objects as RDS for later use ####
saveRDS(fras.out3, paste0(dir.out, "/fraser_dat.rds"))
saveRDS(c.out2, paste0(dir.out, "/columbia_dat.rds"))
saveRDS(bb.out, paste0(dir.out, "/bristol_dat.rds"))

#### Compile data for all systems using rowbind ####
sys.out <- rbind(bb.out, c.out2, fras.out3)
# Inspect new dataframe
unique(sys.out$System)
unique(sys.out$BroodYear)
head(sys.out)
# Save dataframe as.csv
write.csv(sys.out, paste0(dir.out, "/Bristol_Columbia_Fraser_combined.csv"))
