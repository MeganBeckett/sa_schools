# ==================================================================================================
#                          1. Data preparation - Clean and Tidy                                   #
# ==================================================================================================

# LIBRARIES ----------------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(tidylog)
library(here)
library(lubridate)
library(naniar)

# DATA PREP ----------------------------------------------------------------------------------------
# One file per province in South Africa
ec <- read_xlsx(here::here("data_raw/EasternCape.xlsx"))
fs <- read_xlsx(here::here("data_raw/Freestate.xlsx"))
gau <- read_xlsx(here::here("data_raw/Gauteng.xlsx"))
kzn <- read_xlsx(here::here("data_raw/KwaZulu_Natal.xlsx"))
lim <- read_xlsx(here::here("data_raw/Limpopo.xlsx"))
mpu <- read_xlsx(here::here("data_raw/Mpumalanga.xlsx"))
nc <- read_xlsx(here::here("data_raw/NorthernCape.xlsx"))
nw <- read_xlsx(here::here("data_raw/NorthWest.xlsx"))
wc <- read_xlsx(here::here("data_raw/WesternCape.xlsx"))


# Combine files
sa_schools <- rbind(ec, fs, gau, kzn, lim, mpu, nc, nw, wc)

summary(sa_schools)

# CLEAN AND TIDY DATA ------------------------------------------------------------------------------

# Missing data - replace fullstops
sa_schools <- sa_schools %>%
  replace_with_na_all(condition = ~.x == ".")

# Missing data - replace 'NOT APPLICABLE'
sa_schools <- sa_schools %>%
  replace_with_na_all((condition = ~.x == "NOT APPLICABLE"))

# Variable types
sa_schools <- sa_schools %>%
  mutate(NatEmis = as.numeric(NatEmis),
         Quintile = as.numeric(Quintile),
         Educators_number_2017 = as.numeric(`Educator_Number 2017`),
         Learner_number_2017 = as.numeric(`Learner_Number 2017`)
         )

# Multiple data formats for RegistrationDate
sa_schools$dmy <- dmy(sa_schools$RegistrationDate)
sa_schools$mdy <- mdy(sa_schools$RegistrationDate)
sa_schools$ymd <- ymd(sa_schools$RegistrationDate)

sa_schools$dmy[is.na(sa_schools$dmy)] <- sa_schools$mdy[is.na(sa_schools$dmy)] # Replace with mdy if NA
sa_schools$dmy[is.na(sa_schools$dmy)] <- sa_schools$ymd[is.na(sa_schools$dmy)] # Replace with ymd if NA

# Remove incorrect dates (ie. >= 2018)
sa_schools <- sa_schools %>%
  mutate(dmy = ifelse(dmy >= "2018-01-01", NA, dmy))

# Substitute back into RegistrationDate
sa_schools$RegistrationDate <- sa_schools$dmy

# Assess unique values to determine variables to convert to factors
sa_schools %>%
  summarise_all(funs(n_distinct(.))) %>%
  gather(value = "unique_values") %>%
  filter(unique_values <= 10) %>%
  arrange(unique_values)

unique(sa_schools$Sector) # Convert 'Public' to 'PUBLIC' (ie. toupper)
unique(sa_schools$NoFeeSchool)
unique(sa_schools$Open_Boarding_school)
unique(sa_schools$Phase)
unique(sa_schools$OwnerLand)
unique(sa_schools$Section21) # Convert to upper case
unique(sa_schools$Urban_Rural) # Convert to upper case
unique(sa_schools$Quintile)

# Apply above tidy transformations
sa_schools <- sa_schools %>%
  mutate(Sector = toupper(Sector),
         Section21 = toupper(Section21),
         Urban_Rural = toupper(Urban_Rural))

# Convert to factors
sa_schools <- sa_schools %>%
  mutate(Sector = as.factor(Sector),
         Section21 = as.factor(Section21),
         NoFeeSchool = as.factor(NoFeeSchool),
         Urban_Rural = as.factor(Urban_Rural),
         Open_Boarding_school = as.factor(Open_Boarding_school),
         Phase = as.factor(Phase),
         OwnerLand = as.factor(OwnerLand))


# SAVE RDS file
saveRDS(sa_schools, "data_prep/01_sa_schools.RDS")
