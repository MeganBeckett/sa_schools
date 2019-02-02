# ==================================================================================================
#                                         Data preparation                                         #
# ==================================================================================================

# LIBRARIES ----------------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(here)
library(lubridate)
library(naniar)

# DATA PREP ----------------------------------------------------------------------------------------
# One file per province in South Africa
ec <- read_xlsx(here("data_raw/EasternCape.xlsx"))
fs <- read_xlsx(here("data_raw/Freestate.xlsx"))
gau <- read_xlsx(here("data_raw/Gauteng.xlsx"))
kzn <- read_xlsx(here("data_raw/KwaZulu_Natal.xlsx"))
lim <- read_xlsx(here("data_raw/Limpopo.xlsx"))
mpu <- read_xlsx(here("data_raw/Mpumalanga.xlsx"))
nc <- read_xlsx(here("data_raw/NorthernCape.xlsx"))
nw <- read_xlsx(here("data_raw/NorthWest.xlsx"))
wc <- read_xlsx(here("data_raw/WesternCape.xlsx"))

# Combine files
sa_schools <- rbind(ec, fs, gau, kzn, lim, mpu, nc, nw, wc)

summary(sa_schools)

# CLEAN AND TIDY DATA ------------------------------------------------------------------------------

# Missing data
sa_schools <- sa_schools

# Variable types
sa_schools <- sa_schools %>%
  mutate(Quintile = as.numeric(Quintile))
