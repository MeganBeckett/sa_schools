# ==================================================================================================
#                          3. Data preparation - Selection for Shiny App                            #
# ==================================================================================================

# LIBRARIES ----------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidylog)
library(here)

# READ DATA ----------------------------------------------------------------------------------------
sa_schools_03 <- readRDS("data_prep/02_sa_schools.RDS")

# SELECT VARIABLES OF INTEREST ---------------------------------------------------------------------
sa_schools_03 <- sa_schools_03 %>%
  select(NatEmis, Name = Institution_Name, Province, Sector, Type = Type_DoE, Phase, Region = EIRegion,
         District = EIDistrict, Circuit = EICircuit, StreetAddress, Telephone, Section21, Quintile,
         NoFeeSchool, Urban_Rural, BoardingSchool = Open_Boarding_school, Learners = Learner_number_2017,
         Educators = Educator_number_2017, latitude, longitude)

# RENAME PROVINCES ---------------------------------------------------------------------------------
unique(sa_schools_03$Province)

sa_schools_03 <- sa_schools_03 %>%
  mutate(Province = if_else(Province == "EC", "EASTERN CAPE",
                           if_else(Province == "FS", "FREE STATE",
                                  if_else(Province == "GT", "GAUTENG",
                                         if_else(Province == "KZ", "KWA-ZULU NATAL",
                                                if_else(Province == "LP", "LIMPOPO",
                                                       if_else(Province == "MP", "MPUMALANGA",
                                                               if_else(Province == "NC", "NORTHERN CAPE",
                                                                       if_else(Province == "NW", "NORTH WEST",
                                                                               "WESTERN CAPE")))))))))

# SAVE ---------------------------------------------------------------------------------------------
saveRDS(sa_schools_03, "data_prep/03_sa_schools.RDS")

