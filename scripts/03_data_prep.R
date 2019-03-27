# ==================================================================================================
#                          3. Data preparation - Selection for Shiny App                            #
# ==================================================================================================

# LIBRARIES ----------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidylog)
library(here)

# READ DATA ----------------------------------------------------------------------------------------
sa_schools_03 <- readRDS("data/02_sa_schools.RDS")

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

# NEW VARIABLES FOR LEARNER AND TEACHER CATEGORY ---------------------------------------------------
sa_schools_03 <- sa_schools_03 %>%
  mutate(Learners_Cat = ifelse(Learners <= 100, 300,
                               ifelse(Learners > 100 & Learners <= 200, 400,
                                      ifelse(Learners > 200 & Learners <= 500, 500,
                                             ifelse(Learners > 500 & Learners <= 1000, 600,
                                                    ifelse(Learners > 1000 & Learners <= 1500, 700,
                                                           ifelse(Learners > 1500, 800, 300))))))) %>%
  mutate(Educators_Cat = ifelse(Educators <= 10, 300,
                               ifelse(Educators > 10 & Educators <= 20, 400,
                                      ifelse(Educators > 20 & Educators <= 50, 500,
                                             ifelse(Educators > 50 & Educators <= 100, 600,
                                                    ifelse(Educators > 100 & Educators <= 150, 700,
                                                           ifelse(Educators > 150, 800, 300)))))))
# SAVE ---------------------------------------------------------------------------------------------
saveRDS(sa_schools_03, "data/03_sa_schools.RDS")

