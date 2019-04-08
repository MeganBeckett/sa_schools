# LIRRARIES
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(stringr)
library(readxl)
library(writexl)
library(stringdist)
library(ggmap)
library(sp)
library(glue)
library(RSQLite)
library(memoise)

# GEOCODE -------------------------------------------------------------------------------------------------------------

ADDRESS_COMPONENTS = c("street_number", "street_name", "locality", "sublocality", "suburb", "town", "province", "country", "postal_code")
#
parse_geocode_results <- function(results) {
  lapply(results, function(r) {
    cbind(
      tibble(
        geo_address = tryCatch({r$formatted_address}, error = function(e) NA),
        geo_lat = tryCatch({r$geometry$location$lat}, error = function(e) NA),
        geo_lon = tryCatch({r$geometry$location$lng}, error = function(e) NA),
        geo_type = tryCatch({r$geometry$location_type}, error = function(e) NA)
        # types = paste(r$types, collapse = ", "),
        # place_id = r$place_id,
        # partial_match = r$partial_match
      ),
      r$address_components %>%
        lapply(as_tibble) %>%
        bind_rows() %>%
        filter(!types %in% c("political", "sublocality_level_1")) %>%
        select(-short_name) %>% mutate(types =  as.character(types)) %>%
        mutate(
          types = case_when(
            types == "route" ~ "street_name",
            types == "administrative_area_level_1" ~ "province",
            types == "administrative_area_level_2" ~ "town",
            TRUE ~ types
          )
        ) %>%
        complete(types = ADDRESS_COMPONENTS) %>%
        mutate(
          # Assert order.
          types = factor(types, levels = ADDRESS_COMPONENTS),
          # Rename levels.
          types = fct_relabel(types, ~ paste0("geo_", .))
        ) %>%
        spread(types, long_name) %>%
        mutate(
          geo_suburb = unique(c(.$geo_sublocality, .$geo_locality)) %>% paste(collapse = ", "),
          geo_suburb = str_replace(geo_suburb, pattern = "NA,[:space:]|,[:space:]NA", replacement = ""),
          geo_suburb = ifelse(
            str_count(geo_suburb, "\\bDurban\\b") >= 1 & str_count(geo_town, "\\bDurban\\b") == 1,
            str_replace(geo_suburb, "\\b,[:space:]Durban$\\b", ""),
            geo_suburb),
          geo_town = str_replace(geo_town, pattern = "\\b[:space:]Metro$\\b", replacement = ""),
          geo_postal_code = as.integer(geo_postal_code)
        ) %>%
        select(-geo_locality, -geo_sublocality)
    )
  }) %>% bind_rows()
}
# Although geocode() can handle a vector of addresses, here we query only a single address at a time.
#
tolerant_geocode <- function(address, max_retry = 5) {
  message("-> ", address)

  retries <- 0
  #
  while (TRUE) {
    reply <- tryCatch(suppressMessages(geocode(address,
      output = "all",
      source = "google",
      inject = "region=za",
      override_limit = TRUE)),
      error = function(e) e,
      warning = function(w) {
        # print(paste("WARNING ", w))
        # if (grepl("SSL connect error", as.character(w))) {
        #   Sys.sleep(60)
        #   next
        # }
      })
    #
    if ("status" %in% names(reply)) break
    #
    cat("Waiting...")
    retries <- retries + 1
    Sys.sleep(5)
    cat(glue(" retry {retries}."), "\n")

    if (retries >= max_retry) return(NA)
  }

  # Check for no results.
  #
  if (reply$status == "ZERO_RESULTS") {
    return(NA)
  }

  parse_geocode_results(reply$results)
  # # Deal with duplicate matches (take match address which is closest to original address).
  # mutate(
  #   distance = stringdist(tolower(address), tolower(geo_address))
  # ) %>% arrange(distance) %>% select(-distance) %>% head(1)
}

# Create formatted address to use for geocoding:
gis_na <- mutate(gis_na,
  fmt_address = Institution_Name,
  fmt_address = ifelse(!is.na(StreetAddress), paste(fmt_address, StreetAddress, sep = ", "), fmt_address),
  fmt_address = ifelse(!is.na(fmt_address), paste0(fmt_address, ", South Africa"), fmt_address)
)

#  Geocode
geocode_gis_na <- gis_na %>%
  select(fmt_address, everything())
geocode_gis_na <- geocode_gis_na %>%
  mutate(geocode = map(fmt_address, possibly(tolerant_geocode, otherwise = NA)))
#
geocode_gis_na <- geocode_gis_na %>% filter(!NatEmis == 500448588)
geocode_gis_na <- geocode_gis_na %>%
  select(NatEmis, fmt_address, geocode) %>%
  unnest()