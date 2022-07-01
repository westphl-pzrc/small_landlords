#much of this script comes from: https://github.com/summer-of-maps/2019-PublicInterestLawCenter-PhilaHousing/blob/master/Code/Task1_CleanData.R
#https://github.com/summer-of-maps/2019-PublicInterestLawCenter-PhilaHousing
##################SET UP############################

library(tidyverse, quietly = T)
library(tidycensus, quietly = T)
library(acs, quietly = T)
library(sf, quietly = T)
library(ggpubr, quietly = T)
library(lubridate, quietly = T)
library(ggmap, quietly = T)
library(stringr, quietly = T)
library(tigris, quietly = T)

options(tigris_use_cache = TRUE)

setwd("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Data/R Scripts and Datasets/Small Landlords Side Project")

##################LOAD DATA############################

#OPA
opa <- read.csv('./opa_properties_public.csv')

#L&I business licenses
licenses <- read_csv('./business_licenses.csv')

licenses_clean = licenses |>
  drop_na(lat) |>
  drop_na(lng) %>%
  select(-c(licensenum,
            revenuecode,
            censustract, 
            objectid, 
            council_district))

licenses = st_as_sf(licenses, coords = c("lng", "lat"), crs = st_crs(2272))

#vacant property indicators
vacant <- st_read('./Vacant_Indicators_Bldg/Vacant_Indicators_Bldg.shp') %>%
  st_transform(crs=2272)


##################CLEAN DATA############################
#OPA CLEANING

opa_clean <- opa %>% 
  select(parcel_number,
         lng, 
         lat, 
         location, 
         objectid, 
         zip_code, 
         mailing_address_1,
         mailing_address_2, 
         mailing_care_of, 
         mailing_city_state,
         mailing_street, 
         mailing_zip, 
         owner_1, 
         owner_2,
         category_code_description,
         building_code_description) %>%
  filter(category_code_description!="Commercial") %>% #remove non-residential rentals
  #CREATE BINARY FACTOR VARIABLES FOR VACANCY/IN OUT OF PHILLY/MATCHING TAX ADDRESS
  mutate(mailing_street2 = ifelse(mailing_street=="", location, mailing_street)) %>%
  mutate(matching_tax_address = ifelse(location == mailing_street2, 1,0)) %>%
  mutate(vacant = ifelse(grepl("VACAN", building_code_description), 1,0)) %>%
  #create binary column for LL in or out of Philly
  mutate(mailing_city_state2 = ifelse(mailing_city_state=="", "PHILADELPHIA", mailing_city_state)) %>%
  mutate(inPhillyLL = ifelse(grepl("PHIL", mailing_city_state2), 1,0)) %>% 
  mutate(duplicated = duplicated(parcel_number)) %>%
  filter(duplicated=="FALSE")

#Check class
opa_clean$matching_tax_address = as.factor(opa_clean$matching_tax_address)
opa_clean$vacant = as.factor(opa_clean$vacant)
opa_clean$inPhillyLL = as.factor(opa_clean$inPhillyLL)

#Check NAs/Zeros/incomplete rows
#sum(is.na(opa_clean$location))

#subset OPA to non-vacant properties
opa_notvacant <- opa_clean %>% filter(vacant==0)

#pull out the top mailing addresses
top_mailing_address <- opa_clean %>% 
  group_by(mailing_street) %>%
  summarize(mailed_count = n())

#Compile those that are small property owners
small_multiple_property_owners <- top_mailing_address %>%
                                      filter(mailed_count > 1 & mailed_count <= 5)

#join back to full dataset
small_multiple_property_joined <- left_join(opa_clean, 
                                            top_mailing_address, 
                                            by = 'mailing_street') %>%
                                            mutate(mailed_count = ifelse(mailed_count == 361900, 1, mailed_count))

opa_clean_small_landlords <- small_multiple_property_joined %>% 
  filter(mailed_count <= 50)


#LICENSE CLEANING

#CUT DOWN TO 2019-2021
licenses_clean$year <- substring(licenses_clean$initialissuedate,1,4)

licenses_clean <- licenses_clean %>%
  filter(year == 2019|2020|2021)%>%
  mutate(ActiveLicense = ifelse(licenses_clean$licensestatus=="Active", 1,0))


#fill in 0s for NAs
licenses_clean$ActiveLicense[is.na(licenses_clean$ActiveLicense)] <- 0
licenses_clean$license <- 1

#filter down to just rental licenses
#subset RENTALS / VACANT RESIDENTIALS / HIGH RISE
license_rentals <- licenses_clean %>%
  filter(licensetype %in% c("Rental", 'High Rise'))

active_license_rentals<-license_rentals %>%
  filter(ActiveLicense == 1)
inactive_license_rentals<-license_rentals %>%
  filter(ActiveLicense == 0)

#REMOVE DUPLICATES, only unique licenses
license_rentals <- license_rentals[rev(order(as.Date(license_rentals$mostrecentissuedate))),]
license_rentals <- license_rentals[!duplicated(license_rentals$opa_account_num),]
#will drop by about 25%

#Vacant Cleaning
vacant$OPA_ID <- as.numeric(vacant$OPA_ID)
vacant$ADDRESS <- as.character(vacant$ADDRESS)
vacant$vacant <- 1
vacant$vacant <- as.factor(vacant$vacant)


##subset opa, to not vacant and not owner occupied
opa_notvacant_notOO <-opa_notvacant %>%
  filter(matching_tax_address == 0)

#join vacant indicators to opa by opa id
opa_notvacant_notOO$ADDRESS <- as.character(opa_notvacant_notOO$location)
opa_vacant_join <- left_join(opa_notvacant_notOO, vacant, by="ADDRESS")

#set vacant NAs to 0, because they did not match with a vacant parcel
opa_vacant_join$vacant.y <- as.numeric(opa_vacant_join$vacant.y)
opa_vacant_join$vacant.y[is.na(opa_vacant_join$vacant.y)] <- 0
#table(opa_vacant_join$vacant.y)

#clean to exclude vacant homes
opa_vacant_join2 <- opa_vacant_join %>%
  filter(opa_vacant_join$vacant.y != 1)

#join to rentals
small_landlords <- left_join(opa_vacant_join2, 
                             license_rentals, 
                             by=c('location'= 'address'))

small_landlords$ActiveLicense[is.na(opa_vacant_license$ActiveLicense)] <- 0
#table(opa_vacant_license$ActiveLicense)


small_landlords = small_landlords |>
  drop_na(lat) |>
  drop_na(lng)

small_landlords_clean = small_landlords |>
  dplyr::select(-matching_tax_address,
                -vacant.x,
                -vacant.y,
                -duplicated,
                -OBJECTID,
                -OWNER1,
                -OWNER2,
                -BLDG_DESC,
                -OPA_ID,
                -LNIADDRESS,
                -COUNCILDIS,
                -ZONINGBASE,
                -ZIPCODE,
                -BUILD_RANK,
                -the_geom,
                -the_geom_webmercator,
                -geometry.x,
                -geometry.y)

####################ADD ACS INDICATORS#################

acs_variable_list.2019 <- load_variables(2019,
                                         "acs5",
                                         cache = TRUE)


phl_demos <- get_acs(geography = "tract", 
                     year = 2019, #Most recent available year
                     variables = c(
                       "B01003_001E", #TotPop
                       "B05002_013E", #TotForeignBorn
                       "B17001_002E" #Income in the past 12 months below poverty level:
                     ),
                     geometry = T, #we want this as a .shp
                     state = "PA", # What state?
                     county = "Philadelphia", # What County?
                     output = "wide") |> # get a "wide" data type
  rename(tot_pop = "B01003_001E",
         tot_foreign_born = "B05002_013E",
         tot_low_inc = "B17001_002E")

phl_demos = phl_demos |>
              mutate(perc_foreign_born = tot_foreign_born /tot_pop,
                     perc_low_inc = tot_low_inc / tot_pop)

phl_demos$forborn_cut = case_when(
                          phl_demos$perc_foreign_born >= .25 ~ "Yes",
                          phl_demos$perc_foreign_born < .25 ~ "No"
                                                  )

################FINAL JOIN#########################
small_landlords_clean_geo = st_as_sf(small_landlords_clean, 
                                     coords = c("lat", "lng"), 
                                     crs = st_crs(phl_demos))

small_landlords_w_demos = small_landlords_clean_geo |>
                                  st_join(phl_demos)

small_landlords_w_demos$partial_unit_address = paste(small_landlords_w_demos$unit_type, 
                                                     small_landlords_w_demos$unit_num, 
                                                     sep = " ")

small_landlords_w_demos$full_unit_address = paste(small_landlords_w_demos$partial_unit_address, 
                                                  small_landlords_w_demos$ADDRESS, 
                                                  sep = ", ")

small_landlords_filtered = small_landlords_w_demos[!duplicated(small_landlords_w_demos$full_unit_address),]

small_landlords_filtered$mailing_address_full = paste(small_landlords_filtered$mailing_street2, 
                                                   small_landlords_filtered$mailing_city_state2, 
                                                 small_landlords_filtered$mailing_zip,
                                                  sep = ", ")

small_landlords_filtered_again = small_landlords_filtered |>
                                  dplyr::select(
                                        owner_1,
                                        owner_2,
                                        opa_account_num,
                                        legalfirstname,
                                        legallastname,
                                        legalname,
                                        legalentitytype,
                                        building_code_description,
                                        category_code_description,
                                        numberofunits,
                                        owneroccupied,
                                        business_name,
                                        business_mailing_address,
                                        mailing_address_full,
                                        zip_code,
                                        GEOID,
                                        NAME,
                                        tot_pop,
                                        tot_foreign_born,
                                        tot_low_inc,
                                        perc_foreign_born,
                                        perc_low_inc,
                                        forborn_cut,
                                        full_unit_address)

small_landlords_filtered_again_redux = small_landlords_filtered_again |>
                                          filter(!(numberofunits == 1 &
                                                     owneroccupied == "Yes"))

#This takes an annoyingly long time, ~15 minutes. Not sure why. Make it faster?
owner_one_list = small_landlords_filtered_again_redux |>
                      group_by(owner_1) |>
                      summarise(Freq = sum(numberofunits))

owner_one_list = as.data.frame(owner_one_list) |>
                    dplyr::select(-geometry)

small_landlords_owner_unit_count = left_join(small_landlords_filtered_again_redux,
                                             owner_one_list,
                                             by = "owner_1")

small_landlords_final = small_landlords_owner_unit_count |>
                          filter(Freq <= 5)

small_landlords_demo_cuts = small_landlords_final |>
                              filter(forborn_cut == "Yes" |
                                       perc_low_inc >= .2)

small_landlords_sample = sample_frac(small_landlords_demo_cuts, .10)

pz = read_sf("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Data/R Scripts and Datasets/General Boundaries/Shapefiles/PZ_Shapefile",
              "PZ_Boundaries",
              stringsAsFactors = FALSE)

pz = st_transform(pz, st_crs(small_landlords_sample))

pz_small_landlords_sample = small_landlords_sample[pz, ]

ggplot()+
  geom_sf(data = phl_tracts,
          color = "black",
          fill = NA,
          lwd = 0.2) +
  geom_sf(data = pz,
          color = "black",
          fill = NA,
          lwd = 0.5) +
  geom_sf(data = pz_small_landlords_sample,
          color = "darkred",
          alpha = 0.5) +
  coord_sf(xlim = c(-75.22, -75.175), ylim = c(39.95, 39.98), expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 3))