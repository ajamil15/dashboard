library(readr)
library(leaflet)
library(dplyr)
library(DT)
library(sf)
library(tigris)


data = readRDS("/Users/aliajamil/Desktop/r/work/ITA_FIPS.rds")
updated = read_csv("/Users/aliajamil/Desktop/r/work/Updated.csv")
#data2 = read_csv("/Users/aliajamil/Desktop/r/work/ITACaseDetail.csv")
#requires geocoded dataset ^

counties_sf = counties(cb = TRUE, resolution = "20m", class = "sf", year = 2020)
state_sf = states(cb = TRUE, resolution = "20m", class = "sf", year = 2020)
cross = read_csv("/Users/aliajamil/Desktop/r/work/ZIP-COUNTY-FIPS_2017-06.csv")

data %>% summarise(across(everything(), ~n_distinct(.)/n()))

a = unique(data$establishment_id)
b = unique(updated$establishment_id)
i = intersect(a, b)

d = data[, c(5, 178)]
d = d %>%
  distinct()
  #mutate(identifier = paste(street_address, city, state, zip_code, sep = "_"))

final = left_join(updated, d, by = "establishment_id")

na = final %>%
  filter(is.na(GEOID))

unique(na$establishment_name)

#replace missing values
zips = unique(na$zip_code)
c = cross %>%
  filter(ZIP %in% zips) 
count_c = c %>%
  count(ZIP)
c = c [,c(1,4)]
test = left_join(na, c, by = c("zip_code" = "ZIP"))
  


#not updated
cross %>% filter(ZIP == "")
final = final %>%
  mutate(GEOID = case_when(
    is.na(COUNTYNAME) & zip_code == "39530" ~ '28047',
    is.na(COUNTYNAME) & zip_code == "44870" ~ '39043',
    is.na(COUNTYNAME) & zip_code == "77550" ~ '48167',
    is.na(COUNTYNAME) & zip_code == "802" ~ '78030',
    is.na(COUNTYNAME) & zip_code == "97103" ~ '41007',
    is.na(COUNTYNAME) & zip_code == "98121" ~ '53033',
    is.na(COUNTYNAME) & zip_code == "783" ~ '72047',
    is.na(COUNTYNAME) & zip_code == "92037" ~ '06073',
    is.na(COUNTYNAME) & zip_code == "99612" ~ '02013',
    TRUE ~ GEOID
  ))


#remove territories without shapefiles
a = unique(counties_sf$STUSPS)
b = unique(data$state)
c = setdiff(b, a)
#filter out VI, GU, MP, AS
c = c("VI", "GU", "MP", "AS")
final = final %>%
  filter(!state %in% c)

#write data file
?write_rds
write_rds(final, 'ITA_FIPS.rds')
final = read_rds("/Users/aliajamil/Desktop/r/work/dashboard_github/US_dashboard/ITA_FIPS.rds")


#code testing
#correct
agg_data = data %>%
  group_by(GEOID, state) %>%
  summarise(
    total_injuries = n(),
    .groups = "drop"
  )

#if, filtered_state = state_sf %>% filter(STUSPS == input$state)

joined_data = state_sf %>%
  left_join(agg_data, join_by ("STUSPS" == "state"))
#NAME column for labels
county_name = final %>%
  filter(GEOID == 42091) %>%
  distinct(COUNTYNAME) %>%
  pull(COUNTYNAME)

choices = data %>%
  filter(state == "NJ") %>%
  distinct(COUNTYNAME) %>%
  arrange(COUNTYNAME) %>%
  pull(COUNTYNAME)

agg_data = data %>%
  group_by(GEOID, COUNTYNAME, state) %>%
  summarise(
    total_injuries = n())
summary(agg_data$total_injuries)

calc = agg_data %>%
  filter(total_injuries > 5000)
#1, 15, 50, 100, 200, 300, 2500, 5000, 10000, 15000

agg_data = agg_data %>%
  group_by(GEOID, COUNTYNAME, state) %>%
  summarise(total_injuries = sum(total_injuries, na.rm = TRUE),
            hours_worked = sum(hours_worked, na.rm = TRUE),
            incidence_avg = mean(incidence, na.rm = TRUE), .groups = "drop")

agg_data = agg_data %>%
  filter(state == "CT")

range(agg_data$incidence_avg)
hist(agg_data$incidence_avg)


state_counties <- counties_sf %>%
  filter(STUSPS == "CT")
joined_data <- state_counties %>%
  left_join(agg_data, by = "GEOID")

st_crs(joined_data)

bounds = joined_data %>%
  filter(state == "NJ") %>%
  st_bbox()


agg_datas = final %>%
  group_by(establishment_name, GEOID, COUNTYNAME, state) %>%
  summarise(
    total_injuries = n(),
    hours_worked = sum(total_hours_worked, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(incidence = ifelse(hours_worked > 0, (total_injuries * 200000) / hours_worked, NA))

agg_datas = agg_datas %>%
  group_by(GEOID, COUNTYNAME, state) %>%
  summarise(total_injuries = sum(total_injuries, na.rm = TRUE),
            hours_worked = sum(hours_worked, na.rm = TRUE),
            incidence_avg = mean(incidence, na.rm = TRUE), .groups = "drop")

  # Join with shapefile
  map_data <- counties_sf %>%
    left_join(agg_datas, by = "GEOID")
head(map_data)
  
  # Plot in Albers projection
  ggplot(map_data_albers) +
    geom_sf(aes(fill = incidence_avg), color = "#FFFFFF") +
    scale_fill_viridis_c(
      option = "magma",
      direction = -1,
      name = "Injury Incidence Rate"
    ) +
    coord_sf(crs = st_crs(5070), expand = FALSE) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )
  
data = read_csv("ITA_FIPS.csv")

  filtered = data
    filtered <- filtered %>% filter(state == "NJ")

    filtered <- filtered %>% filter(COUNTYNAME == "Essex County")

  filtered = filtered %>%
    group_by(establishment_name) %>%
    mutate(total_injuries = n(),
           incidence = ifelse(total_hours_worked > 0, (total_injuries * 200000) / total_hours_worked, NA))%>%
    ungroup()
  
  
#zipcode fix
  data2 = readRDS("State_Dashboard/ITA_FIPS.rds")

data2$zip_code = as.character(data2$zip_code)

unique(nchar(data2$zip_code))

eight_seven = data %>%
  filter(nchar(data$zip_code) < 6)
list = unique(eight_seven$zip_code)


cleanzip = data %>%
  filter(zip_code %in% list) %>%
  distinct(arcgis_address, .keep_all = TRUE)
cleanzip = cleanzip [, c(12, 43)]
cleanzip$zip_code = format(cleanzip$zip_code, scientific = FALSE, trim = TRUE)

library(stringr)
cleanzip = cleanzip %>%
  mutate(last5 = str_sub(arcgis_address, -5))
cleanzip = cleanzip %>%
  mutate(last5 = case_when(
    last5 %in% c("akota", "aware", "esota", "ginia", "olina", "oming", "onsin", "orida", "ornia", "raska", "sippi")
      ~ str_sub(arcgis_address, 1, 5),
    last5 %in% c("lines", "icine", "h Eby", "enter") ~ NA,
    TRUE ~ last5))

data2$zip_code = format(data2$zip_code, scientific = FALSE, trim = TRUE)

data2 = data2 %>%
  mutate(zip_code = case_when(
    nchar(zip_code) == 4 ~ paste0("0", zip_code),
    nchar(zip_code) == 3 ~ paste0("00", zip_code),
    nchar(zip_code) >= 7 ~ cleanzip$last5[match(zip_code, cleanzip$zip_code)],
    TRUE ~ zip_code))

unique(nchar(data2$zip_code))

saveRDS(data2, "ITA_FIPS.rds")


