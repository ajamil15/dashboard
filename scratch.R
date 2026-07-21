library(readr)
library(dplyr)
library(DT)
library(sf)
library(tigris)
library(stringr)
library(ggplot2)

#data
data = readRDS("/Users/aliajamil/Desktop/r/work/ITA_FIPS.rds")
  geoid = data [ , c(1, 43)]
  geoid = geoid %>% distinct()
naics = readxl:: read_xlsx("/Users/aliajamil/Desktop/r/work/NAICS.xlsx")
naics = naics %>%
  rename(naics_code_2digits = NAICS,
         naics_title_2digits = Sector)
naics$naics_code_2digits = as.character(naics$naics_code_2digits)
updated = read_csv("/Users/aliajamil/Desktop/r/work/ITA_Case_Detail_Data_2023.csv")


data1 = left_join(updated, geoid)
data1 = data1 %>%
  mutate(naics_code_2digits = str_sub(naics_code, end = 2))
data1 = left_join(data1, naics)

data2 = read_csv("/Users/aliajamil/Desktop/r/work/ITA_Case_Detail_Data_2024.csv")

counties_sf = counties(cb = TRUE, resolution = "20m", class = "sf", year = 2020)%>%
  st_transform(crs = 4326) %>%
  st_simplify(dTolerance = 500)

#merge
data2$zip_code = as.character(data2$zip_code)
data2$ein = as.character(data2$ein)
data2 = data2 %>%
  rename(year_filing_for = year_of_filing)
final = full_join(data1, data2)

#write data file
final = final [, c(2:3, 5, 8:10, 52, 16, 20, 33:39, 41, 43, 45, 47, 49:50)]
final <- final %>%
  mutate(across(c(state, zip_code, naics_title_2digits, soc_description,
                  nature_title_pred, part_title_pred, event_title_pred,
                  source_title_pred, GEOID), as.factor))
data1 = final %>%
  filter (year_filing_for == 2023)
data2 = final %>%
  filter (year_filing_for == 2024)
saveRDS(data1, "/Users/aliajamil/Desktop/r/work/dashboard_github/US_dashboard/ITA_FIPS_23.rds", compress = "xz")
saveRDS(data2, "/Users/aliajamil/Desktop/r/work/dashboard_github/US_dashboard/ITA_FIPS_24.rds", compress = "xz")


#remove territories without shapefiles
a = unique(counties_sf$STUSPS)
b = unique(data$state)
c = setdiff(b, a)
#filter out VI, GU, MP, AS
c = c("VI", "GU", "MP", "AS")
final = final %>%
  filter(!state %in% c)



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
  data2 = readRDS("ITA_FIPS.rds")

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

