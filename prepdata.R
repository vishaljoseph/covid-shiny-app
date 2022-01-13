library(tidyverse)
library(vroom)
library(sf)
library(tigris)
library(leaflet)
library(htmlwidgets)

setwd("~/Desktop/CPR/sample")

# download.file(url = "https://github.com/nychealth/coronavirus-data/archive/master.zip", 
#               destfile = "coronavirus-data-master.zip")
# 
# unzip(zipfile = "coronavirus-data-master.zip")

cases <- vroom("coronavirus-data-master/trends/caserate-by-modzcta.csv")
pc_positive <- vroom("coronavirus-data-master/trends/percentpositive-by-modzcta.csv")
tests <- vroom("coronavirus-data-master/trends/testrate-by-modzcta.csv")
geo_data <- st_read("coronavirus-data-master/Geography-resources/MODZCTA_2010.shp")  #modzcta shp file

# preparing datasets for zip code analysis
cases_zip <- cases %>%
  select(-c(2:7)) %>%
  pivot_longer(2:178,
               names_to = "modzcta",
               names_prefix = "CASERATE_",
               values_to = "case_rate"
  )
pc_positive_zip <- pc_positive %>%
  select(-c(2:7)) %>% 
  pivot_longer(2:178,
               names_to = "modzcta",
               names_prefix = "PCTPOS_",
               values_to = "pc_positive"
  )
tests_zip <- tests %>%
  select(-c(2:7)) %>% 
  pivot_longer(2:178,
               names_to = "modzcta",
               names_prefix = "TESTRATE_",
               values_to = "test_rate"
  )

# merging all three into one dataset
all <- cases_zip %>% 
  left_join(pc_positive_zip, by = c("week_ending", "modzcta")) %>% 
  left_join(tests_zip, by = c("week_ending", "modzcta"))

# merging 'all' dataset with the geo data
all_geo <- geo_join(geo_data, all,
                    'MODZCTA', 'modzcta',
                    how = "inner")

#converting week_ending column to datetype
all_geo$week_ending <- as.Date(all_geo$week_ending, format = "%m/%d/%Y")

#saving all_geo df as a .RDS file
saveRDS(all_geo, "nyc_covid/all_geo.RDS")


# preparing datasets for borough analysis
cases_bor <- cases %>%
  select(c(1, 3:7)) %>%
  pivot_longer(2:6,
               names_to = "bor",
               names_prefix = "CASERATE_",
               values_to = "case_rate"
  )

pc_positive_bor <- pc_positive %>%
  select(c(1, 3:7)) %>%
  pivot_longer(2:6,
               names_to = "bor",
               names_prefix = "PCTPOS_",
               values_to = "pc_positive"
  )

tests_bor <- tests %>%
  select(c(1, 3:7)) %>%
  pivot_longer(2:6,
               names_to = "bor",
               names_prefix = "TESTRATE_",
               values_to = "test_rate"
  )

# merging all three into one dataset
all_bor <- cases_bor %>% 
  left_join(pc_positive_bor, by = c("week_ending", "bor")) %>% 
  left_join(tests_bor, by = c("week_ending", "bor"))

#converting week_ending column to datetype
all_bor$week_ending <- as.Date(all_bor$week_ending, format = "%m/%d/%Y")

#saving all_geo df as a .RDS file
saveRDS(all_bor, "nyc_covid/all_bor.RDS")


# preparing datasets for trend analysis
trend_df <- all %>% 
  group_by(week_ending) %>% 
  summarize(case_rate = mean(case_rate),
            test_rate = mean(test_rate),
            pc_positive = mean(pc_positive))

#converting week_ending column to datetype
trend_df$week_ending <- as.Date(trend_df$week_ending, format = "%m/%d/%Y")

#saving trend_df as a .RDS file
saveRDS(trend_df, "nyc_covid/trend_df.RDS")



#making sample interactive map using leaflet

# m <- leaflet() %>%
#   addTiles() %>%  # Add default OpenStreetMap map tiles
#   addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
# m  # Print the map

labels <- sprintf(
  "<strong>%s</strong><br/>%g cases per 100,000 people",
  all_geo$MODZCTA, all_geo$case_rate) %>% 
  lapply(htmltools::HTML)

pal <- colorBin(palette = "OrRd", 9, domain = all_geo$case_rate)

sample_map <- all_geo %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet() %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(label = labels,
              stroke = FALSE,
              smoothFactor = 0.5,
              opacity = 1,
              fillOpacity = 0.7,
              fillColor = ~ pal(case_rate),
              highlightOptions = highlightOptions(weight = 5,
                                                  fillOpacity = 1,
                                                  color = "black",
                                                  opacity = 1,
                                                  bringToFront = TRUE)) %>% 
  addLegend("bottomright",
            pal = pal,
            values = ~ case_rate,
            title = "Cases Per 100,000",
            opacity = 0.7)
  
sample_map

saveWidget(sample_map, "nyc_covid/nyc_covid_pc_pos_map.html")




