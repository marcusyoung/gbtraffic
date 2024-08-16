library(dplyr)
library(tidyverse)
library(RPostgres)
library(sf)
library(RCurl)
library(rjson)
library(jsonlite)



las <- c(1:214)
gb_countpoints <- data.frame()
gb_traffic <- data.frame()
aadf_by_direction <- data.frame()
raw_counts <- data.frame()


# countpoints

# need to insert delay to prevent request being blocked

for (i in las) {
  print(paste0("getting ", i))
  Sys.sleep(0.5)

countpoint_url <- paste0("https://storage.googleapis.com/dft-statistics/road-traffic/downloads/countpoints/local_authority_id/dft_countpoints_local_authority_id_", i, ".csv")
traffic_url <- paste0("https://storage.googleapis.com/dft-statistics/road-traffic/downloads/traffic/local_authority_id/dft_traffic_local_authority_id_", i, ".csv")
aadf_by_direction_url <- paste0("https://storage.googleapis.com/dft-statistics/road-traffic/downloads/aadfbydirection/local_authority_id/dft_aadfbydirection_local_authority_id_", i, ".csv")
raw_counts_url <- paste0("https://storage.googleapis.com/dft-statistics/road-traffic/downloads/rawcount/local_authority_id/dft_rawcount_local_authority_id_", i, ".csv")

skip_to_next <- FALSE

tryCatch( {
countpoints <-
  read_csv(countpoint_url, col_types = "iicicccccccccnn", col_select = c(count_point_id, region_id, region_name, local_authority_id, local_authority_name, easting, northing, latitude, longitude, road_type, road_name, start_junction_road_name, end_junction_road_name, link_length_km, link_length_miles))

traffic <- read_csv(traffic_url, col_types = "icinnnn", col_select = c(local_authority_id, local_authority_name, year, link_length_km, link_length_miles, cars_and_taxis, all_motor_vehicles))

aadf <- read_csv(aadf_by_direction_url, col_types = "iiiciccccciinnnnccciiiiiiiiiiiii", col_select = c(count_point_id, year, region_id, local_authority_id, direction_of_travel, estimation_method, estimation_method_detailed, pedal_cycles, two_wheeled_motor_vehicles, cars_and_taxis, buses_and_coaches, lgvs, hgvs_2_rigid_axle, hgvs_3_rigid_axle, hgvs_4_or_more_rigid_axle, hgvs_3_or_4_articulated_axle, hgvs_5_articulated_axle, hgvs_6_articulated_axle, all_hgvs, all_motor_vehicles))

raw <- read_csv(raw_counts_url, col_types = "iciciicicccccccccnniiiiiiiiiiiii", col_select = c(count_point_id, year, count_date, direction_of_travel, hour, pedal_cycles, two_wheeled_motor_vehicles, cars_and_taxis, buses_and_coaches, lgvs, hgvs_2_rigid_axle, hgvs_3_rigid_axle, hgvs_4_or_more_rigid_axle, hgvs_3_or_4_articulated_axle, hgvs_5_articulated_axle, hgvs_6_articulated_axle, all_hgvs, all_motor_vehicles))

}, error = function(e) { skip_to_next <<- TRUE})

if(skip_to_next) { next }

gb_countpoints <- rbind(gb_countpoints, countpoints)
gb_traffic <- rbind(gb_traffic, traffic)
aadf_by_direction <- rbind(aadf_by_direction, aadf)
raw_counts <- rbind(raw_counts, raw)

}

## Manual - problem with Enfield 2024

enfield_countpoint_url <- paste0("https://storage.googleapis.com/dft-statistics/road-traffic/downloads/countpoints/local_authority_id/dft_countpoints_local_authority_id_", 121, ".csv")

enfield_aadf_by_direction_url <- paste0("https://storage.googleapis.com/dft-statistics/road-traffic/downloads/aadfbydirection/local_authority_id/dft_aadfbydirection_local_authority_id_", 121, ".csv")

enfield_raw_counts_url <- paste0("https://storage.googleapis.com/dft-statistics/road-traffic/downloads/rawcount/local_authority_id/dft_rawcount_local_authority_id_", 121, ".csv")

enfield_countpoints <-
  read_csv(enfield_countpoint_url, col_types = "iicicccccccccnn", col_select = c(count_point_id, region_id, region_name, local_authority_id, local_authority_name, easting, northing, latitude, longitude, road_type, road_name, start_junction_road_name, end_junction_road_name, link_length_km, link_length_miles))

enfield_aadf <- read_csv(enfield_aadf_by_direction_url, col_types = "iiiciccccciinnnnccciiiiiiiiiiiii", col_select = c(count_point_id, year, region_id, local_authority_id, direction_of_travel, estimation_method, estimation_method_detailed, pedal_cycles, two_wheeled_motor_vehicles, cars_and_taxis, buses_and_coaches, lgvs, hgvs_2_rigid_axle, hgvs_3_rigid_axle, hgvs_4_or_more_rigid_axle, hgvs_3_or_4_articulated_axle, hgvs_5_articulated_axle, hgvs_6_articulated_axle, all_hgvs, all_motor_vehicles))

enfield_raw <- read_csv(enfield_raw_counts_url, col_types = "iciciicicccccccccnniiiiiiiiiiiii", col_select = c(count_point_id, year, count_date, direction_of_travel, hour, pedal_cycles, two_wheeled_motor_vehicles, cars_and_taxis, buses_and_coaches, lgvs, hgvs_2_rigid_axle, hgvs_3_rigid_axle, hgvs_4_or_more_rigid_axle, hgvs_3_or_4_articulated_axle, hgvs_5_articulated_axle, hgvs_6_articulated_axle, all_hgvs, all_motor_vehicles))


gb_countpoints <- rbind(gb_countpoints, enfield_countpoints)
aadf_by_direction <- rbind(aadf_by_direction, enfield_aadf)
raw_counts <- rbind(raw_counts, enfield_raw)

enfield_json_file <- "https://roadtraffic.dft.gov.uk/api/traffic/local-authorities?filter[local_authority_id]=121"
enfield_json_data <- jsonlite::fromJSON(enfield_json_file, flatten = TRUE)
enfield_traffic <- enfield_json_data$data
enfield_traffic$region_id <- NULL
enfield_traffic$id <- NULL
enfield_traffic$ons_code <- NULL

gb_traffic <- rbind(gb_traffic, enfield_traffic)

# set roadtype to lower case
gb_countpoints$road_type <- tolower(gb_countpoints$road_type)

gb_countpoints %>% arrange(count_point_id) %>%
  write.csv(., file = "dev/dft_csv/countpoints.csv", quote = TRUE, row.names = FALSE, na = "")

# traffic

gb_traffic %>% arrange(local_authority_id, year) %>%
  write.csv(., file = "dev/dft_csv/gb_traffic.csv", quote = TRUE, row.names = FALSE, na = "")

# aadf_by_direction

aadf_by_direction %>% arrange(local_authority_id, year, count_point_id) %>%
  write.csv(., file = "dev/dft_csv/gb_aadf_dir_import.csv", quote = TRUE, row.names = FALSE, na = "")


# raw_counts

raw_counts %>% arrange(count_point_id, year, count_date, hour) %>%
  write.csv(., file = "dev/dft_csv/gb_raw_counts_import.csv", quote = TRUE, row.names = FALSE, na = "")


# major road network

pg_host <- Sys.getenv('SFTZ_PG_HOST')
pg_port <- Sys.getenv('SFTZ_PG_PORT')
pg_db <- Sys.getenv('SFTZ_PG_DB')
pg_user <- Sys.getenv('SFTZ_PGMAIN_USER')
pg_pwd <- Sys.getenv('SFTZ_PGMAIN_PASSWORD')

con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = pg_db,
  host = pg_host,
  port = pg_port,
  user = pg_user,
  password = pg_pwd
)


years <- (2018:2023)

major_roads <- data.frame()

temp <- tempfile()
temp2 <- tempfile()

for (yr in years) {
  url <- paste0("https://storage.googleapis.com/dft-statistics/road-traffic/mrdb-", yr, ".zip")
  download.file(url, temp)
  unzip(temp, exdir = temp2)
  table <- paste0("major_road_links_", yr)
  st_write(read_sf(temp2), con, layer = RPostgres::Id(schema = "traffic_gb", table = table))
  temp <- tempfile()
  temp2 <- tempfile()
  }

