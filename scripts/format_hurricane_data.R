library(readxl)
library(dplyr)
library(RSQLite)
library(DBI)
library(tidyr)
library(sf)

source('functions.R')

con = dbConnect(RSQLite::SQLite(), 'data/data.db')

# Clean up data ---------

hurr = readxl::read_excel('data/Copy of Atlantic & Pacific Hurricane Data.xlsx')
hurr = janitor::clean_names(hurr)
hurr = hurr %>% mutate( datetime = as.POSIXct(data_date_time, format = '%m/%d/%Y %H:%M') )
hurr = hurr %>% mutate(datetime = as.POSIXct(datetime, format = '%FT%H:%M:%SZ'))
hurr = hurr %>% arrange(hurricane_id, datetime)
hurr = hurr %>% mutate(across(where(is.numeric), ~case_when(.x == -999 ~ NA, .default = .x)))

# Save output in db -----------

chosen_storms = c(
    "DORIAN2019" = 'AL052019',
    "IAN2022" = 'AL092022',
    "IRMA2017" = 'AL112017',
    "MARIA2017" = 'AL152017',
    "OLGA2019" = 'AL172019'
)
chosen_storms = tibble::enframe(chosen_storms) |> rename(NAME = name, ID = value)

# Normalize products
paths = hurr %>% select(hurricane_id, datetime, latitude, longitude)
paths = paths %>% split(paths$hurricane_id)
paths = lapply(paths, add_motion_dynamics)
paths = bind_rows(paths)

hurricane_names = hurr %>% distinct(hurricane_id, hurricane_name, hurricane_year)

radii = hurr %>% select(hurricane_id, datetime, contains('kt_wind'), radius_of_maximum_wind_in_nautical_miles)

radii_averages = radii %>%
    select(-radius_of_maximum_wind_in_nautical_miles) %>%
    pivot_longer(cols = c(contains("kt_wind"))) %>%
    mutate(wind_speed_kt = stringr::str_sub(name, 2, 3)) %>%
    group_by(hurricane_id, datetime, wind_speed_kt) %>%
    summarize(average_radius_nt_mi = mean(value, na.rm = TRUE), .groups = 'drop')

radii_summary = radii %>% pivot_longer(cols = c(contains("kt_wind"), "radius_of_maximum_wind_in_nautical_miles")) %>%
    group_by(hurricane_id, datetime) %>%
    summarize(maximal_radius_nt_mi = max(value, na.rm = TRUE), .groups = 'drop')

properties = hurr %>% select(hurricane_id, datetime, maximum_sustained_wind_in_knots, minimum_pressure_in_millibars, status_of_system, status_of_system_detail, record_identifier, record_identifier_detail, basin)

date_dim = hurr %>% distinct(datetime, data_day, data_year, data_month, data_hour_utc, data_minute) %>% mutate(yday = lubridate::yday(datetime))

chosen_storm_paths = paths %>% filter(hurricane_id %in% chosen_storms$ID)
chosen_storm_paths_interp = interpolate_paths(chosen_storm_paths, 5/60)
chosen_storm_paths_interp_pts = st_as_sf(chosen_storm_paths_interp, coords = c('longitude', 'latitude'))
world = spData::world
chosen_storm_paths_interp_pts = st_set_crs(chosen_storm_paths_interp_pts, st_crs(world))
land_water = !is.na(as.numeric(st_intersects(chosen_storm_paths_interp_pts, world)))
chosen_storm_paths_interp$over_land = land_water
chosen_storm_paths_interp = chosen_storm_paths_interp %>% relocate(hurricane_id, .after = datetime)

paths %>%
    filter(hurricane_id %in% chosen_storms$ID) %>%
    group_by(hurricane_id) %>%
    summarize(total_distance = sum(distance_traveled_miles, na.rm = TRUE))

chosen_storm_paths_interp %>%
    group_by(hurricane_id) %>%
    summarize(total_dist = max(cumulative_dist_mi)) %>%
    data.frame()

winners = chosen_storm_paths_interp %>%
    filter(cumulative_dist_mi <= 4000) %>%
    mutate(frame = row_number()) %>%
    group_by(hurricane_id) %>%
    filter(cumulative_hours == max(cumulative_hours)) %>%
    left_join(hurricane_names, by = 'hurricane_id') %>%
    arrange(desc(cumulative_hours)) %>%
    data.frame()

# Write to database
dbWriteTable(con, 'chosen_storms', chosen_storms, overwrite = TRUE)
dbWriteTable(con, 'paths', paths, overwrite = TRUE)
dbWriteTable(con, 'names', hurricane_names, overwrite = TRUE)
dbWriteTable(con, 'radii', hurricane_names, overwrite = TRUE)
dbWriteTable(con, 'radii_averages', radii_averages, overwrite = TRUE)
dbWriteTable(con, 'radii_maxima', radii_summary, overwrite = TRUE)
dbWriteTable(con, 'properties', properties, overwrite = TRUE)
dbWriteTable(con, 'date_dim', date_dim, overwrite = TRUE)
dbWriteTable(con, 'paths_interp', chosen_storm_paths_interp, overwrite = TRUE)

# Summary statistics ---------------

modern_hurr = paths %>% left_join(hurricane_names, by = 'hurricane_id') %>% filter(hurricane_year >= 2000)

# Fastest storms by average speed
modern_hurr %>%
    group_by(hurricane_id, hurricane_name, hurricane_year) %>%
    summarize(average_speed_mph = weighted.mean(average_speed_mph, delta_t_hours, na.rm=TRUE)) %>%
    arrange(desc(average_speed_mph))

# Overall average speed
modern_hurr %>% summarize(average_speed_mph = weighted.mean(average_speed_mph, delta_t_hours, na.rm=TRUE))

# Average distance traveled
modern_hurr %>%
    group_by(hurricane_id, hurricane_name, hurricane_year) %>%
    summarize(
        distance_traveled_miles = sum(distance_traveled_miles, na.rm = TRUE),
        total_lifetime = sum(delta_t_hours, na.rm = TRUE),
        .groups = 'drop') %>%
    summarize(
        distance_traveled_miles = mean(distance_traveled_miles, na.rm = TRUE),
        total_lifetime = mean(total_lifetime, na.rm = TRUE),
    )
