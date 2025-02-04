library(RSQLite)
library(sf)
library(spData)
library(DBI)
library(dplyr)
library(mgcv)
library(ggplot2)

con = dbConnect(RSQLite::SQLite(), 'data/data.db')

output_folder = 'img/analysis/all_storm_map'

# Plot out storm paths ----------------------

path = dbGetQuery(con,
                  '
select p.*, n.hurricane_name, n.hurricane_year from paths p
join names n on
p.hurricane_id = n.hurricane_id
where hurricane_year > 2000
')

cols = viridisLite::viridis(n = 100, alpha = 0.9)
path = path %>% filter(!is.na(average_speed_mph), longitude > -160, longitude < 0)
path$average_speed_mph_cut = cut(path$average_speed_mph, 100, include.lowest = TRUE)
path$color = cols[path$average_speed_mph_cut]
storm_path = path %>% split(path$hurricane_id)
max_length = max(sapply(storm_path, nrow))

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
for (frame in seq(1, max_length, by = 1)) {
    cat(frame, "\r")
    png(filename = sprintf("%s/frame_%05d.png", output_folder, frame), width = 1920, height = 1080)
    par(bg='black', mar=c(0,0,0,0), family="Cascadia Code")
    plot(st_geometry(atlantic), bg=NA, col=NA, border='white')
    lapply(storm_path, \(x) {
        x = x %>% select(longitude, latitude, color) %>% slice_head(n = frame)
        lines(x$longitude, x$latitude, col = x$color)
    })
    dev.off()
}
