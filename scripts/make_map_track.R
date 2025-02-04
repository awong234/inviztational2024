library(RSQLite)
library(sf)
library(spData)
library(DBI)
library(dplyr)

con = dbConnect(RSQLite::SQLite(), 'data/data.db')

output_folder = 'img/map_animation/'
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# -----------

world = spData::world
sf_use_s2(FALSE)
atlantic = world %>% st_crop(xmin = -160, xmax = 0, ymin = 0, ymax = 80)
sf_use_s2(TRUE)


path = dbGetQuery(con,
'
select p.*, n.hurricane_name, n.hurricane_year from paths_interp p
join names n on
p.hurricane_id = n.hurricane_id
where p.hurricane_id in (select ID from chosen_storms)
')

colors = yaml::read_yaml('colors.yml')

{
    path$hurricane_initial = substr(path$hurricane_name, 1,1)
    path$point_color = path$line_color = colors[path$hurricane_id] |> do.call(what = c, args = _)
    storms = path %>% split(path$hurricane_id)
    storms = lapply(storms, \(x) {
        lighten_amount = sin(1:nrow(x))/2
        x$point_color = colorspace::lighten(x$point_color, lighten_amount)
        x
    })
}

max_length = sapply(storms, nrow) |> max()


for (frame in seq(1, max_length)) {
    cat(frame, "\r")
    png(filename = sprintf("%s/frame_%05d.png", output_folder, frame), width = 300*1.5, height = 200*1.5)
    par(bg='#00baff', mar=c(0,0,0,0), family="Cascadia Code")
    plot(st_geometry(atlantic), bg=NA, col=NA, border='white')
    lapply(storms, \(x) {
        x = x %>% select(longitude, latitude, line_color, point_color, hurricane_initial) %>% slice_head(n = frame)
        lines(x$longitude, x$latitude, col = x$line_color)
        x_final = tail(x, 1)
        points(x_final$longitude, x_final$latitude, col = x_final$point_color, cex=2, lwd = 3)
        text(x_final$longitude, x_final$latitude, col = x_final$line_color, labels = x_final$hurricane_initial, cex=1, adj=c(-1,1))
    })
    dev.off()
}

