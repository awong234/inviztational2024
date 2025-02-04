library(RSQLite)
library(sf)
library(spData)
library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(patchwork)

con = dbConnect(RSQLite::SQLite(), 'data/data.db')

colors = yaml::read_yaml('colors.yml')

path = dbGetQuery(con,
                  '
select
    p.*,
    n.hurricane_name,
    n.hurricane_year,
    pr.maximum_sustained_wind_in_knots
from paths_interp p
join names n on
    p.hurricane_id = n.hurricane_id
left join properties pr on
    pr.hurricane_id = p.hurricane_id
    and pr.datetime = p.datetime
where p.hurricane_id in (select ID from chosen_storms)
')

world = spData::world
path_sf = st_as_sf(path, coords = c('longitude', 'latitude'))
st_crs(path_sf) = st_crs(world)
over_land = !is.na(as.numeric(st_intersects(path_sf, world)))
path$over_land = over_land

summary = path %>%
    group_by(hurricane_id, hurricane_name, hurricane_year) %>%
    summarize(
        `Avg. Forward Speed`   = weighted.mean(average_speed_mph, w = delta_t_hours),
        `Avg. Max Wind Speed`  = weighted.mean(maximum_sustained_wind_in_knots, w = delta_t_hours, na.rm = TRUE),
        `Total Distance Traveled` = sum(distance_traveled_miles),
        `Total Lifetime`       = sum(delta_t_hours),
        `Total Time Over Land` = sum(delta_t_hours[over_land]),
        .groups = 'drop'
    )

minmax_scale = function(x) {
    # 100 * ((x - min(x)) / (max(x) - min(x)))
    x / max(x)
}

summary_long = summary %>%
    mutate(across(c(`Avg. Forward Speed`,
                    `Avg. Max Wind Speed`,
                    `Total Distance Traveled`,
                    `Total Lifetime`,
                    `Total Time Over Land`), minmax_scale)) %>%
    pivot_longer(
        cols = c(`Avg. Forward Speed`,
        `Avg. Max Wind Speed`,
        `Total Distance Traveled`,
        `Total Lifetime`,
        `Total Time Over Land`)
    )

summary_long$name = factor(summary_long$name, levels = rev(c("Avg. Forward Speed",
                                                         "Avg. Max Wind Speed",
                                                         "Total Distance Traveled",
                                                         "Total Lifetime",
                                                         "Total Time Over Land")))


hurricane_damage = c(
    "AL052019"   = "$5.1 billion",
    "AL092022"   = "$113 billion",
    "AL112017"   = "$77.2 billion",
    "AL152017"   = "$91.6 billion",
    "AL172019"   = "$0.4 billion"
)

hurr_plot = function(df) {

    hurricane_id   = df$hurricane_id[2]
    hurricane_name = df$hurricane_name[1]
    hurricane_year = df$hurricane_year[1]
    hurricane_damage = hurricane_damage[hurricane_id]
    hurricane_color = colors[[hurricane_id]]
    hurricane_color_light = colorspace::lighten(hurricane_color, amount = 0.7)


    ggplot(df, aes(x = x_start, xend = x_end, y = name, yend = name, color = sequence)) +
        stat_identity(geom='segment', size=5, lineend='butt') +
        scale_color_binned(type = 'gradient', low = hurricane_color_light, high = hurricane_color) +
        labs(title = paste(hurricane_name, hurricane_year), subtitle = hurricane_damage) +
        xlim(c(0, 1.01))
}



windowsFonts("consolas" = windowsFont("Consolas"))

# Add segments
cuts = seq(0, 1, by = 0.2)
summary_long$value_cut = cut(summary_long$value, breaks = cuts)
new_df = summary_long[0, ]
for (i in seq(1, nrow(summary_long))) {
    times_rep = summary_long[i, 'value_cut'] |> as.integer()
    for (j in seq(1, times_rep)) {
        this_cut = cuts[j+1]
        addition = summary_long[i, ]
        addition$x_start = cuts[j] + 0.005
        addition$sequence = j
        if (j == times_rep) {
            last_bucket_width = cuts[j+1] - cuts[j]
            last_bucket_value = addition$value - cuts[j]
            proportion_remaining = 0.2 * (last_bucket_value / last_bucket_width) + (cuts[j] + 0.005)
            addition$x_end = proportion_remaining
        } else {
            addition$x_end = cuts[j+1] - 0.005
        }
        new_df = rbind(new_df, addition)
    }
}



# storm_summaries = summary_long %>% split(summary_long$hurricane_id)
storm_summaries = new_df %>% split(new_df$hurricane_id)

storm_plots = lapply(storm_summaries, hurr_plot)
storm_plots[[5]]
text = stringr::str_wrap("A race to the finish! First storm to travel 4000 miles wins!", width = 40)
storm_plots = c(storm_plots, list(grid::textGrob(text, gp = gpar(col = 'white', fontsize = 24))))

png(filename = 'img/analysis/storm-stats.png', width = 3000, height = 1000, res = 150, bg = 'transparent')
pl = wrap_plots(storm_plots,ncol=3) & theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_rect(colour = 'white',  linewidth = 1, fill = NA),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill  = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    text = element_text(size = 15, family = 'consolas', color = 'white'),
    axis.text = element_text(size = 15, family = 'consolas', color = 'white'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank()
)
print(pl)
dev.off()
