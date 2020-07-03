# ABOUT ----
# Cluster modeling of County Population & Demographics data

# Start-up -----
setwd(here::here())
rm(list = ls())

# Clear non-otb packages loaded in env
suppressWarnings(invisible(lapply(
  paste0('package:', names(sessionInfo()$otherPkgs)),
  detach,
  character.only = TRUE,
  unload = TRUE
)))

# Load Packages
require(tidyverse)
require(magrittr)

# Load Data
pop = readr::read_csv(file = "2018_txpopest_county.csv", col_names = TRUE, trim_ws = TRUE) 
# ... Data sourced from: "https://demographics.texas.gov/Data/TPEPP/Estimates/"
# ... Direct download link: "https://demographics.texas.gov/Resources/TPEPP/Estimates/2018/2018_txpopest_county.csv"

# Simple Pre-processing 
pop = pop %>% 
  select(FIPS, county, jan1_2019_pop_est) %>%
  rename(County = county) %>% 
  filter(County != "State of Texas")


# Standardize [County] names to align with other data-sources
pop = pop %>% 
  mutate(County = stringr::str_to_title(County, locale = "en")) %>% 
  mutate(County = ifelse(County == "De Witt", "Dewitt", County))

# Scale population values
pop = pop %>% 
  mutate(value = log(jan1_2019_pop_est))

# Calc mean, sd
pop_bounds = pop %$% c(mean(value), sd(value)) 

# # Segment into (3)-groups
# pop_bounds =  c("Low" = pop_bounds[1] - (1.5 * pop_bounds[2]),
#                 "High" = pop_bounds[1] + (1.5 * pop_bounds[2]))
# 
# pop = pop %>%
#   mutate(pop_group = ifelse(
#     value < pop_bounds["Low"],
#     yes = 1,
#     ifelse(value > pop_bounds["High"], yes = 3, no = 2)
#   ))

# Segment into (4)-groups 
# ..High, >= m+2sd ... 4
# .. m+1sd < Mod-High < m+2sd ... 3
# .. m-1sd < Moderate < m+1sd ... 2
# .. Low < m - 1d ... 1


# Calc bounds
pop_bounds = c(pop_bounds[1] - pop_bounds[2],
               pop_bounds[1] + pop_bounds[2],
               pop_bounds[1] + (2*pop_bounds[2])
)
# Map Groups
pop = pop %>%
  mutate(pop_group = ifelse(
    test = (value <= pop_bounds[1]),
    yes = 1,
    no = ifelse(
      test = (value > pop_bounds[1] & value < pop_bounds[2]),
      yes = 2,
      no = ifelse(
        test = (value > pop_bounds[2] & value < pop_bounds[3]),
        yes = 3,
        no = 4
      )
    )
  ))

pop = pop %>% 
  mutate(pop_group = factor(pop_group)) %>%
  mutate(pop_group = factor(pop_group, levels = c("1", "2", "3", "4"), 
                            labels = c("Rural", "Goldilocks", "Suburban", "Urban"),
                            ordered = TRUE)) %>%
  mutate(pop_group = forcats::fct_rev(pop_group)) 
  
  
p = pop %>%
  ggplot(data = ., mapping = aes(x = jan1_2019_pop_est, fill = pop_group)) +
  geom_histogram(mapping = aes(y = ..count../sum(..count..)), bins = 30) + 
  scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 1, scale = 1/1e3, suffix = "K", big.mark = ",")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggthemes::theme_fivethirtyeight() +
  xlab("Jan 2019 Population Estimate") + ylab("Pcnt. of Counties") +
  labs(fill = "") +
  ggtitle(label = "Texas Communities", subtitle = "Population Segmentation, Log-scale") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.title = element_text(),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")
        )

plotly::ggplotly(p) # preview objects

# Save plot
saveRDS(object = p, file = "shiny-server_files/plot_Texas-Communities_Population-Segmentation.RDS")


# Map [pop] ----
texas_geo = urbnmapr::counties %>% filter(state_name == "Texas")
texas_geo = texas_geo %>% 
  # Create Stanardized "County" field that allows easy matching 
  # ... to population, demographic and covid data
  # ... FIPS in population & demographis data appears to be invalid,
  # ... ... therefore join over corresponding ubrnmapr::counties field will not work
  mutate(County = stringr::str_remove(county_name, "\\s*County"),
         County = stringr::str_to_title(County)) 

pop_maps = pop %>% 
  left_join(x = ., 
            y = texas_geo, 
            by = "County")

lat_lims = c(min(pop_maps$lat, na.rm = TRUE), max(pop_maps$lat, na.rm = TRUE))

p = pop_maps %>% 
  ggplot(data = ., mapping = aes(x = long, y = lat, 
                                 group = group, fill = pop_group,
                                 text = sprintf(paste("County:", County)))) +
  geom_polygon(color = "black", size = 0.2) + 
  coord_map(projection = "albers", lat0 = lat_lims[1], lat1 = lat_lims[2]) +
  xlab("") + ylab("") +
  labs(fill = "County-type") +
  ggtitle(label = "Texas County Classification", subtitle = "Community-types") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")
  )

plotly::ggplotly(p) # preview objects

# Save plot
saveRDS(object = p, file = "shiny-server_files/plot_Texas-Communities_Population-Segmentation-Map.RDS")

dev.off()

# Export Key Objects ----
saveRDS(object = texas_geo, "shiny-server_files/texas-shapefile_county-level.RDS")
saveRDS(object = pop, "shiny-server_files/texas-demographics_county-populations_segmented.RDS")

