#==============================================================================
#
# 09_stage1_maps.r -- map the ideal points estimated at Stage 1
#
# This script maps users by their estimated ideal point, and correlates the
# mean ideal point per departement with local and presidential elections.
#
#==============================================================================

library(ggplot2)
library(gridExtra)

library(dplyr)
library(readr)
library(stringr)

library(rgdal)
library(rgeos)
library(maptools)

load("model/stage1_results.rda")

# map theme
theme_mapped = theme_bw(14) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = rel(1)),
        strip.background = element_rect(fill = "grey90"),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(1)),
        legend.position = "bottom")

#==============================================================================
# MAP OF FOLLOWERS IDEAL POINTS
#==============================================================================

# saved earlier
g = read_csv("data/cities.csv")

# the only cases that we miss are DOM-TOM and 'Saint-' homonyms:
# Saint-Denis (93), Saint-Louis (69), Saint-André (59), etc.
filter(est, !is.na(ville) & !ville %in% g$ville) %>%
  select(ville, departement, region) %>%
  unique %>%
  data.frame

# city names are okay; let's also convert the names of the departements
g$departement = gsub("[[:punct:]]|[0-9]", " ", g$departement)       # unwanted symbols
g$departement = iconv(g$departement, to = "ASCII//TRANSLIT")        # accents to punctuation
g$departement = gsub("[[:punct:]]", "", g$departement)              # accents out
g$departement = tolower(str_trim(gsub("\\s+", " ", g$departement))) # spaces out

# only missing DOM-TOM
depts = na.omit(est$departement)
table(depts[ !depts %in% tolower(g$departement)])

# create a vector of departement numbers
depts = g$numero

# and name it after the city
names(depts) = g$ville

# now assign a departement number to users
est$departement_id = depts[ est$ville ]

# that step identifies users with cities
table(!is.na(est$departement_id))

# let's now use departement names
names(depts) = g$departement

# and use only unique values
depts = depts[ !duplicated(depts) ]

# now assign a departement number to those users who do not yet have one
m = is.na(est$departement_id)
est$departement_id[ m ] = depts[ est$departement[ m ] ]

# note: some users do not have the same departement number as their city
# indicates, e.g. users with dual locations "Paris/Lyon"; should be fine

# correct the departement numbers to match the map data
m = nchar(est$departement_id) < 2
est$departement_id[ m ] = paste0("0", est$departement_id[ m ])

# mean thetas in each departement
depts = group_by(est, departement_id) %>%
  summarise(n = n(), mu = mean(phat)) %>%
  filter(n > 10)

# IGN shapefiles: http://professionnels.ign.fr/geofla
depmap = readOGR(dsn = "GEOFLA_2-0_DEPARTEMENT_SHP_LAMB93_FXX_2014-12-05/GEOFLA/1_DONNEES_LIVRAISON_2014-12-00068/GEOFLA_2-0_SHP_LAMB93_FR-ED141/DEPARTEMENT", layer = "DEPARTEMENT")
depggm = fortify(depmap, region = "CODE_DEPT")

# add mean thetas
depggm = left_join(depggm, rename(depts, id = departement_id), by = "id")

# add panel title
depggm$panel = "Mean Twitter-based ideal points"

# mean ideal point by departement, colored from red (left) to blue (right)
g1 = ggplot(depggm, aes(map_id = id)) +
  geom_map(aes(fill = mu), map = depggm, color = "white", size = 1) +
  # geom_text(data = group_by(depggm, id) %>%
  #             summarise(long = mean(long), lat = mean(lat), n = unique(n)),
  #           aes(x = long, y = lat, label = n), color = "white") +
  expand_limits(x = depggm$long,
                y = depggm$lat) +
  scale_fill_gradient2("", na.value = "grey80",
                       limits = max(range(depts$mu)) * c(-1, 1),
                       low = "#B40F20", mid = "white",
                       midpoint = mean(depts$mu),
                       high = "#046C9A") +
  guides(fill = guide_colorbar(barwidth = 12, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  facet_grid(. ~ panel) +
  theme_mapped +
  labs(y = NULL, x = NULL)

#==============================================================================
# MAP OF IDEAL POINTS VERSUS SARKOZY VOTE SHARE
#==============================================================================

# round 2
elec = read_csv("data/elec_pres2012_2.csv", col_types = list(id = col_character()))

# join and correlate
elec = left_join(elec, rename(depts, id = departement_id), by = "id")

# invert because left-wing ideal points are negative
with(elec, cor(mu, -p_hollande, use = "complete.obs"))

# and of course the opposite yields the same result
with(elec, cor(mu, p_sarkozy, use = "complete.obs"))

# add mean thetas
depggm = left_join(depggm, select(elec, id, p_sarkozy), by = "id")

# add panel title
depggm$panel = "Sarkozy vote share, 2012 round 2"

# map of vote share
g2 = ggplot(depggm, aes(map_id = id)) +
  geom_map(aes(fill = p_sarkozy), map = depggm, color = "white", size = 1) +
  expand_limits(x = depggm$long,
                y = depggm$lat) +
  scale_fill_gradient2("", na.value = "grey80",
                       limits = range(elec$p_sarkozy),
                       low = "#B40F20", mid = "white",
                       midpoint = mean(elec$p_sarkozy),
                       high = "#046C9A") +
  guides(fill = guide_colorbar(barwidth = 12, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  facet_grid(. ~ panel) +
  theme_mapped +
  labs(y = NULL, x = NULL)

png("plots/map_vote.png", width = 10, height = 6, units = "in", res = 300)
grid.arrange(g1, g2, ncol = 2)
dev.off()

# correlation to round 1
elec = read_csv("data/elec_pres2012_1.csv", col_types = list(id = col_character()))

elec = group_by(elec, id) %>%
  summarise(p_left = sum(vote[ side == "Left" ], na.rm = TRUE),
            p_right = sum(vote[ side == "Right" ], na.rm = TRUE))

elec = inner_join(elec, rename(depts, id = departement_id), by = "id")

with(elec, cor(mu, p_left))
with(elec, cor(mu, p_right))

#==============================================================================
# CORRELATION TO LOCAL ELECTION VOTE SHARE
#==============================================================================

# 2015 round 1
elec = read_csv("data/elec_dept2015_1.csv", col_types = list(id = col_character()))

elec = group_by(elec, id) %>%
  summarise(p_left = sum(vote[ side == "Left" ], na.rm = TRUE),
            p_right = sum(vote[ side == "Right" ], na.rm = TRUE))

elec = inner_join(elec, rename(depts, id = departement_id), by = "id")

with(elec, cor(mu, p_left))
with(elec, cor(mu, p_right))

# 2015 round 2
elec = read_csv("data/elec_dept2015_2.csv", col_types = list(id = col_character()))

elec = group_by(elec, id) %>%
  summarise(p_left = sum(vote[ side == "Left" ], na.rm = TRUE),
            p_right = sum(vote[ side == "Right" ], na.rm = TRUE))

elec = inner_join(elec, rename(depts, id = departement_id), by = "id")

with(elec, cor(mu, p_left))
with(elec, cor(mu, p_right))

# 2011 round 2
elec = read_csv("data/elec_dept2011_2.csv", col_types = list(id = col_character()))

elec = group_by(elec, id) %>%
  summarise(p_left = sum(vote[ side == "Left" ], na.rm = TRUE),
            p_right = sum(vote[ side == "Right" ], na.rm = TRUE))

elec = inner_join(elec, rename(depts, id = departement_id), by = "id")

with(elec, cor(mu, p_left))
with(elec, cor(mu, p_right))

# 2011 round 1
elec = read_csv("data/elec_dept2011_1.csv", col_types = list(id = col_character()))

elec = group_by(elec, id) %>%
  summarise(p_left = sum(vote[ side == "Left" ], na.rm = TRUE),
            p_right = sum(vote[ side == "Right" ], na.rm = TRUE))

elec = inner_join(elec, rename(depts, id = departement_id), by = "id")

with(elec, cor(mu, p_left))
with(elec, cor(mu, p_right))
