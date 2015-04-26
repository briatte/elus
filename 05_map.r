#==============================================================================
#
# 05_map.r
#
# Purpose: - geocode all available locations provided by the Twitter followers
#          - plot maps of the followers, including relative to total population
#          - correlate followers with age groups and voter registration rates
#
# Details: - geocodes from Google Maps
#          - list of French administrative units from Insee, 2012/2014
#          - population figures for age groups from Irdes and Insee, 2014
#          - voter registration figures from Conseil général de Gironde, 2013
#
#==============================================================================

# density curve map
library(ggmap)
library(ggplot2)

# choropleth map
library(rgdal)
library(maptools)

library(dplyr)
library(knitr)

library(readr)
library(stringr)

dir.create("plots", showWarnings = FALSE)

u = read_csv("data/users.csv", col_types = list(id = col_character()))

# comment out to plot maps out of all users instead of sampled ones
u = filter(u, sample)

#==============================================================================
# GEOCODE CITIES
#==============================================================================

if(!file.exists("data/geocodes.csv")) {

  geocodes = as.data.frame(table(u$ville), stringsAsFactors = FALSE)
  geocodes = cbind(geocodes, NA, NA, NA)
  names(geocodes) = c("ville", "users", "lon", "lat", "address")

  # note: 'baie mahault' needs to be added manually (Guadeloupe)
  for(i in geocodes$ville) {
    g = geocode(paste(i, "france"), "latlona")
    if(!is.na(g$lon)) {
      geocodes$lon[ geocodes$ville == i ] = g$lon
      geocodes$lat[ geocodes$ville == i ] = g$lat
      geocodes$address[ geocodes$ville == i ] = as.character(g$address)
    }
  }

  # manual geocodes for DOM-TOM locations
  geocodes$address[ geocodes$ville == "cayenne" ] = "cayenne, french guiana"
  geocodes$lon[ geocodes$ville == "cayenne" ] = -52.32690
  geocodes$lat[ geocodes$ville == "cayenne" ] = 4.92270
  geocodes$address[ geocodes$ville == "la possession" ] = "la possession, reunion"
  geocodes$lon[ geocodes$ville == "la possession" ] = 55.33570
  geocodes$lat[ geocodes$ville == "la possession" ] = -20.93299
  geocodes$address[ geocodes$ville == "le lamentin" ] = "le lamentin, martinique"
  geocodes$lon[ geocodes$ville == "le lamentin" ] = -61.00000
  geocodes$lat[ geocodes$ville == "le lamentin" ] = 14.60000
  geocodes$address[ geocodes$ville == "le marin" ] = "le marin, martinique"
  geocodes$lon[ geocodes$ville == "le marin" ] = -60.8658
  geocodes$lat[ geocodes$ville == "le marin" ] = 14.4694
  geocodes$address[ geocodes$ville == "saint andre" ] = "saint-andré, reunion"
  geocodes$lon[ geocodes$ville == "saint andre" ] = 55.64727
  geocodes$lat[ geocodes$ville == "saint andre" ] = -20.96373
  geocodes$address[ geocodes$ville == "saint martin" ] = "saint martin"
  geocodes$lon[ geocodes$ville == "saint martin" ] = -63.05225
  geocodes$lat[ geocodes$ville == "saint martin" ] = 18.08255
  geocodes$address[ geocodes$ville == "saint pierre" ] = "saint-pierre, reunion"
  geocodes$lon[ geocodes$ville == "saint pierre" ] = 55.47184
  geocodes$lat[ geocodes$ville == "saint pierre" ] = -21.33284

  write_csv(geocodes[ !is.na(geocodes$lon), ], "data/geocodes.csv")

}

geocodes = read_csv("data/geocodes.csv")
stopifnot(na.omit(u$ville) %in% geocodes$ville) # make sure all cities are found

# get users
n = as.data.frame(table(u$ville), stringsAsFactors = FALSE)
names(n) = c("ville", "users")

# update users column
geocodes$users = NULL
geocodes = left_join(geocodes, n, by = "ville") %>%
  filter(!is.na(lon), !is.na(lat), !is.na(users))

# order by descending users
geocodes = arrange(geocodes, -users) %>%
  select(ville, lon, lat, users, address)

# shown in map: metropolitan France, ~ 65,000 users
sum(subset(geocodes, grepl("france", address))$users)

# excluded: DOM-TOM
filter(geocodes, !grepl("france", address))

#==============================================================================
# MAP 1: DENSITY CURVES
#==============================================================================

fr = get_map(location = "france", zoom = 6, source = "stamen", maptype = "toner")

ggmap(fr, darken = c(.5, "white")) +
  geom_point(data = filter(geocodes, abs(lon) < 30),
             aes(y = lat, x = lon, size = log10(users + 1)),
             alpha = .75, color = "black") +
  geom_density2d(data = filter(geocodes, abs(lon) < 30),
                 aes(y = lat, x = lon, size = log10(users + 1)),
                 size = 1) +
  scale_size_area("Followers", max_size = 12,
                  breaks = 1:3, labels = 10^(1:3)) +
  labs(y = NULL, x = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom")

ggsave("plots/map1_cities.png", width = 12, height = 12)

#==============================================================================
# MAP 2: CHOROPLETH
#==============================================================================

depts = read_csv("data/geo_departements.csv")

# extract préfectures
g = data.frame(ville = depts$prefecture,
               departement = depts$departement,
               numero = depts$numero,
               stringsAsFactors = FALSE)

# extract sous-préfectures
for(i in 1:nrow(depts)) {

  y = unlist(strsplit(depts$sousprefs[ i ], ","))
  if(length(y) > 0)
    g = rbind(g, data.frame(ville = y,
                            departement = depts$departement[ i ],
                            numero = depts$numero[ i ],
                            stringsAsFactors = FALSE))

}

# extract large cities
villes = read_csv("data/geo_villes.csv")
villes$departement[ villes$departement == "Métropole de Lyon" ] = "Rhône"
villes$departement[ villes$departement == "Val-d'Oise" ] = "Val-d’Oise"

for(i in 1:nrow(villes)) {

  if(!villes$ville[ i ] %in% g$ville & villes$ville[ i ] != "Saint-Martin")
    g = rbind(g, data.frame(ville = villes$ville[ i ],
                            departement = villes$departement[ i ],
                            numero = depts$numero[ depts$departement == villes$departement[ i ] ],
                            stringsAsFactors = FALSE))

}

# list now contains préfectures, sous-préfectures and large cities
g = arrange(g, ville)

# remove DOM-TOM and/or ambiguous duplicate case
g[ g$ville %in% g$ville[ duplicated(g$ville) ], ]
g = filter(g, nchar(numero) < 3 & ville != "Saint-Denis")

# finally, make the city names match those in user dataset
g$ville = gsub("[[:punct:]]|[0-9]", " ", g$ville)       # unwanted symbols
g$ville = iconv(g$ville, to = "ASCII//TRANSLIT")        # accents to punctuation
g$ville = gsub("[[:punct:]]", "", g$ville)              # accents out
g$ville = tolower(str_trim(gsub("\\s+", " ", g$ville))) # spaces out

# the only cases that we miss are DOM-TOM and 'Saint-' homonyms:
# Saint-Denis (93), Saint-Louis (69), Saint-André (59), etc.
filter(u, !is.na(ville) & !ville %in% g$ville) %>%
  select(ville, departement, region) %>%
  unique %>%
  data.frame

# let's be thorough (1): some users have only the departement informed
d = as.data.frame(table(u$departement[ is.na(u$ville) & !is.na(u$departement) ]),
                  stringsAsFactors = FALSE)
names(d) = c("departement", "add_dep")

# let's assign them to the préfecture of the departement:

# first, convert the names of the departements
depts$departement = gsub("[[:punct:]]|[0-9]", " ", depts$departement)       # unwanted symbols
depts$departement = iconv(depts$departement, to = "ASCII//TRANSLIT")        # accents to punctuation
depts$departement = gsub("[[:punct:]]", "", depts$departement)              # accents out
depts$departement = tolower(str_trim(gsub("\\s+", " ", depts$departement))) # spaces out

# second, convert the names of the préfectures
depts$prefecture = gsub("[[:punct:]]|[0-9]", " ", depts$prefecture)       # unwanted symbols
depts$prefecture = iconv(depts$prefecture, to = "ASCII//TRANSLIT")        # accents to punctuation
depts$prefecture = gsub("[[:punct:]]", "", depts$prefecture)              # accents out
depts$prefecture = tolower(str_trim(gsub("\\s+", " ", depts$prefecture))) # spaces out

# now create a vector of préfectures
dept = depts$prefecture

# and name it after their departement
names(dept) = depts$departement

# and finally convert the additional users
d$ville = dept[ d$departement ]

# let's now merge the data
n = full_join(n, d, by = "ville")
n$users[ is.na(n$users) ] = 0
n$add_dep[ is.na(n$add_dep) ] = 0
n$total = n$users + n$add_dep

# how many additional users did we geocode from their departement? quite a few:
sum(n$total) - sum(n$users)

# let's be thorough (2): some users have only the region informed
r = as.data.frame(table(u$region[ is.na(u$ville) & !is.na(u$departement) ]),
                  stringsAsFactors = FALSE)
names(r) = c("region", "add_reg")

# let's assign them to the chef-lieu of the region:

reg = read_csv("data/geo_regions.csv")

# first, convert the names of the departements
reg$region = gsub("[[:punct:]]|[0-9]", " ", reg$region)       # unwanted symbols
reg$region = iconv(reg$region, to = "ASCII//TRANSLIT")        # accents to punctuation
reg$region = gsub("[[:punct:]]", "", reg$region)              # accents out
reg$region = tolower(str_trim(gsub("\\s+", " ", reg$region))) # spaces out

# since we are using a different dataset for region names, check that they
# match the region names in the users data: missing only the DOM-TOM, fine
table(u$region[ !u$region %in% reg$region ])

# second, convert the names of the chefs-lieu
reg$cheflieu = gsub("[[:punct:]]|[0-9]", " ", reg$cheflieu)       # unwanted symbols
reg$cheflieu = iconv(reg$cheflieu, to = "ASCII//TRANSLIT")        # accents to punctuation
reg$cheflieu = gsub("[[:punct:]]", "", reg$cheflieu)              # accents out
reg$cheflieu = tolower(str_trim(gsub("\\s+", " ", reg$cheflieu))) # spaces out

# since we are still using a different dataset, check again that they match the
# town names in the users dataset; all of them are found, so we are fine again
table(reg$cheflieu[ !reg$cheflieu %in% u$ville ])

# now create a vector of chefs-lieu
regs = reg$cheflieu

# and name it after their region
names(regs) = reg$region

# and finally convert the additional users
r$ville = regs[ r$region ]
r = r[ !is.na(r$ville), ]

# let's now merge the data
n = full_join(n, r, by = "ville")
n$users[ is.na(n$users) ] = 0 # unneeded, just to be safe
n$add_reg[ is.na(n$add_reg) ] = 0
n$total = n$total + n$add_reg

# how many additional users did we geocode from their departement? just a few:
sum(n$total) - sum(n$add_dep) - sum(n$users)

# and finally, we get the number of users by departement number
g = inner_join(g, select(n, ville, total), by = "ville") %>%
  select(total, id = numero) %>%
  group_by(id) %>%
  summarise(users = sum(total)) %>%
  arrange(id)

# correct the numbers to match the map data
g$id[ nchar(g$id) < 2 ] = paste0("0", g$id[ nchar(g$id) < 2 ])

# log10 tiles
g$Q = cut(g$users, c(0, 100, 1000, Inf), right = FALSE)
levels(g$Q) = c("< 100", "< 1000", "1000+")

# add population figures
dept = depts[, c("numero", "pop2011") ]
names(dept)[1] = "id"
dept$id[ nchar(dept$id) < 2 ] = paste0("0", dept$id[ nchar(dept$id) < 2 ])

# compute users per 1,000 inhabitants
g = inner_join(g, dept, by = "id") %>%
  mutate(ratio = 1000 * users / pop2011, users_ratio = 100 * users / sum(users)) %>%
  arrange(-ratio)

# map theme
theme_mapped = theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

gpclibPermit()
gpclibPermitStatus()

# http://professionnels.ign.fr/geofla#tab-3
depmap = readOGR(dsn = "GEOFLA_2-0_DEPARTEMENT_SHP_LAMB93_FXX_2014-12-05/GEOFLA/1_DONNEES_LIVRAISON_2014-12-00068/GEOFLA_2-0_SHP_LAMB93_FR-ED141/DEPARTEMENT", layer = "DEPARTEMENT")
depggm = fortify(depmap, region = "CODE_DEPT")

# add users variables
depggm = left_join(depggm, g, by = "id")

ggplot(depggm, aes(map_id = id)) +
  geom_map(aes(fill = Q), map = depggm, color = "white", size = 1) +
  expand_limits(x = depggm$long,
                y = depggm$lat) +
  scale_fill_brewer("Followers", palette = "Reds", na.value = "grey") +
  coord_equal() +
  theme_mapped +
  labs(y = NULL, x = NULL)

ggsave("plots/map2_country.png", width = 12, height = 12)

#==============================================================================
# MAPS 3 AND 4: FOLLOWERS AND POPULATION RATIOS
#==============================================================================

dep = select(depts, departement, id = numero, pop_ratio = pop2011) %>%
  filter(nchar(id) < 3)
dep$pop_ratio = 100 * dep$pop_ratio / sum(dep$pop_ratio)

# correct the numbers to match the map data
dep$id[ nchar(dep$id) < 2 ] = paste0("0", dep$id[ nchar(dep$id) < 2 ])

# add to map data
depggm = left_join(depggm, dep, by = "id")

# # show over-represented departements
# qplot(data = unique(select(depggm, id, pop_ratio, users_ratio, departement)),
#       y = pop_ratio, x = users_ratio, label = departement,
#       color = (users_ratio / pop_ratio) > 1, geom = "text") +
#   scale_y_log10() +
#   scale_x_log10() +
#   guides(color = FALSE) +
#   coord_equal()

g1 = ggplot(depggm, aes(map_id = id)) +
  geom_map(aes(fill = cut(pop_ratio, quantile(pop_ratio), include.lowest = TRUE, dig.lab = 2)),
           map = depggm, color = "white", size = 1) +
  expand_limits(x = depggm$long,
                y = depggm$lat) +
  scale_fill_brewer("Percentage quartiles", palette = "Greys") +
  coord_equal() +
  theme_mapped +
  labs(y = NULL, x = NULL, title = "Distribution of French population")

ggsave("plots/map3_ratio_population.png", g1, width = 9, height = 9)

g2 = ggplot(depggm, aes(map_id = id)) +
  geom_map(aes(fill = cut(users_ratio, quantile(users_ratio), include.lowest = TRUE, dig.lab = 2)),
           map = depggm, color = "white", size = 1) +
  expand_limits(x = depggm$long,
                y = depggm$lat) +
  scale_fill_brewer("Percentage quartiles", palette = "Greys") +
  coord_equal() +
  theme_mapped +
  labs(y = NULL, x = NULL, title = "Distribution of selected followers")

ggsave("plots/map4_ratio_followers.png", g2, width = 9, height = 9)

#==============================================================================
# POPULATION CORRELATES
#==============================================================================

# Overall population distribution
corr = unique(depggm[, c("id", "users_ratio", "pop_ratio") ])
with(corr, cor(users_ratio, pop_ratio)) # rho ~ .46 (main outlier: Paris)

# Population estimée par tranche d'âge (Irdes)
# https://www.data.gouv.fr/fr/datasets/population-par-tranche-d-age-et-sexe-estimations-localisees-de-population/
pop = read_csv("data/population2014.csv")
pop = inner_join(select(pop, -dept), select(g, id, users), by = "id")

corr = data.frame()
for(i in c("0_19", "20_39", "40_59", "60_74", "75")) {
  corr = rbind(corr, data.frame(
    pop = gsub("_", "-", i),
    rho = as.numeric(cor(pop[, paste0("pop", i) ], pop$users))
  ))
}
print(kable(corr, digits = 2))

# Taux d'inscription sur les listes électorales (2013)
# https://www.data.gouv.fr/fr/datasets/taux-d-inscription-sur-les-listes-electorales-rdl/
particip = read_csv("data/inscriptions2013.csv")

particip$id = as.numeric(particip$CODE)
particip$id[ nchar(particip$id) < 2 ] = paste0("0", particip$id[ nchar(particip$id) < 2 ])
particip$id[ particip$LB_DEPARTEMENT == "CORSE SUD" ] = "2A"
particip$id[ particip$LB_DEPARTEMENT == "HAUTE CORSE" ] = "2B"
particip$inscrits = gsub("%", "", gsub(",", ".", particip$TAUX_INSCIPTION))
particip$inscrits = as.numeric(particip$inscrits)
particip = inner_join(select(particip, id, inscrits), select(g, id, users, pop2011))

# # remove Seine-Saint-Denis because missing city of Saint-Denis
# qplot(data = filter(particip, id != 93), y = users, x = inscrits,
#       size = log10(pop2011)) +
#   geom_text(aes(label = id, size = log10(pop2011) / 3), color = "white") +
#   scale_size_area(max_size = 9) +
#   scale_y_log10() +
#   geom_smooth(se = FALSE, lty = "dashed") +
#   theme_bw() +
#   theme(#panel.background = element_blank(),
#     legend.position = "none") +
#   labs(y = "Followers (log-10 scale)\n", x = "\nVoting registration rate (%)")

with(filter(particip, id != 93), cor(log10(1 + users), inscrits)) # -.62
