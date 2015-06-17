#==============================================================================
#
# 05_map.r -- plot maps of followers located in metropolitan France
#
# The script maps politicians and followers and correlates the number of
# followers with age groups at the level of each metropolitan département.
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

library(xtable)

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

# calls rgeos
gpclibPermit()
gpclibPermitStatus()

dir.create("plots", showWarnings = FALSE)

# load informative followers
u = read_csv("data/users.csv", col_types = list(id = col_character()))

# comment out to plot maps out of all users instead of sampled ones
u = filter(u, sample)

# get follower counts
n = as.data.frame(table(u$ville), stringsAsFactors = FALSE)
names(n) = c("ville", "users")

#==============================================================================
# CHOROPOLETHS 1 AND 2: FOLLOWERS AND POLITICIANS
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

# save for later use
write_csv(g, "data/cities.csv")

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

# how many additional users did we add from their departement? quite a few:
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

# how many additional users did we add from their region? just a few:
sum(n$total) - sum(n$add_dep) - sum(n$users)

# and finally, we get the number of users by departement number
g = inner_join(g, select(n, ville, total), by = "ville") %>%
  select(total, id = numero) %>%
  group_by(id) %>%
  summarise(users = sum(total)) %>%
  arrange(id)

# correct the departement numbers to match the map data
g$id[ nchar(g$id) < 2 ] = paste0("0", g$id[ nchar(g$id) < 2 ])

# log10 tiles
g$Q = cut(g$users, c(0, 1, 100, 1000, Inf), right = FALSE)
levels(g$Q) = c("0", "< 100", "< 1000", "1000+")

# add population figures
dept = depts[, c("numero", "pop2011") ]
names(dept)[1] = "id"

# correct the departement numbers to match the map data
dept$id[ nchar(dept$id) < 2 ] = paste0("0", dept$id[ nchar(dept$id) < 2 ])

# % of total users located in each departement
g = inner_join(g, dept, by = "id") %>%
  mutate(ratio = 1000 * users / pop2011, users_ratio = 100 * users / sum(users)) %>%
  arrange(-ratio)

# IGN shapefiles: http://professionnels.ign.fr/geofla
depmap = readOGR(dsn = "maps/GEOFLA_2-0_DEPARTEMENT_SHP_LAMB93_FXX_2014-12-05/GEOFLA/1_DONNEES_LIVRAISON_2014-12-00068/GEOFLA_2-0_SHP_LAMB93_FR-ED141/DEPARTEMENT", layer = "DEPARTEMENT")
depggm = fortify(depmap, region = "CODE_DEPT")

# add users variables
depggm = left_join(depggm, g, by = "id") %>%
  mutate(panel = "Followers")

g1 = ggplot(depggm, aes(map_id = id)) +
  geom_map(aes(fill = Q), map = depggm, color = "white", size = 1) +
  expand_limits(x = depggm$long, y = depggm$lat) +
  scale_fill_brewer("", palette = "Greys", na.value = "white") +
  coord_equal() +
  facet_grid(. ~ panel) +
  theme_mapped +
  theme(plot.margin = unit(c(0.5, -1, 0.5, 0), "cm")) +
  labs(y = NULL, x = NULL)

d = read_csv("data/politicians.csv", col_types = list(id = col_character()))

# get politician counts
p = as.data.frame(table(d$departement), stringsAsFactors = FALSE)
names(p) = c("departement", "politicians")

p = filter(p, departement != "") %>%
  left_join(select(depts, id = numero, departement), by = "departement")

# correct the departement numbers to match the map data
p$id[ nchar(p$id) < 2 ] = paste0("0", p$id[ nchar(p$id) < 2 ])

depggm = left_join(depggm, p, by = "id") %>%
  mutate(panel = "Politicians")

g2 = ggplot(depggm, aes(map_id = id)) +
  geom_map(aes(fill = cut(politicians, c(0, 1, 10, 20, Inf),
                          c("0", "< 10", "< 20", "20+"), right = FALSE)),
           map = depggm, color = "white", size = 1) +
  expand_limits(x = depggm$long, y = depggm$lat) +
  scale_fill_brewer("", palette = "Greys", na.value = "white") +
  coord_equal() +
  facet_grid(. ~ panel) +
  theme_mapped +
  theme(plot.margin = unit(c(0.5, 0, 0.5, -1), "cm")) +
  labs(y = NULL, x = NULL)

png("plots/map_counts.png", width = 10, height = 6, units = "in", res = 300)
grid.arrange(g1, g2, ncol = 2)
dev.off()

#==============================================================================
# CHOROPLETHS 3 AND 4: FOLLOWERS AND POPULATION (RATIOS)
#==============================================================================

dep = select(depts, departement, id = numero, pop_ratio = pop2011) %>%
  filter(nchar(id) < 3)

# % of population located in each departement (excluding Paris)
dep$pop_ratio = 100 * dep$pop_ratio / sum(dep$pop_ratio)

# correct the departement numbers to match the map data
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

scaler <- function(x) {
  q = quantile(x)
  cut(x, q, include.lowest = TRUE, right = FALSE,
      labels = paste("<", round(q[ -1 ], 1)))
}

depggm$panel = "% of French population"
g1 = ggplot(depggm, aes(map_id = id)) +
  geom_map(aes(fill = scaler(pop_ratio)), map = depggm, color = "white", size = 1) +
  expand_limits(x = depggm$long, y = depggm$lat) +
  scale_fill_brewer("", palette = "Greys") +
  coord_equal() +
  facet_grid(. ~ panel) +
  theme_mapped +
  theme(plot.margin = unit(c(0.5, -1, 0.5, 0), "cm")) +
  labs(y = NULL, x = NULL)

depggm$panel = "% of sampled followers"
g2 = ggplot(depggm, aes(map_id = id)) +
  geom_map(aes(fill = scaler(users_ratio)), map = depggm, color = "white", size = 1) +
  expand_limits(x = depggm$long, y = depggm$lat) +
  scale_fill_brewer("", palette = "Greys") +
  coord_equal() +
  facet_grid(. ~ panel) +
  theme_mapped +
  theme(plot.margin = unit(c(0.5, 0, 0.5, -1), "cm")) +
  labs(y = NULL, x = NULL)

png("plots/map_ratios.png", width = 10, height = 6, units = "in", res = 300)
grid.arrange(g1, g2, ncol = 2)
dev.off()

#==============================================================================
# POPULATION CORRELATES
#==============================================================================

# overall population distribution
corr = unique(depggm[, c("id", "users_ratio", "pop_ratio") ])
with(corr, cor(users_ratio, pop_ratio)) # rho ~ .46 (main outlier: Paris)
with(corr[ corr$id != 75, ], cor(users_ratio, pop_ratio))

# Population estimée par tranche d'âge (Irdes)
# https://www.data.gouv.fr/fr/datasets/population-par-tranche-d-age-et-sexe-estimations-localisees-de-population/
pop = read_csv("data/population2014.csv")
pop = inner_join(select(pop, -dept), select(g, id, users), by = "id")

tbl = data.frame()
for(i in c("0_19", "20_39", "40_59", "60_74", "75")) {
  tbl = rbind(tbl, data.frame(
    pop = gsub("_", "-", i),
    rho = as.numeric(cor(pop[, paste0("pop", i) ], pop$users))
  ))
}
tbl = xtable(select(tbl, `Age group` = pop, Correlation = rho),
             digits = 2, caption = "Pearson’s correlation coefficients between age-group population estimates and Twitter users at the level of $N$ = 96 \\emph{départements}.", label = "tbl:pearson")

print(tbl, booktabs = TRUE, include.rownames = FALSE,
      file = "tables/population.tex")

rm(list = ls())
gc()
