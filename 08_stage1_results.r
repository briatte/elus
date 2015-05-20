#==============================================================================
#
# 08_stage1_results.r -- plot the results of the model at Stage 1
#
# This script visualizes the results that directly relate to the estimated ideal
# points of mass and elite users. All plots are saved to the "plots" folder.
# The last part of the script also runs Heidelberger diagnostics on the results.
#
# Several figures replicate those found in Pablo Barberá's paper "Birds of the
# Same Feather Tweet Together: Bayesian Ideal Point Estimation Using Twitter
# Data", Political Analysis (2015) 23:76–91.
#
#==============================================================================

library(dplyr)
library(readr)
library(stringr)

library(coda)
library(rstan)

library(ggplot2)
library(gridExtra)
library(reshape2)

library(rgdal)
library(rgeos)
library(maptools)

load('draft/stanfit/matrix_selected.rda')
load('draft/stanfit/stanfit.rda')

theme_paper =  theme_bw(14) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        strip.text = element_text(size = rel(1)),
        strip.background = element_rect(fill = "grey90"),
        legend.text = element_text(size = rel(1)),
        axis.ticks.x = element_blank())

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
# HEIDELBERGER DIAGNOSTICS
#==============================================================================

thetas = rstan::extract(stan.fit, pars = "theta")
phis = rstan::extract(stan.fit, pars = "phi")

# examples of (stationary) chains
examples = data.frame(
  iteration = 1:nrow(phis[[1]]),
  lepenjm = phis[[1]][, which(colnames(y) == "lepenjm") ],
  mlp_officiel = phis[[1]][, which(colnames(y) == "mlp_officiel") ],
  nicolassarkozy = phis[[1]][, which(colnames(y) == "nicolassarkozy") ],
  fhollande = phis[[1]][, which(colnames(y) == "fhollande") ],
  jlmelenchon = phis[[1]][, which(colnames(y) == "jlmelenchon") ],
  random_user = thetas[[1]][, sample(1:ncol(thetas[[1]]), 1) ]
)

# Heidelberger and Welch's convergence diagnostic
heidelberger = apply(examples[, -1], 2, heidel.diag)[4, ]
heidelberger = heidelberger == 1

qplot(data = melt(examples, "iteration") %>%
        mutate(heidelberger = heidelberger[ variable ]),
      y = value, x = iteration, lty = heidelberger, geom = "line") +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 1)) +
  scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed")) +
  guides(linetype = FALSE) +
  labs(y = "Twitter-based ideal point\n", x = "\nIteration (after thinning)") +
  theme_paper

ggsave("plots/stage1_heidelberger.pdf", width = 10, height = 10)
ggsave("plots/stage1_heidelberger.png", width = 10, height = 10)

# identify all non-stationary chains
hd_pol = apply(phis[[1]], 2, heidel.diag)
table(hd_pol[4, ], exclude = NULL)

# problems: @jnguerini and @olivierfalorni
colnames(y)[ which(hd_pol[4, ] != 1) ]

#==============================================================================
# IDEAL POINTS FOR POLITICIANS
#==============================================================================

d = read_csv("data/politicians.csv", col_types = list(id = col_character())) %>%
  select(name, gender, party, twitter)

phis = data.frame(twitter = colnames(y),
                  phat = apply(phis$phi, 2, mean),
                  phat_sd = apply(phis$phi, 2, sd),
                  phat_q025 = apply(phis$phi, 2, quantile, probs = c(0.025)),
                  phat_q975 = apply(phis$phi, 2, quantile, probs = c(0.975)),
                  n_eff = round(summary(stan.fit, pars = "phi")$summary[, "n_eff"]),
                  stringsAsFactors = FALSE) %>%
  inner_join(d, ., by = "twitter") %>%
  arrange(-phat)

#==============================================================================
# PARTY POSITIONS (equivalent to Barberá 2015, Fig. 3)
#==============================================================================

# party colors

p = read_csv("data/parties.csv")
colors = p$color
names(colors) = p$party

# party means

normalize <- function(x){ (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) }

pm = group_by(mutate(phis, phat = normalize(phat)), party) %>%
  summarise(n = n(), mu = mean(phat), min = min(phat), max = max(phat)) %>%
  left_join(., mutate(p, parlgov = normalize(parlgov)), by = "party")

# # violins plot
# qplot(data = mutate(phis, phat = normalize(phat)),
#                     x = reorder(party, phat, mean), y = phat,
#                     color = party, geom = "violin") +
#   geom_point(data = pm, aes(x = reorder(party, mu), y = mu), shape = 1, size = 4) +
#   geom_point(data = pm, aes(x = reorder(party, mu), y = parlgov), shape = 4, size = 4) +
#   scale_color_manual("", values = colors) +
#   guides(color = FALSE) +
#   labs(x = NULL, y = paste("Twitter-based mean ideal points (o)\n",
#                            "and ParlGov Left/Right scores (x)\n")) +
#   theme_paper

# range plot
qplot(data = pm, x = reorder(party, mu), color = party,
      y = mu, ymin = min, ymax = max, geom = "pointrange") +
  geom_point(aes(y = mu), size = 4, color = "white") +
  geom_point(aes(y = mu), shape = 1, size = 4) +
  geom_point(aes(y = parlgov), shape = 4, size = 4) +
  scale_color_manual("", values = colors) +
  guides(color = FALSE) +
  labs(x = NULL, y = paste("Twitter-based mean ideal points (o)\n",
                           "and ParlGov Left/Right scores (x)\n")) +
  theme_paper

ggsave("plots/ideal_points_parties.pdf", width = 10, height = 5)
ggsave("plots/ideal_points_parties.png", width = 10, height = 5)

# # party orderings
#
# arrange(pm, mu) %>%
#   mutate(order_phat = order(mu), order_parlgov = order(parlgov)) %>%
#   select(party, order_phat, order_parlgov)

# # correlation to expert scores
#
# group_by(phis, party) %>%
#   summarise(mu = mean(phat)) %>%
#   inner_join(., select(p, party, parlgov, chess), by = "party") %>%
#   melt(., c("party", "mu")) %>%
#   qplot(data = ., x = value, y = mu, label = party, geom = "text") +
#   facet_wrap(~ variable) +
#   theme_paper

#==============================================================================
# IDEAL POINTS FOR FOLLOWERS
#==============================================================================

u = read_csv("data/users.csv", col_types = list(id = col_character()))

est = summary(stan.fit, permute = FALSE)

# sanity check: same number of thetas in model than rows in matrix
thetas = dimnames(est$c_summary)[[1]]
thetas = thetas[ grepl("theta", thetas) ]
stopifnot(length(thetas) == nrow(y))

est = data.frame(id = names(stan.fit@stan_args[[1]]$init$beta),
                 phat = est$c_summary[ paste0("theta[", 1:length(thetas), "]"), 1, 1 ],
                 stringsAsFactors = FALSE) %>%
  left_join(., u, by = "id")

#==============================================================================
# KEY POLITICAL ACCOUNTS (equivalent to Barberá 2015, Fig. 2)
#==============================================================================

key = select(est, screen_name, phat) %>%
  filter(tolower(screen_name) %in%
           tolower(c("SOS_Racisme", "Elysee", "partisocialiste", "FNJ_officiel",
                     "PartiRadicalG", "NicolasSarkozy", "UDI_off", "ump")))

# other left-wing candidates: EELVToulouse, EELVCreteil, EELVIdF,
#   JeunesSocialist, mairie18paris, MDM_France
# View(filter(est, verified, phat < -1))

# other right-wing candidates: CG06, JeunesUMP6006, JeunesUMP6007,
#   Manifpourtous84, JEavecSarkozy, ump84vaucluse
# View(filter(est, verified, phat > 1))

key$ycoord = c(0.1, 0.1, 0.12, 0.1, 0.12, 0.12, 0.1, 0.1)
key$xcoord = 0.5 # in case you need horizontal adjustment

qplot(data = filter(phis, party %in% c("UMP", "PS")),
      x = phat, fill = party, alpha = I(.5), geom = "density") +
  annotate("text", x = -1.9, y = 1.1, label = "Socialists (PS)") +
  annotate("text", x = +2.1, y = 1.1, label = "Conservatives (UMP)") +
  scale_fill_manual("Twitter-based ideal points of politicians", values = colors) +
  geom_point(data = key, aes(y = 0, x = phat, fill = NULL)) +
  geom_segment(data = key, aes(y = -4.5 * ycoord, yend = 0,
                               x = phat, xend = phat, fill = NULL)) +
  geom_text(data = key, aes(y = -5 * ycoord, hjust = xcoord, x = phat,
                            fill = NULL, label = screen_name), size = 3.5) +
  labs(x = "\nTwitter-based ideal points", y = "Distribution density\n") +
  ylim(-0.7, 1.2) +
  theme_paper +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

ggsave("plots/ideal_points_key_actors.pdf", width = 10, height = 5)
ggsave("plots/ideal_points_key_actors.png", width = 10, height = 5)

#==============================================================================
# MASS VERSUS ELITES (equivalent to Barberá 2015, Fig. 4)
#==============================================================================

# left panel
ideal = rbind(select(phis, id = twitter, phat), select(est, id, phat))
ideal$id = ifelse(grepl("\\D", ideal$id), "Politicians", "Followers")
ideal$panel = "Elite and Mass Ideal Points"

# right panel
est$profile = "No indication"
est$profile[ grepl(paste0("#(", paste0(p$party[ p$side < 0 & p$party != "DVG" ], collapse = "|"), ")"),
                   est$description) ] = "Left-wing"
est$profile[ grepl(paste0("#(", paste0(p$party[ p$side > 0 & p$party != "DVD" ], collapse = "|"), ")"),
                   est$description) ] = "Right-wing"

table(est$profile, exclude = NULL)
est$panel = "Mass Ideal Points"

# merge and plot
ideal = rbind(ideal, select(est, id = profile, phat, panel))

g = by(data = ideal, INDICES = ideal$panel, FUN = function(data) {

  g = ggplot(data, aes(x = phat, lty = id)) +
    geom_density() +
    scale_linetype_discrete("") +
    scale_x_continuous(breaks = -5:5) +
    theme_paper +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),

          legend.position = c(0.175, 0.825)) +
    facet_grid(. ~ panel) +
    labs(y = NULL,
         x = "\nTwitter-based ideal point")

  # left side
  if(any(grepl("Elite", data$panel)))
    g = g +
      ylab("Distribution density\n") +
      theme(plot.margin = unit(c(0.5, 0, 0.5, 0.5), "cm"))

  g
})

pdf("plots/ideal_points_elite_mass.pdf", width = 10, height = 5)
grid.arrange(g[[1]], g[[2]], ncol = 2)
dev.off()

png("plots/ideal_points_elite_mass.png", width = 10, height = 5, units = "in", res = 300)
grid.arrange(g[[1]], g[[2]], ncol = 2)
dev.off()

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

rm(list = ls())
gc()
