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

library(coda)
library(rstan)

library(ggplot2)
library(gridExtra)
library(reshape2)

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

#==============================================================================
# IDEAL POINTS FOR POLITICIANS
#==============================================================================

d = read_csv("data/politicians.csv", col_types = list(id = col_character())) %>%
  select(name, gender, party, twitter)

phis = rstan::extract(stan.fit, pars = "phi")
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

pdf("plots/ideal_points_elite_mass.pdf", height = 5, width = 10)
grid.arrange(g[[1]], g[[2]], ncol = 2)
dev.off()

png("plots/ideal_points_elite_mass.png", height = 5, width = 10, units = "in", res = 300)
grid.arrange(g[[1]], g[[2]], ncol = 2)
dev.off()

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

# Heidelberger tests
heidelberger = apply(examples[, -1], 2, heidel.diag)[4, ]
heidelberger = heidelberger == 1

qplot(data = melt(examples, "iteration") %>%
        mutate(heidelberger = heidelberger[ variable ]),
      y = value, x = iteration, color = heidelberger, geom = "line") +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 1)) +
  scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred")) +
  guides(color = FALSE) +
  labs(y = "Twitter-based ideal point\n", x = "\nIteration (after thinning)") +
  theme_paper

ggsave("plots/stage1_heidelberger.pdf", width = 10, height = 10)
ggsave("plots/stage1_heidelberger.png", width = 10, height = 10)

# non-stationary chains
hd_pol = apply(phis[[1]], 2, heidel.diag)
table(hd_pol[4, ], exclude = NULL)

# problematic politicians:
# @jnguerini and @olivierfalorni
colnames(y)[ which(hd_pol[4, ] != 1) ]

rm(list = ls())
gc()
