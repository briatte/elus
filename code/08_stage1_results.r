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

load('draft-models/stanfit-05/matrix_selected.rda')
load('draft-models/stanfit-05/stanfit.rda')

theme_paper =  theme_bw(14) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        strip.text = element_text(size = rel(1)),
        strip.background = element_rect(fill = "grey90"),
        legend.text = element_text(size = rel(1)),
        axis.ticks.x = element_blank())

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
  plaurent_pcf = phis[[1]][, which(colnames(y) == "plaurent_pcf") ],
  random_user = thetas[[1]][, sample(1:ncol(thetas[[1]]), 1) ]
)

# Heidelberger and Welch's convergence diagnostic
heidelberger = apply(examples[, -1], 2, heidel.diag)[4, ]
heidelberger = heidelberger == 1

qplot(data = melt(examples, "iteration") %>%
        mutate(heidelberger = heidelberger[ variable ]),
      y = value, x = iteration, lty = heidelberger, geom = "line") +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
  scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed")) +
  guides(linetype = FALSE) +
  labs(y = "Twitter-based ideal point\n", x = "\nIteration (after thinning)") +
  theme_paper

ggsave("plots/stage1_heidelberger.pdf", width = 10, height = 10)
ggsave("plots/stage1_heidelberger.png", width = 10, height = 10)

# identify all non-stationary chains
hd_pol = apply(phis[[1]], 2, heidel.diag)
table(hd_pol[4, ], exclude = NULL)

# problems: @jnguerini and @m_chamussy
colnames(y)[ which(hd_pol[4, ] != 1) ]

#==============================================================================
# IDEAL POINTS FOR POLITICIANS (equivalent to Barberá 2015, Appendix Figure 3)
#==============================================================================

d = read_csv("data/politicians.csv", col_types = list(id = col_character())) %>%
  select(name, gender, party, twitter, type, followers)

phis = data.frame(twitter = colnames(y),
                  phat = apply(phis$phi, 2, mean),
                  phat_sd = apply(phis$phi, 2, sd),
                  phat_q025 = apply(phis$phi, 2, quantile, probs = c(0.025)),
                  phat_q975 = apply(phis$phi, 2, quantile, probs = c(0.975)),
                  n_eff = round(summary(stan.fit, pars = "phi")$summary[, "n_eff"]),
                  stringsAsFactors = FALSE) %>%
  inner_join(d, ., by = "twitter") %>%
  arrange(-phat)

# party colors

p = read_csv("data/parties.csv")
colors = p$color
names(colors) = p$party

# ideal points for most followed MEPs, MPs and Senators

qplot(data = group_by(phis, type) %>%
        arrange(-followers) %>%
        mutate(order = 1:n()) %>%
        filter(order %in% 1:50 & type %in% c("MEP", "MP", "Senator")),
      y = reorder(twitter, phat), x = phat, color = party) +
  # almost invisible error bars
  # geom_segment(aes(x = phat_q025, xend = phat_q975, yend = reorder(twitter, phat))) +
  facet_wrap(~ type, nrow = 1, scales = "free_y") +
  scale_color_manual("", values = colors, breaks = p$party) +
  labs(y = NULL, x = "\nTwitter-based ideal point") +
  theme_paper +
  theme(legend.key = element_blank(),
        axis.text.y = element_text(size = rel(3/4)),
        legend.position = "bottom")

ggsave("plots/ideal_points_politicians.pdf", width = 10, height = 10)
ggsave("plots/ideal_points_politicians.png", width = 10, height = 10)

phis$type = reorder(phis$type, phis$phat, mean)

qplot(data = phis, y = phat, x = type, colour = party, size = log10(followers)) +
  geom_violin(aes(group = type), fill = NA) +
  geom_hline(data = group_by(phis, type) %>%
               summarise(phat = mean(phat)),
             aes(yintercept = phat, color = NULL), lty = "dashed") +
  scale_color_manual("", values = colors, breaks = p$party) +
  facet_grid(. ~ type, scales = "free_x") +
  labs(y = "Twitter-based ideal point\n", x = "\nType of mandate") +
  theme_paper

ggsave("plots/ideal_points_mandates.pdf", width = 10, height = 10)
ggsave("plots/ideal_points_mandates.png", width = 10, height = 10)

#==============================================================================
# PARTY POSITIONS (equivalent to Barberá 2015, Fig. 3)
#==============================================================================

# range plot with Chapel Hill scores (0-10)

std01 <- function(x) { (x - min(x)) / (max(x) - min(x)) }

pm = group_by(mutate(phis, phat = 10 * std01(phat)), party) %>%
  summarise(n = n(), mu = mean(phat), sd = sd(phat)) %>%
  left_join(., p, by = "party")

pm$party = factor(pm$party, levels = pm$party[ order(pm$mu) ])

qplot(data = pm, x = "A", xend = "A", y = mu - 2 *sd, yend = mu + 2 * sd,
      lty = "Ideal point (Twitter)", geom = "segment") +
  geom_point(aes(y = mu), size = 2) +
  geom_segment(aes(x = "B", xend = "B", y = chess - 2 * chess_sd, yend = chess + 2 * chess_sd,
                   lty = "Party position (Chapel Hill)")) +
  geom_point(aes(x = "B", y = chess), size = 2) +
  geom_text(aes(y = -1.5, label = n)) +
  scale_color_manual("", values = colors) +
  scale_linetype_manual("", values = c("solid", "dashed")) +
  facet_grid(. ~ party, scales = "free_x") +
  labs(x = NULL, y = "Mean score ± 2 standard deviations\n") +
  theme_paper +
  theme(legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")

ggsave("plots/ideal_points_parties_chess.pdf", width = 10, height = 5)
ggsave("plots/ideal_points_parties_chess.png", width = 10, height = 5)

# violin plot with Chapel Hill scores (0-10)

qplot(data = mutate(phis, phat = 10 * std01(phat)),
                    x = reorder(party, phat, mean), y = phat,
                    fill = party, color = I("grey50"), alpha = I(.5), geom = "violin") +
  geom_point(data = pm, aes(x = reorder(party, mu), y = mu), shape = 1, size = 4) +
  geom_point(data = pm, aes(x = reorder(party, mu), y = chess), shape = 4, size = 4) +
  scale_fill_manual("", values = colors) +
  guides(fill = FALSE) +
  labs(x = NULL, y = paste("Twitter-based mean ideal points (o)\n",
                           "and Chapel Hill Left/Right scores (x)\n")) +
  theme_paper

# range plot with normalized ParlGov scores

normalize <- function(x){ (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) }

pm = group_by(mutate(phis, phat = normalize(phat)), party) %>%
  summarise(n = n(), mu = mean(phat), min = min(phat), max = max(phat)) %>%
  left_join(., mutate(p, parlgov = normalize(parlgov)), by = "party")

qplot(data = pm, x = reorder(party, mu), color = party,
      y = mu, ymin = min, ymax = max, geom = "pointrange") +
  geom_point(aes(y = mu), size = 4, color = "white") +
  geom_point(aes(y = mu), shape = 1, size = 4) +
  geom_point(aes(y = parlgov), shape = 4, size = 4) +
  scale_color_manual("", values = colors) +
  scale_x_discrete(breaks = with(pm, reorder(party, mu)),
                   labels = with(pm, paste0(reorder(party, mu), "\nn = ",
                                            reorder(n, mu)))) +
  guides(color = FALSE) +
  labs(x = NULL, y = paste("Normalized Twitter-based mean ideal points (o)\n",
                           "and normalized ParlGov Left/Right scores (x)\n")) +
  theme_paper

ggsave("plots/ideal_points_parties_parlgov.pdf", width = 10, height = 5)
ggsave("plots/ideal_points_parties_parlgov.png", width = 10, height = 5)

# largest differences (mean error = .66)
mutate(pm, diff = mu - chess) %>%
  filter(abs(diff) > mean(abs(diff), na.rm = TRUE)) %>%
  arrange(diff)

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

# remember, n = 10
with(pm, cor(mu, chess, use = "pairwise.complete.obs"))   # rho ~ .96
with(pm, cor(mu, parlgov, use = "pairwise.complete.obs")) # rho ~ .95

#==============================================================================
# SELECTED ELITES
#==============================================================================

# full distribution with former Prime Ministers or Presidents
qplot(data = phis, y = reorder(name, phat), x = phat,
      color = party, alpha = I(.25)) +
  geom_segment(aes(x = phat_q025, xend = phat_q975, yend = reorder(name, phat)), alpha = .25) +
  geom_text(data = filter(phis, grepl("Raffarin|Villepin|Fillon|Ayrault|Valls|Camba|Sarko|Hollande|(ne |al-)Le Pen|Cosse|Duflot|Voynet|ian Paul",
                                      name, ignore.case = TRUE)), aes(label = name), alpha = 1) +
  scale_color_manual("", values = colors, breaks = pm$party[ order(pm$mu) ]) +
  labs(y = NULL, x = "\nTwitter-based ideal point") +
  theme_paper +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom")

ggsave("plots/ideal_points_elites.pdf", width = 8, height = 16)
ggsave("plots/ideal_points_elites.png", width = 8, height = 16)

# governments

min = read_csv("data/governments.csv") %>% filter(gov != "Ayrault-1")
min = left_join(min, d, by = "name")

min$status[ min$status == "in" ] = ""
min$status[ min$status != "" ] = "replaced/resigned"

left_join(min, select(phis, twitter, phat, phat_q025, phat_q975),
          by = "twitter") %>%
  filter(!is.na(phat)) %>%
  select(-ministry, -status) %>%
  unique %>%
  arrange(phat) %>%
  qplot(data = ., y = reorder(name, phat), x = phat, color = party) +
  geom_segment(aes(x = phat_q025, xend = phat_q975, yend = reorder(name, phat))) +
  geom_text(aes(label = name, x = phat_q025 * 1.025), hjust = 1, size = 4) +
  geom_text(aes(label = name, x = phat_q025 * 1.025), color = "black", alpha = .5, hjust = 1, size = 4) +
  geom_vline(data = left_join(min, select(phis, twitter, phat, phat_q025, phat_q975),
                              by = "twitter") %>%
               group_by(gov) %>%
               summarise(mu = median(phat, na.rm = TRUE)),
             aes(xintercept = mu), lty = "dashed", color = "grey50") +
  scale_color_manual("", values = colors) +
  guides(color = FALSE) +
  labs(y = "Government member", x = "\nTwitter-based ideal point (dashed line at median)") +
  xlim(-3, 0) +
  facet_grid(gov ~ ., scales = "free", space = "free") +
  theme_paper +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave("plots/ideal_points_governments.pdf", width = 10, height = 10)
ggsave("plots/ideal_points_governments.png", width = 10, height = 10)

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
           tolower(c("SOS_Racisme", "Elysee", "partisocialiste", "FN_officiel",
                     "PartiRadicalG", "UDI_off", "ump", "NicolasSarkozy")))

# other left-wing candidates (not all are verified accounts):
#   EELVToulouse, EELVCreteil, EELVIdF, JeunesSocialist, mairie18paris,
#   MDM_France, fhollande
# View(filter(est, verified, phat < -1))

# other right-wing candidates  (not all are verified accounts):
#   CG06, JeunesUMP6006, JeunesUMP6007, Manifpourtous84, JEavecSarkozy,
#   ump84vaucluse, journalPresent, FN_Vannes, radiocourtoisie, RenaudCamus
# View(filter(est, verified, phat > 1))

key$ycoord = c(0.08, 0.08, 0.12, 0.12, 0.12, 0.12, 0.08, 0.08)
key$xcoord = c(   1,  0.5,  0.5,  0.5,  0.5,    1,  0.5, -0.1)

qplot(data = filter(phis, party %in% c("UMP", "PS")),
      x = phat, group = party, alpha = I(.5), geom = "density") + # fill = party,
  annotate("text", x = -1.8, y = 1.3, label = "Socialists (PS)") +
  annotate("text", x = +2.0, y = 1.1, label = "Conservatives (UMP)") +
  # scale_fill_manual("Twitter-based ideal points of politicians", values = colors) +
  geom_point(data = key, aes(y = 0, x = phat, group = NULL)) +
  geom_segment(data = key, aes(y = -4.5 * ycoord, yend = 0,
                               x = phat, xend = phat, group = NULL)) +
  geom_text(data = key, aes(y = -5 * ycoord, hjust = xcoord, x = phat,
                            group = NULL, label = screen_name), size = 3.5) +
  labs(x = "\nTwitter-based ideal points", y = "Distribution density\n") +
  ylim(-0.7, 1.4) +
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
                   est$description, ignore.case = TRUE) ] = "Left-wing"
est$profile[ grepl(paste0("#(", paste0(p$party[ p$side > 0 & p$party != "DVD" ], collapse = "|"), ")"),
                   est$description, ignore.case = TRUE) ] = "Rright-wing"

table(est$profile)

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

save(phis, est, file = "model/stage1_results.rda")

rm(list = ls())
gc()
