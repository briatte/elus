#==============================================================================
#
# A3_detrended_ca.r -- detrended correspondence analysis of the model matrix
#
# The script runs detrended correspondence analysis (CA) on the same matrix used
# in the rest of the analysis. The detrending suppresses the arch effect created
# by FN politicians and followers, returns at least two sensible dimensions, and
# correlates to the estimated ideal points obtained through the Bayesian model.
#
# See Hill, M.O. and H.G. Gauch, Jr. 1980. Detrended Correspondence Analysis:
# an improved ordination technique. Vegetatio 42:47-58.
#
#==============================================================================

library(dplyr)
library(readr)

library(ggplot2)

# correspondence analysis

# problem -- official package contains a small bug:
# the ca function returns an error when nd != NA
# library(ca)

# solution -- install alternative version with a fix
devtools::install_github("briatte/ca")
library(ca)

# detrended correspondence analysis
library(vegan)

theme_paper =  theme_bw(14) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        strip.text = element_text(size = rel(1)),
        strip.background = element_rect(fill = "grey90"),
        legend.text = element_text(size = rel(1)),
        axis.ticks.x = element_blank())

d = read_csv("data/politicians.csv", col_types = list(id = col_character()))

# colors for party affiliations
p = read_csv("data/parties.csv")
colors = p$color
names(colors) = p$party

load("model/matrix_selected.rda")

cat("Selected matrix:", nrow(y), "rows,", ncol(y), "cols\n")

min(colSums(y)) # politicians: min. number of informative followers (200)
min(rowSums(y)) # users: min. number of politicians followed (3)

# works only with the fixed version of the ca package
ca_2d = ca(y, nd = 2)

r = data_frame(twitter = ca_2d$colnames,
               d1 = ca_2d$colcoord[, 1],
               d2 = ca_2d$colcoord[, 2]) %>%
  left_join(select(d, twitter, party), by = "twitter")

# problem -- Front national politicians are orthogonal to all others
qplot(data = r, x = d1, y = d2, color = party) +
  scale_color_manual("", breaks = names(colors), values = colors) +
  labs(y = "Dimension 2\n", x = "\nDimension 1") +
  theme_paper +
  theme(legend.key = element_blank())

ggsave("plots/ca_2d.png", width = 7, height = 5)
ggsave("plots/ca_2d.pdf", width = 7, height = 5)

# solution -- switch to detrended correspondence analysis
ord = decorana(t(y),
               iweigh = 0, # no downweighting of rare groups
               iresc = 4,  # number of rescaling cycles
               ira = 0,    # detrended
               mk = 26,    # number of segments
               short = 0)  # rescale shortest gradient

res = data.frame(twitter = rownames(scores(ord)),
                 scores(ord), stringsAsFactors = FALSE) %>%
  left_join(select(d, twitter, party, followers, followers_sample, statuses, age),
            by = "twitter") %>%
  arrange(DCA1)

# correlations of covariates to each dimension

with(res, cor(DCA1, log10(followers)))
with(res, cor(DCA1, log10(followers_sample)))
with(res, cor(DCA1, log10(statuses)))
with(res, cor(DCA1, age))

with(res, cor(DCA2, log10(followers)))        # moderate
with(res, cor(DCA2, log10(followers_sample))) # moderate
with(res, cor(DCA2, log10(statuses)))
with(res, cor(DCA2, age))

# boxplots of first dimension for politicians

qplot(data = res, x = reorder(party, DCA1, median),
      y = DCA1, color = party, geom = "boxplot") +
  scale_color_manual("", breaks = names(colors), values = colors) +
  guides(color = FALSE) +
  theme_paper +
  labs(x = NULL, y = "First dimension of detrended CA\n")

ggsave("plots/dca_parties.png", width = 10, height = 5)
ggsave("plots/dca_parties.pdf", width = 10, height = 5)

# compare to Bayesian ideal points and to Chapel Hill expert scores

load("model/stage1_results-05.rda")

std01 <- function(x) { (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) }

pm = full_join(select(phis, twitter, phat), select(res, twitter, DCA1),
               by = "twitter") %>%
  left_join(select(d, twitter, party), by = "twitter") %>%
  mutate(phat = 10 * std01(phat), DCA1 = 10 * std01(DCA1)) %>%
  group_by(party) %>%
  summarise(n = n(),
            mu_phat = mean(phat, na.rm = TRUE), sd_phat = sd(phat, na.rm = TRUE),
            mu_dca = mean(DCA1, na.rm = TRUE), sd_dca = sd(DCA1, na.rm = TRUE)) %>%
  left_join(., p, by = "party")

pm$party = factor(pm$party, levels = pm$party[ order(pm$mu_dca) ])

qplot(data = pm, x = "A", xend = "A", y = mu_dca - 2 * sd_dca, yend = mu_dca + 2 * sd_dca,
      lty = "Detrended CA ideal point (Twitter)", geom = "segment") +
  geom_point(aes(y = mu_dca), size = 2) +
  geom_segment(aes(x = "B", xend = "B", y = mu_phat - 2 * sd_phat, yend = mu_phat + 2 * sd_phat,
                   lty = "MCMC ideal point (Twitter)")) +
  geom_point(aes(x = "B", y = mu_phat), size = 2) +
  geom_segment(aes(x = "C", xend = "C", y = chess - 2 * chess_sd, yend = chess + 2 * chess_sd,
                   lty = "Party position (Chapel Hill)")) +
  geom_point(aes(x = "C", y = chess), size = 2) +
  geom_text(aes(x = "B", y = -1.5, label = n)) +
  scale_color_manual("", values = colors) +
  scale_linetype_manual("", values = c("solid", "dashed", "dotted")) +
  facet_grid(. ~ party, scales = "free_x") +
  labs(x = NULL, y = "Mean score ± 2 standard deviations\n") +
  theme_paper +
  theme(legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")

ggsave("plots/dca_parties_chess.png", width = 10, height = 5)
ggsave("plots/dca_parties_chess.pdf", width = 10, height = 5)

qplot(data = res, x = DCA1, y = DCA2, color = party, alpha = I(.5)) +
  geom_text(data = res[ res$twitter %in% c("mlp_officiel", "lepenjm",
                                           "jlmelenchon", "nicolassarkozy",
                                           "emmacosse", "jccambadelis",
                                           "fhollande", "jlborloo",
                                           "bayrou"), ], aes(label = twitter)) +
  scale_color_manual("", breaks = names(colors), values = colors) +
  theme_paper +
  theme(legend.key = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1)) +
  labs(y = "Dimension 2\n",
       x = "\nDimension 1")

ggsave("plots/dca_2d.png", width = 10, height = 10)
ggsave("plots/dca_2d.pdf", width = 10, height = 10)

# correlation to Bayesian ideal points (politicians)

phis = left_join(phis, select(res, twitter, DCA1), by = "twitter")

with(phis, cor(phat, DCA1, use = "complete.obs")) # > .94
summary(lm(phat ~ DCA1, data = phis)) # beta = 1.7, R-squared = .89

# correlation to Bayesian ideal points (followers)

est = left_join(est, data.frame(id = rownames(scores(ord, "species")),
                                scores(ord, "species"), stringsAsFactors = FALSE),
                by = "id")

with(est, cor(phat, DCA1, use = "complete.obs")) # > .92
summary(lm(phat ~ DCA1, data = est)) # beta = 0.7, R-squared = .84

# plot both series

normalize = function(x) { (x - mean(x)) / sd(x) }

qplot(data = filter(est, !is.na(DCA1)),
      x = normalize(phat), y = normalize(DCA1), color = "Followers") +
  geom_point(data = filter(phis, !is.na(DCA1)),
             aes(x = normalize(phat), y = normalize(DCA1), color = party)) +
  scale_color_manual("", breaks = c(names(colors), "Followers"),
                     values = c(colors, "Followers" = "grey75")) +
  labs(y = "Detrended correspondence analysis\n",
       x = "\nBayesian Spatial Following Model") +
  theme_paper +
  theme(legend.key = element_blank())

ggsave("plots/dca_vs_bayesian.png", width = 7, height = 5)
ggsave("plots/dca_vs_bayesian.pdf", width = 7, height = 5)

rm(list = ls())
gc()
