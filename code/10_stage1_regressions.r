#==============================================================================
#
# 09_activity.r -- model user activity and mentions by ideological positions
#
# This scripts produces ordinary least squares regressions of Twitter activity
# as a function of estimated ideology. See also the results obtained by Pablo
# Barberá and Gonzalo Rivero in their paper "Understanding the political
# representativeness of Twitter users", Social Science Computer Review (2014).
#
#==============================================================================

library(ggplot2)
library(reshape2)

library(dplyr)
library(readr)

library(lubridate)
library(stringr)

library(rstan)

library(MASS)
library(texreg)

dir.create("tables", showWarnings = FALSE)

load('draft-models/stanfit-05/stanfit.rda')
load('draft-models/stanfit-05/matrix_selected.rda')

load('tweets-04.rda')

theme_paper =  theme_bw(14) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        strip.text = element_text(size = rel(1)),
        strip.background = element_rect(fill = "grey90"),
        legend.text = element_text(size = rel(1)),
        axis.ticks.x = element_blank())

#==============================================================================
# ASSESS COVERAGE
#==============================================================================

# sanity check: all users should appear in initial matrix
table(unique(t$id) %in% rownames(y))

# month-week breakdown of tweets in year 2015
t$week = week(t$date)
t$month = month(t$date, label = TRUE)
t$year = year(t$date)

# number of users who tweeted each week
qplot(data = filter(t, year == 2015) %>%
        group_by(month, week) %>%
        summarise(n_users = n_distinct(id)),
      x = factor(week), y = n_users) +
  facet_wrap(~ month, scales = "free_x") +
  labs(y = "Number of Twitter users who tweeted that week\n", x = "\nWeek") +
  theme_paper

# percentage of users who tweeted each week
total = n_distinct(t$id)
qplot(data = filter(t, year == 2015) %>%
        group_by(month, week) %>%
        summarise(p_users = n_distinct(id) / total),
      x = factor(week), y = p_users) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_hline(y = c(1/3, 1/2, 2/3), lty = "dashed") +
  facet_wrap(~ month, scales = "free_x") +
  labs(y = "Percentage of Twitter users who tweeted\n", x = "\nWeek") +
  theme_paper

# sample: keep year 2015 weeks 1-16
s = filter(t, year == 2015 & week < 17)

# 2.8 million tweets
nrow(s)

# 17,161 users
n_distinct(s$id)

# percentage of total ~ 95%
n_distinct(s$id) / nrow(y)

#==============================================================================
# USER ACTIVITY
#==============================================================================

# summarise by date
tt = group_by(s, date) %>% summarise(n_users = n_distinct(id), n_tweets = n())

# events of interest
chrono = data.frame(
  date = as.Date(c("2015-01-07", "2015-03-22", "2015-03-29")),
  event = c("Charlie Hebdo", "Round 1", "Round 2")
)
chrono$n_users = tt$n_users[ tt$date %in% chrono$date ]
chrono$n_tweets = tt$n_tweets[ tt$date %in% chrono$date ]

# # number of users
# qplot(data = tt, x = date, y = n_users, geom = "line") +
#   geom_point(data = chrono)

# # number of tweets
# qplot(data = tt, x = date, y = n_tweets, geom = "line") +
#   geom_point(data = chrono)

# number of tweets per user
qplot(data = tt, x = date, y = n_tweets / n_users, geom = "line") +
  geom_point(data = chrono) +
  geom_text(data = chrono, aes(y = 1.05 * n_tweets / n_users, label = event)) +
  labs(y = "Number of tweets per user\n", x = "\nMonth") +
  theme_paper

#==============================================================================
# POLITICIAN MENTIONS
#==============================================================================

# number of politicians mentioned, by party
s$n_ps    = 1 * (nchar(s$mentions_ps) > 0)    + str_count(s$mentions_ps, ",")
s$n_ump   = 1 * (nchar(s$mentions_ump) > 0)   + str_count(s$mentions_ump, ",")
s$n_fn    = 1 * (nchar(s$mentions_fn) > 0)    + str_count(s$mentions_fn, ",")
s$n_fdg   = 1 * (nchar(s$mentions_fdg) > 0)   + str_count(s$mentions_fdg, ",")
s$n_modem = 1 * (nchar(s$mentions_modem) > 0) + str_count(s$mentions_modem, ",")
s$n_eelv  = 1 * (nchar(s$mentions_eelv) > 0)  + str_count(s$mentions_eelv, ",")
s$n_prg   = 1 * (nchar(s$mentions_prg) > 0)   + str_count(s$mentions_prg, ",")
s$n_udi   = 1 * (nchar(s$mentions_udi) > 0)   + str_count(s$mentions_udi, ",")
s$n_dvd   = 1 * (nchar(s$mentions_dvd) > 0)   + str_count(s$mentions_dvd, ",")
s$n_dvg   = 1 * (nchar(s$mentions_dvg) > 0)   + str_count(s$mentions_dvg, ",")
s$n_ind   = 1 * (nchar(s$mentions_ind) > 0)   + str_count(s$mentions_ind, ",")

# same classification as starting values
s$n_left = s$n_ps + s$n_fdg + s$n_eelv + s$n_prg + s$n_dvg
s$n_right = s$n_ump + s$n_fn + s$n_modem + s$n_udi + s$n_dvd
s$n_total = s$n_left + s$n_right + s$n_ind

# ~ 33% users do not mention any politician in their tweets over the observed period
summarise(group_by(s, id), n_total = sum(n_total > 0) / n()) %>%
  filter(n_total == 0) %>%
  nrow(.) / n_distinct(s$id)

#==============================================================================
# MERGE USER INFO TO MENTIONS, REPLIES, RETWEETS AND IDEAL POINT ESTIMATES
#==============================================================================

est = summary(stan.fit, permute = FALSE)

# sanity check: same number of thetas in model than rows in matrix
thetas = dimnames(est$c_summary)[[1]]
thetas = thetas[ grepl("theta", thetas) ]
stopifnot(length(thetas) == nrow(y))

est = data.frame(id = names(stan.fit@stan_args[[1]]$init$beta),
                 phat = est$c_summary[ paste0("theta[", 1:length(thetas), "]"), 1, 1 ],
                 stringsAsFactors = FALSE)

# merge 1: user tweet counts with ideal points
s = group_by(s, id) %>%
  summarise(n_tweets = n(),
            n_replies = sum(reply),
            n_retweets = sum(retweet),
            n_mentions = sum(n_total),
            n_left = sum(n_left),
            n_right = sum(n_right),
            n_ps = sum(n_ps),
            n_ump = sum(n_ump),
            n_fn = sum(n_fn),
            n_fdg = sum(n_fdg),
            n_modem = sum(n_modem),
            n_eelv = sum(n_eelv),
            n_prg = sum(n_prg),
            n_udi = sum(n_udi),
            n_dvd = sum(n_dvd),
            n_dvg = sum(n_dvg),
            n_ind = sum(n_ind)) %>%
  inner_join(., est, by = "id")

# colors of partisan mentions
p = read_csv("data/parties.csv")
colors = p$color
names(colors) = p$party

# fractions of partisan mentions
p = filter(s, n_mentions > 0) %>%
  mutate(n_fdg = n_fdg / n_mentions,
         n_eelv = n_eelv / n_mentions,
         n_ps = n_ps / n_mentions,
         n_prg = n_prg / n_mentions,
         n_dvg = n_dvg / n_mentions,
         n_dvd = n_dvd / n_mentions,
         n_modem = n_modem / n_mentions,
         n_udi = n_udi / n_mentions,
         n_ump = n_ump / n_mentions,
         n_fn = n_fn / n_mentions,
         n_ind = n_ind / n_mentions,
         ntile = floor(phat / sd(phat))) %>%
  group_by(ntile) %>%
  summarise(n = n(),
            FDG = mean(n_fdg), EELV = mean(n_eelv), PS = mean(n_ps),
            PRG = mean(n_prg), DVG = mean(n_dvg), DVD = mean(n_dvd),
            UMP = mean(n_ump), MODEM = mean(n_modem), UDI = mean(n_udi),
            FN = mean(n_fn), IND = mean(n_ind))

qplot(data = melt(p, c("ntile", "n")), x = ntile, y = value,
      color = variable, fill = variable, geom = "area") +
  scale_fill_manual("", values = colors) +
  scale_color_manual("", values = colors) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Partisan mentions\n",
       x = "\nStandardized Twitter-based ideal point") +
  theme_paper

ggsave("plots/mentions_party.pdf", width = 10, height = 5)
ggsave("plots/mentions_party.png", width = 10, height = 5)

# merge 2: user tweet counts and ideal points with gender and creation date
u = read_csv("data/users.csv", col_types = list(id = col_character()))
s = left_join(s, select(u, id, age, gender, foll owers), by = "id")

s$position = 0
s$position[ s$phat < mean(s$phat) - sd(s$phat) ] = -1 # ~ 0 - 1 = -1
s$position[ s$phat > mean(s$phat) + sd(s$phat) ] = 1  # ~ 0 + 1 = +1
table(s$position)

ggplot(data = select(s, id, position, gender,
                     `Left-wing partisan mentions` =  n_left,
                     `Right-wing partisan mentions` = n_right,
                     `Total partisan mentions` = n_mentions) %>%
         mutate(gender = factor(gender, levels = c("f", "m"),
                                labels = c("Female", "Male"))) %>%
         melt(c("id", "position", "gender")) %>%
         filter(!is.na(gender))) +
  aes(x = factor(position), y = 1 + value) +
  scale_x_discrete(labels = c("Left", "Centre", "Right")) +
  scale_y_log10() +
  geom_boxplot(outlier.size = 1, aes(fill = gender), position = "dodge") +
  scale_fill_brewer("", palette = "Greys") +
  facet_grid(. ~ variable) +
  labs(y = "Number of partisan mentions per follower\n",
       x = "\nIdeological placement of follower") +
  theme_paper +
  theme(legend.position = "bottom")

ggsave("plots/mentions_n.pdf", width = 10, height = 5)
ggsave("plots/mentions_n.png", width = 10, height = 5)

#==============================================================================
# LINEAR REGRESSIONS (equivalent to Barberá and Rivero 2014, Table 3)
#==============================================================================

# baselines
s$position = factor(s$position)
s$position = relevel(s$position, ref = 2)

# comparisons of means
tapply(s$n_tweets, s$position, mean)
tapply(s$followers, s$position, mean)
tapply(s$n_retweets, s$position, mean)
tapply(s$n_mentions, s$position, mean)

# ACTIVITY: tweets, followers, retweets and mentions
M1 = lm(log10(1 + n_tweets)   ~ position + age + gender, data = s)
M2 = lm(log10(1 + followers)  ~ log10(1 + n_tweets) + position + age + gender, data = s)
M3 = lm(log10(1 + n_retweets) ~ log10(1 + n_tweets) + position + age + gender, data = s)
M4 = lm(log10(1 + n_mentions) ~ log10(1 + n_tweets) + position + age + gender, data = s)

# check: run on full sample without the gender variable
F1 = lm(log10(1 + n_tweets)   ~ position + age , data = s)
F2 = lm(log10(1 + followers)  ~ log10(1 + n_tweets) + position + age, data = s)
F3 = lm(log10(1 + n_retweets) ~ log10(1 + n_tweets) + position + age, data = s)
F4 = lm(log10(1 + n_mentions) ~ log10(1 + n_tweets) + position + age, data = s)

screenreg(list(M1, F1, M2, F2, M3, F3, M4, F4),
          custom.model.names = c("S", "S", "F", "F", "R", "R", "M", "M"),
          custom.coef.names = c("Intercept", "Far-left", "Far-right",
                                "Account age", "Male user", "Tweets"),
          include.rsquared = FALSE)

texreg(list(M1, M2, M3, M4),
       custom.model.names = c("Tweets", "Followers", "Retweets", "Mentions"),
       custom.coef.names = c("Intercept", "Far-left", "Far-right",
                             "Account age", "Male user", "Tweets (log-10)"),
       include.rsquared = FALSE, include.adjrs = FALSE,
       caption = paste("Ordinary least squares regression estimates of Twitter",
                       "activity by ideological placement, with standard errors in brackets.",
                       "All dependent variables are logged at base 10."),
       label = "tbl:ols_activity",
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE,
       file = "tables/ols_activity.tex")

#==============================================================================
# NEGATIVE BINOMIAL REGRESSIONS
#==============================================================================

# DV = count of mentions of Greens, Socialists, Conservatives and Extreme-Right

# Using continuous estimated ideal points
P1 = glm.nb(n_eelv ~ log10(1 + n_mentions) + phat + age + gender, data = s)
P2 = glm.nb(n_ps ~ log10(1 + n_mentions) + phat + age + gender, data = s)
P3 = glm.nb(n_ump ~ log10(1 + n_mentions) + phat + age + gender, data = s)
P4 = glm.nb(n_fn ~ log10(1 + n_mentions) + phat + age + gender, data = s)

# check: run on full sample without the gender variable
F1 = glm.nb(n_eelv ~ log10(1 + n_mentions) + phat + age, data = s)
F2 = glm.nb(n_ps ~ log10(1 + n_mentions) + phat + age, data = s)
F3 = glm.nb(n_ump ~ log10(1 + n_mentions) + phat + age, data = s)
F4 = glm.nb(n_fn ~ log10(1 + n_mentions) + phat + age, data = s)

screenreg(list(P1, F1, P2, F2, P3, F3, P4, F4),
          custom.coef.names = c("Intercept", "Mentions", "Left-right placement",
                                "Account age", "Male user"),
          include.rsquared = FALSE)

# predicted mean number of mentions
sim = data.frame(n_mentions = mean(s$n_mentions), age = mean(s$age), gender = c("m", "f"))
sim = rbind(
  cbind(sim[1, ], data.frame(phat = seq(min(s$phat), max(s$phat), .1)), row.names = NULL),
  cbind(sim[2, ], data.frame(phat = seq(min(s$phat), max(s$phat), .1)), row.names = NULL)
)

sim = rbind(
  cbind(sim, response = "EELV", predict(P1, sim, type = "link", se.fit = TRUE), row.names = NULL),
  cbind(sim, response = "PS", predict(P2, sim, type = "link", se.fit = TRUE), row.names = NULL),
  cbind(sim, response = "UMP", predict(P3, sim, type = "link", se.fit = TRUE), row.names = NULL),
  cbind(sim, response = "FN", predict(P4, sim, type = "link", se.fit = TRUE), row.names = NULL)
)

sim$gender  = ifelse(sim$gender == "f", "Females", "Males")

ggplot(data = sim, aes(x = phat, y = exp(fit))) +
  geom_ribbon(aes(ymin = exp(fit - 1.96 * se.fit),
                  ymax = exp(fit + 1.96 * se.fit)),
              fill = "grey85") +
  geom_line() +
  scale_fill_brewer(palette = "Greys") +
  facet_grid(gender ~ response) +
  labs(y = "Predicted number of partisan mentions\n",
       x = "\nTwitter-based estimated ideal point") +
  theme_paper

ggsave("plots/mentions_predicted.pdf", width = 10, height = 6)
ggsave("plots/mentions_predicted.png", width = 10, height = 6)

# Using ideological points
P1 = glm.nb(n_eelv ~ log10(1 + n_mentions) + position + age + gender, data = s)
P2 = glm.nb(n_ps ~ log10(1 + n_mentions) + position + age + gender, data = s)
P3 = glm.nb(n_ump ~ log10(1 + n_mentions) + position + age + gender, data = s)
P4 = glm.nb(n_fn ~ log10(1 + n_mentions) + position + age + gender, data = s)

# check: run on full sample without the gender variable
F1 = glm.nb(n_eelv ~ log10(1 + n_mentions) + position + age, data = s)
F2 = glm.nb(n_ps ~ log10(1 + n_mentions) + position + age, data = s)
F3 = glm.nb(n_ump ~ log10(1 + n_mentions) + position + age, data = s)
F4 = glm.nb(n_fn ~ log10(1 + n_mentions) + position + age, data = s)

screenreg(list(P1, F1, P2, F2, P3, F3, P4, F4),
          custom.coef.names = c("Intercept", "Mentions", "Far-left", "Far-right",
                                "Account age", "Male user"),
          include.rsquared = FALSE)

texreg(list(P1, P2, P3, P4),
       custom.model.names = c("EELV", "PS", "UMP", "FN"),
       custom.coef.names = c("Intercept", "Mentions (log-10)", "Far-left", "Far-right",
                             "Account age", "Male user"),
       include.rsquared = FALSE, include.adjrs = FALSE,
       caption = paste("Negative binomial regression estimates of Twitter",
                       "mentions by ideological placement, with standard errors in brackets.",
                       "All dependent variables are counts of partisan mentions."),
       label = "tbl:nb_mentions",
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE,
       file = "tables/nb_mentions.tex")
