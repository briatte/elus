#==============================================================================
#
# 09_activity.r -- model user activity by ideological positions
#
#==============================================================================

library(ggplot2)
library(reshape2)

library(dplyr)
library(readr)
library(stringr)

library(rstan)
library(texreg)

dir.create("tables", showWarnings = FALSE)

load('draft/stanfit/stanfit.rda')
load('tweets_april.rda')

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
load('draft/stanfit/matrix_selected.rda')
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
  theme_paper

# percentage of users who tweeted each week
total = n_distinct(t$id)
qplot(data = filter(t, year == 2015) %>%
        group_by(month, week) %>%
        summarise(p_users = n_distinct(id) / total),
      x = factor(week), y = p_users) +
  geom_hline(y = c(1/3, 1/2, 2/3), lty = "dashed") +
  facet_wrap(~ month, scales = "free_x") +
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
  geom_text(data = chrono, aes(y = 1.05 * n_tweets / n_users, label = event))

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

# merge 2: user tweet counts and ideal points with gender and creation date
u = read_csv("data/users.csv", col_types = list(id = col_character()))
s = left_join(s, select(u, id, age, gender, followers), by = "id")

s$position = 0
s$position[ s$phat < mean(s$phat) - sd(s$phat) ] = -1 # ~ 0 - 1 = -1
s$position[ s$phat > mean(s$phat) + sd(s$phat) ] = 1  # ~ 0 + 1 = +1
table(s$position)

# baselines
s$position = factor(s$position)
s$position = relevel(s$position, ref = 2)

#==============================================================================
# LINEAR REGRESSIONS (equivalent to Barberá and Rivero 2014, Table 3)
#==============================================================================

# ACTIVITY: tweets, followers, retweets and mentions
M1 = lm(log10(1 + n_tweets) ~ position + age + gender, data = s)
M2 = lm(log10(1 + followers) ~ log10(1 + n_tweets) + position + age + gender, data = s)
M3 = lm(log10(1 + n_retweets) ~ log10(1 + n_tweets) + position + age + gender, data = s)
M4 = lm(log10(1 + n_mentions) ~ log10(1 + n_tweets) + position + age + gender, data = s)

screenreg(list(M1, M2, M3, M4),
          custom.model.names = c("Statuses", "Followers", "Retweets", "Mentions"),
          custom.coef.names = c("Intercept", "Far-left", "Far-right", "Account age", "Male user", "Tweets"),
          include.rsquared = FALSE)

texreg(list(M1, M2, M3, M4),
       custom.model.names = c("Statuses", "Followers", "Retweets", "Mentions"),
       custom.coef.names = c("Intercept", "Far-left", "Far-right", "Account age", "Male user", "Tweets"),
       include.rsquared = FALSE, include.adjrs = FALSE, file = "tables/ols_activity.tex")

# MENTIONS: Greens, Socialists, Conservatives, Extreme-Right
P1 = lm(log10(1 + n_eelv) ~ log10(1 + n_mentions) + position + age + gender, data = s)
P2 = lm(log10(1 + n_ps) ~ log10(1 + n_mentions) + position + age + gender, data = s)
P3 = lm(log10(1 + n_ump) ~ log10(1 + n_mentions) + position + age + gender, data = s)
P4 = lm(log10(1 + n_fn) ~ log10(1 + n_mentions) + position + age + gender, data = s)

screenreg(list(P1, P2, P3, P4),
          custom.model.names = c("EELV", "PS", "UMP", "FN"),
          custom.coef.names = c("Intercept", "Far-left", "Far-right", "Account age", "Male user", "Tweets"),
          include.rsquared = FALSE)

texreg(list(P1, P2, P3, P4),
       custom.model.names = c("EELV", "PS", "UMP", "FN"),
       custom.coef.names = c("Intercept", "Far-left", "Far-right", "Account age", "Male user", "Tweets"),
       include.rsquared = FALSE, include.adjrs = FALSE, file = "tables/ols_mentions.tex")
