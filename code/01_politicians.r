#==============================================================================
#
# 01_politicians.r -- create the initial list of politicians' Twitter accounts
#
# The script scrapes politicians' details from the Elus 2.0 website, selects
# those with Twitter accounts, and recodes their party affiliations. The file
# data/politicians.csv is based on extensive checks of that initial list, on
# additional accounts retrieved from Regards Citoyens (see A0_additions.r), and
# on over 200 manual additions.
#
#==============================================================================

source('00_functions.r')

library(dplyr)
library(readr)
library(xtable)

dir.create("tables", showWarnings = FALSE)

if(!file.exists("data/politicians.csv")) {

  dir.create("politicians", showWarnings = FALSE)

  library(rvest)
  library(stringr)

  get_info <- function(h, x) {

    y = html_node(h, paste0("td#cbfv_", x))
    ifelse(is.null(y), NA, html_text(y))

  }

  # find pagination

  p = 1460
  if(is.na(p)) {

    p = html("http://www.elus20.fr/elus-web-facebook-twitter/") %>%
      html_node(".pagenav_end") %>%
      html_attr("href") %>%
      gsub("(.*)limitstart=(\\d+)", "\\2", .) %>%
      as.numeric

  }
  p = seq(0, p, by = 20)

  # scrape all pages

  d = data.frame()
  for(i in rev(p)) {

    u = paste0("http://www.elus20.fr/elus-web/userslist?limitstart=", i)
    f = paste0("politicians/elus-web-", i, ".html")

    cat(sprintf("%2.0f", which(p == i)), f)

    if(!file.exists(f))
      try(download.file(u, f, mode = "wb", quiet = TRUE))

    if(!file.info(f)$size) {

      cat(": failed\n")
      file.remove(f)

    } else {

      h = html(f) %>% html_node(xpath = "//table[@id='cbUserTable']/tbody")

      # img = html_nodes(h, xpath = "tr/td[1]//a/img") %>% html_attr("src")
      nom = html_nodes(h, xpath = "tr/td[1]//a/img") %>% html_attr("alt")
      url = html_nodes(h, xpath = "tr/td[1]//a") %>% html_attr("href")

      cat(":", length(url), "URLs\n")
      for(j in rev(url[ str_count(url, "/") == 3 ])) {

        f = paste0("politicians/elu-", gsub("http://www.elus20.fr/", "", j), ".html")
        cat(sprintf("%5.0f", which(url == j)), f, "\n")

        if(!file.exists(f))
          try(download.file(j, f, mode = "wb", quiet = TRUE))

        if(!file.info(f)$size) {

          # cat(": failed\n")
          file.remove(f)

        } else {

          h = html(f)
          # nom = h1.title span

          d = rbind(d, data.frame(
            nom = nom[ url == j ],
            parti = get_info(h, 55),
            parti2 = get_info(h, 89),
            titre = get_info(h, 76),
            mandats = get_info(h, 54),
            ville = get_info(h, 35),
            departement = get_info(h, 75),
            region = get_info(h, 74),
            # site = cbfv_59
            # site2 = cbfv_79
            twitter = get_info(h, 60),
            facebook = get_info(h, 101),
            wikipedia = get_info(h, 73),
            # youtube = cbfv_78
            # flickr = cbfv_80
            stringsAsFactors = FALSE
          ))

          # cat(":", tail(d$parti, 1), "\n")

        }

      }

    }

  }

  # mandate types
  table(unlist(strsplit(d$mandats[!is.na(d$twitter)], ", ")))

  d$twitter = tolower(gsub("http://(www\\.)?(fr\\.)?(t)?witter.com/|/$", "", d$twitter))
  d$twitter[ d$twitter == "" ] = NA # empty account (Pierre JOUVET)
  stopifnot(length(na.omit(d$twitter)) == n_distinct(na.omit(d$twitter)))

  cat(nrow(d), "politicians", sum(!is.na(d$twitter)), "using Twitter\n")

  # party abbreviations
  d$parti3 = d$parti
  d$parti3[ d$parti3 == "Lutte Ouvrière" ] = "LO"
  d$parti3[ d$parti3 == "Les Progressistes" ] = "LP" # Besson
  d$parti3[ d$parti3 == "Alliance Centriste" ] = "AC"
  d$parti3[ d$parti3 == "République Solidaire" ] = "RS" # Villepin
  d$parti3[ d$parti3 == "Solidarité et Progrès" ] = "SP" # Cheminade
  d$parti3[ d$parti3 == "Mouvement Indépendantiste Martiniquais" ] = "MAR-I"
  d$parti3[ d$parti3 == "Parti Progressiste Martiniquais" ] = "MAR-P"
  d$parti3[ d$parti3 == "Lyon Divers Droite" ] = "DVD"
  d$parti3[ d$parti3 == "Le Chêne" ] = "LC" # Gaullists
  d$parti3[ d$parti3 == "Mouvement Unitaire Progressiste" ] = "MUP" # Hue
  d$parti3[ d$parti3 == "Parti de Gauche" ] = "PG"
  d$parti3[ d$parti3 == "Parti Chrétien-Démocrate" ] = "PCD"
  d$parti3[ d$parti3 == "Mouvement Républicain et Citoyen" ] = "MRC" # Chevènement
  d$parti3[ d$parti3 == "Debout la République" ] = "DLR" # Dupont-Aignan
  d$parti3[ d$parti3 == "La Gauche Moderne" ] = "LGM" # Bockel
  d$parti3[ d$parti3 == "Mouvement pour la France" ] = "MPF" # de Villiers
  d$parti3[ d$parti3 == "Europe Ecologie - Les Verts" ] = "EELV"
  d$parti3[ d$parti3 == "Parti Radical" ] = "PRV" # Valoisiens
  d$parti3[ d$parti3 == "Parti Radical de Gauche" ] = "PRG"
  d$parti3[ d$parti3 == "Le Nouveau Centre" ] = "NC"
  d$parti3[ d$parti3 == "MoDem" ] = "MODEM"
  d$parti3[ d$parti3 == "sans parti" ] = "IND"

  # manual additions
  d$parti3[ d$nom %in% c("Arnaud RICHARD", "Meyer HABIB") ] = "UDI"

  # party recodings
  d$parti3[ d$parti3 == "NC"  ] = "UDI" # Nouveau Centre
  d$parti3[ d$parti3 == "LGM" ] = "UDI" # Bockel
  d$parti3[ d$parti3 == "AC"  ] = "UDI" # since 2012
  d$parti3[ d$parti3 == "PRV" ] = "UDI" # since 2012

  d$parti3[ d$parti3 == "MAR-I" ] = "DVG" # Martinique, independentist
  d$parti3[ d$parti3 == "MAR-P" ] = "DVG" # Martinique, progressist
  d$parti3[ d$parti3 == "MRC" ] = "DVG" # Chevènement
  d$parti3[ d$parti3 == "MUP" ] = "DVG" # Hue
  d$parti3[ d$parti3 == "NPA" ] = "DVG" # far-left
  d$parti3[ d$parti3 == "LO" ] = "DVG" # far-left

  d$parti3[ d$parti3 == "PG" ] = "FDG"  # Front de Gauche
  d$parti3[ d$parti3 == "PCF" ] = "FDG" # Front de Gauche

  d$parti3[ d$parti3 == "LP" ] = "UMP" # Besson
  d$parti3[ d$parti3 == "LC" ] = "UMP" # Gaullists
  d$parti3[ d$parti3 == "CPNT" ] = "UMP" # since 2010

  d$parti3[ d$parti3 == "PCD" ] = "DVD" # Poisson, Boutin
  d$parti3[ d$parti3 == "DLR" ] = "DVD" # Dupont-Aignan
  d$parti3[ d$parti3 == "CAP21" ] = "DVD" # Lepage
  d$parti3[ d$parti3 == "RS" ] = "DVD" # Villepin
  d$parti3[ d$parti3 == "MPF" ] = "DVD" # de Villiers
  d$parti3[ d$parti3 == "CNIP" ] = "DVD" # Bourdouleix

  d$parti3[ d$parti3 == "SP" ] = "IND" # Cheminade

  print(table(d$parti3, exclude = NULL))

  write_csv(select(d, name = nom, party = parti3, twitter, mandates = mandats, description = titre) %>%
              filter(!is.na(twitter)) %>%
              arrange(name),
            "data/politicians.csv")

}

d = read_csv("data/politicians.csv", col_types = list(id = col_character()))

# remove unused accounts from original spreadsheet
if("source" %in% names(d))
  d = filter(d, !grepl("Checking", source))

if("keep" %in% names(d))
  d = filter(d, keep == "yes")

# politicians sample
tbl = group_by(d, party) %>%
  summarise(n = n(), m = sum(gender == "m"), f = sum(gender == "f")) %>%
  mutate(ratio = round(m / f, 1), percent = round(100 * n / nrow(d), 1)) %>%
  arrange(-percent) %>%
  select(party, n, m, f, ratio, percent)

# export table
tbl = select(tbl, Party = party, Accounts = n, Males = m, Females = f, `Sample weight (%)` = percent)
tbl = xtable(tbl, digits = 1, caption = "Politicians sample, by party affiliation.", label = "tbl:sample")
print(tbl, booktabs = TRUE, include.rownames = FALSE, file = "tables/sample.tex")

# check for unique names
stopifnot(!is.na(d$name))
stopifnot(!duplicated(d$name))

# check for gender codes
stopifnot(!is.na(d$gender))
stopifnot(d$gender %in% c("m", "f"))

# check for Twitter accounts
stopifnot(!is.na(d$twitter))
stopifnot(!duplicated(d$twitter))

# check all parties are listed and colored
stopifnot(!is.na(d$party))
stopifnot(unique(d$party) %in% read_csv("data/parties.csv")$party)

cat("\n-", nrow(d), "politicians from", n_distinct(d$party) - 1, "parties,",
    sum(d$party == "IND"), "independents")

# gender
cat("\n-", round(100 * prop.table(table(d$gender))["f"], 1), "% females, ")
cat(round(sum(d$gender == "m") / sum(d$gender == "f"), 1), "males for 1 female\n")

# mandates
m = table(unlist(strsplit(d$mandates, ", ")))
m[ m > 50 ]

# identify MPs, Senator, MEPs and non-parliamentarians
d$is_mp  = grepl("D(é|e)puté(e)?(,|$)|Assemblée Nationale", d$mandates)
d$is_sen = grepl("S(e|é)nat(eur|rice)?", d$mandates)
d$is_mep = grepl("europ", d$mandates)
d$is_loc = !grepl("D(é|e)puté(e)?|Assemblée Nat|S(e|é)nat(eur|rice)?", d$mandates) &
  d$mandates != "Sans mandat électif courant"
d$is_else = (d$mandates == "Sans mandat électif courant")

# no overlap between categories
stopifnot(d$is_mp + d$is_sen + d$is_mep + d$is_loc + d$is_else == 1)

# collapse in a single variable
d$type = NA
d$type[ d$is_mp ] = "MP"
d$type[ d$is_sen ] = "Senator"
d$type[ d$is_mep ] = "MEP"
d$type[ d$is_loc ] = "Local"
d$type[ d$is_else ] = "Else"
d$type = factor(d$type, levels = c("MP", "Senator", "MEP", "Local", "Else"))

# party/mandate breakdown
table(d$party, d$type)

# party/gender breakdowns
table(d$gender, d$type)

# MPs
cat("-",
    sum(d$type == "MP"), "MPs",
    sum(d$type == "MP" & d$gender == "m"), "male",
    sum(d$type == "MP" & d$gender == "f"), "female",
    round(100 * sum(d$type == "MP") / 577, 1), "% of chamber\n")

# Senators
cat("-",
    sum(d$type == "Senator"), "Senators",
    sum(d$type == "Senator" & d$gender == "m"), "male",
    sum(d$type == "Senator" & d$gender == "f"), "female",
    round(100 * sum(d$type == "Senator") / 348, 1), "% of chamber\n")

# MEPs
cat("-",
    sum(d$type == "MEP"), "MEPs",
    sum(d$type == "MEP" & d$gender == "m"), "male",
    sum(d$type == "MEP" & d$gender == "f"), "female",
    round(100 * sum(d$type == "MEP") / 74, 1), "% of approportionment\n")

# else
cat("-", sum(d$type == "Local"), "strictly local,",
    sum(d$type == "Else"), "non-elected politicians\n")

if(!file.exists("model/politicians.rda")) {

  politicians = list()
  save(politicians, file = "model/politicians.rda")

}

load("model/politicians.rda")

j = unique(d$twitter)
j = j[ !j %in% names(politicians) ] # skip closed account

for(i in rev(j)) {

  u = try(user <- getUsers(screen_name = i, oauth_folder = "credentials"))

  if(!"try-error" %in% class(u)) {

    cat(which(j == i), i, "\n")
    politicians[[i]] = user
    save(politicians, file = "model/politicians.rda")

  } else {

    cat(which(j == i), i, "failed\n")

  }

}

p = data.frame(
  twitter = names(politicians),
  id = get_nfo(politicians, "id_str"),
  statuses = get_nfo(politicians, "statuses_count"),
  followers = get_nfo(politicians, "followers_count"),
  created = get_nfo(politicians, "created_at"),
  last_tweeted = get_status(politicians, "created_at"),
  stringsAsFactors = FALSE
)

# account creation time, in days
p$age = as.integer(Sys.time() - as.POSIXct(p$created, format = "%a %b %d %H:%M:%S %z %Y"))

# account creation year
p$created = as.integer(substring(p$created, first = nchar(p$created) - 3))

# date of last tweet (when available)
p$last_tweeted = as.POSIXct(p$last_tweeted, format = "%a %b %d %H:%M:%S %z %Y")
p$last_tweeted = as.Date(p$last_tweeted)

write_csv(left_join(select(d, name, gender, party, twitter, mandates),
                    p, by = "twitter") %>%
            arrange(twitter),
          "data/politicians.csv")

rm(list = ls())
gc()
