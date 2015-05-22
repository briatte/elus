# ==============================================================================
#
# A2_elections.r -- convert Excel files for presidential and local elections
#
# The original files come from data.gouv.fr and were released by the Ministry of
# the Interior. The original Excel files should be in the "draft" folder. The
# converted files are saved to the "data" folder.
#
# ==============================================================================

library(dplyr)
library(readxl)

library(stringr)
library(tidyr)

#
# Présidentielle 2012 T2
# https://www.data.gouv.fr/fr/datasets/election-presidentielle-2012-resultats-572124/
#

elec = read_excel("draft/presidentielle2012_t2.xls", sheet = "Départements T2")
elec = elec[, c(1, 19, 25) ]
names(elec) = c("id", "p_hollande", "p_sarkozy")

# correct the departement numbers when they include zeros
elec$id = gsub("\\.0+", "", elec$id)

# correct the departement numbers to match the map data
m = nchar(elec$id) < 2
elec$id[ m ] = paste0("0", elec$id[ m ])

write_csv(filter(elec, !is.na(id)), "data/elec_pres2012_2.csv")

#
# Départementale 2015 T2
# https://www.data.gouv.fr/fr/datasets/elections-departementales-2015-resultats-tour-2/
#

elec = read_excel("draft/Dep_15_Resultats_T2_c.xlsx", sheet = 3)
elec = data.frame(elec[, which(grepl("Code|Voix/Exp", names(elec))) ])

elec = elec %>%
  rename(id = Code.du.département,
         Code.Nuance.0 = Code.Nuance,
         X..Voix.Exp.0 = X..Voix.Exp) %>%
  filter(!is.na(id)) %>%
  gather(col, value, -id) %>%
  mutate(col = str_replace(col, "Code.Nuance", "list"),
         col = str_replace(col, "X..Voix.Exp", "vote")) %>%
  separate(col, into = c("var", "side"), sep = "\\.") %>%
  spread(var, value, convert = TRUE) %>%
  arrange(id) %>%
  na.omit

elec$list = gsub("BC-", "", elec$list)
table(elec$list)

elec$side = NA
elec$side[ elec$list %in% c("COM", "DVG", "FG", "PG", "RDG", "SOC", "UG", "VEC") ] = "Left"
elec$side[ elec$list %in% c("DLF", "DVD", "EXD", "FN", "MDM", "UC", "UD", "UDI", "UMP") ] = "Right"

# correct the departement numbers to match the map data
m = nchar(elec$id) < 2
elec$id[ m ] = paste0("0", elec$id[ m ])

# elec %>%
#   group_by(id) %>%
#   top_n(n=1)

write_csv(elec, "data/elec_dept2015_2.csv")

#
# Départementales 2015 T1
# https://www.data.gouv.fr/fr/datasets/elections-departementales-2015-resultats-tour-1/
#

elec = read_excel("draft/Dep_15_Resultats_T1_c.xlsx", sheet = 3, skip = 1)
elec = data.frame(elec[, which(grepl("Code|Voix/Exp", names(elec))) ])

elec = elec %>%
  rename(id = Code.du.département,
         Code.Nuance.0 = Code.Nuance,
         X..Voix.Exp.0 = X..Voix.Exp) %>%
  filter(!is.na(id)) %>%
  gather(col, value, -id) %>%
  mutate(col = str_replace(col, "Code.Nuance", "list"),
         col = str_replace(col, "X..Voix.Exp", "vote")) %>%
  separate(col, into = c("var", "side"), sep = "\\.") %>%
  spread(var, value, convert = TRUE) %>%
  arrange(id) %>%
  na.omit

elec$list = gsub("BC-", "", elec$list)
table(elec$list)

elec$side = NA
elec$side[ elec$list %in% c("COM", "DVG", "FG", "PG", "RDG", "SOC", "UG", "VEC") ] = "Left"
elec$side[ elec$list %in% c("DLF", "DVD", "EXD", "FN", "MDM", "UC", "UD", "UDI", "UMP") ] = "Right"

# correct the departement numbers to match the map data
m = nchar(elec$id) < 2
elec$id[ m ] = paste0("0", elec$id[ m ])

# elec %>%
#   group_by(id) %>%
#   top_n(n=1)

write_csv(elec, "data/elec_dept2015_1.csv")


#
# Cantonales 2011 T2
# https://www.data.gouv.fr/fr/datasets/elections-cantonales-2011-resultats-572053/
#

elec = read_excel("draft/cantonales2011.xls", sheet = 5)
elec = data.frame(elec[, which(grepl("Code|Voix/Exp", names(elec))) ])

elec = elec %>%
  rename(id = Code.du.département,
         Code.Nuance.0 = Code.Nuance,
         X..Voix.Exp.0 = X..Voix.Exp) %>%
  filter(!is.na(id)) %>%
  gather(col, value, -id) %>%
  mutate(col = str_replace(col, "Code.Nuance", "list"),
         col = str_replace(col, "X..Voix.Exp", "vote")) %>%
  separate(col, into = c("var", "side"), sep = "\\.") %>%
  spread(var, value, convert = TRUE) %>%
  arrange(id) %>%
  na.omit %>%
  filter(vote > 0)

table(elec$list)

elec$side = NA
elec$side[ elec$list %in% c("COM", "DVG", "ECO", "EXG", "PG", "RDG", "SOC", "VEC") ] = "Left"
elec$side[ elec$list %in% c("DVD", "EXD", "FN", "M", "M-NC", "MODM", "UMP") ] = "Right"

# correct the departement numbers when they include zeros
elec$id = gsub("\\.0+", "", elec$id)

# correct the departement numbers to match the map data
m = nchar(elec$id) < 2
elec$id[ m ] = paste0("0", elec$id[ m ])

write_csv(elec, "data/elec_dept2011_2.csv")

#
# Cantonales 2011 T1 (same file, different sheet)
# https://www.data.gouv.fr/fr/datasets/elections-cantonales-2011-resultats-572053/
#

elec = read_excel("draft/cantonales2011.xls", sheet = 4)
elec = data.frame(elec[, which(grepl("Code|Voix/Exp", names(elec))) ])

elec = elec %>%
  rename(id = Code.du.département,
         Code.Nuance.0 = Code.Nuance,
         X..Voix.Exp.0 = X..Voix.Exp) %>%
  filter(!is.na(id)) %>%
  gather(col, value, -id) %>%
  mutate(col = str_replace(col, "Code.Nuance", "list"),
         col = str_replace(col, "X..Voix.Exp", "vote")) %>%
  separate(col, into = c("var", "side"), sep = "\\.") %>%
  spread(var, value, convert = TRUE) %>%
  arrange(id) %>%
  na.omit %>%
  filter(vote > 0)

table(elec$list)

elec$side = NA
elec$side[ elec$list %in% c("COM", "DVG", "ECO", "EXG", "PG", "RDG", "SOC", "VEC") ] = "Left"
elec$side[ elec$list %in% c("DVD", "EXD", "FN", "M", "M-NC", "MODM", "UMP") ] = "Right"

# correct the departement numbers when they include zeros
elec$id = gsub("\\.0+", "", elec$id)

# correct the departement numbers to match the map data
m = nchar(elec$id) < 2
elec$id[ m ] = paste0("0", elec$id[ m ])

write_csv(elec, "data/elec_dept2011_1.csv")
