# ==============================================================================
#
# A1_network.r -- visualize politicians' shared followers as network ties
#
# Plots and models the one-mode network of shared follower ties in a subsample
# of strongly connected politicians.
#
# The network file (model/network.rda) contains the full network, whereas the
# model results (model/ergm.rda) contain the subset network on which the ERGM
# is run (number of shared followers >= 1000 and tie strength > 0.5).
#
# ==============================================================================

library(ergm)
library(network)

library(GGally)
library(ggplot2)

library(dplyr)
library(readr)

d = read_csv("data/politicians.csv", col_types = list(id = col_character()))

# ==============================================================================
# BUILD ADJACENCY MATRIX
# ==============================================================================

if(!file.exists("model/network.rda")) {

  load("model/userlist.rda")

  # sanity check
  stopifnot(length(followers_m) == length(filesList))

  cat(date(), ": building adjacency matrix of",
      length(filesList), "politicians x", length(userlist), "users...\n")

  M = list()

  for(i in 1:length(followers_m))
    M[[i]] = as.numeric(userlist %in% followers_m[[i]])

  M = sapply(M, rbind)
  
  colnames(M) = gsub("followers/|\\.rda", "", filesList)
  rownames(M) = userlist

  cat(date(), ": collapsing two-mode to one-mode...\n")

  # collapse to one-mode (politicians)
  m = t(M) %*% M

  cat(date(), ": building weighted edge list...\n")

  e = data.frame()
  for(i in nrow(m):1) {
    e = rbind(e, data.frame(
      i = rownames(m)[i],
      j = colnames(m),
      n = m[i, ],
      w = m[i, ] / m[i , i],
      stringsAsFactors = FALSE,
      row.names = NULL
    ))
  }

  # remove self-loops and null ties
  e = filter(e, w < 1 & n != 0)

  cat(date(), ": building network object...\n")

  n = network(e[, 1:2 ], directed = TRUE)
  set.edge.attribute(n, "count", e[, 3])
  set.edge.attribute(n, "weight", e[, 4])

  tw = d$party
  names(tw) = d$twitter
  n %v% "party" = as.character(tw[ network.vertex.names(n) ])

  save(M, m, n, e, file = "model/network.rda")

  cat(date(), ": done.\n")

} else {

  load("model/network.rda")

}

# ==============================================================================
# PLOT ONE-MODE NETWORK
# ==============================================================================

colors = c(
  "FDG"   = "#E41A1C", # red
  "EELV"  = "#4DAF4A", # green
  "DVG"   = "#FB8072", # light red
  "PRG"   = "#FFFF33", # yellow
  "PS"    = "#F781BF", # pink
  "MODEM" = "#FDB462", # light orange
  "UDI"   = "#FF7F00", # orange
  "DVD"   = "#80B1D3", # light blue
  "UMP"   = "#377EB8", # blue
  "FN"    = "#A65628", # brown
  "IND"   = "#AAAAAA"  # light grey
)

# plot high tie strength networks
for(w in c(.66, .5)) {

  edges = e[ e$w > w, ]
  net = network(edges[, 1:2 ], directed = TRUE)
  set.edge.attribute(net, "count", edges[ , 3])
  set.edge.attribute(net, "weight", edges[ , 4])

  a = d$party
  names(a) = d$twitter
  net %v% "party" = as.character(a[ network.vertex.names(net) ])

  g = ggnet(net, node.group = net %v% "party", node.color = colors,
            segment.alpha = .5, size = 3, label.nodes = FALSE, label.size = 3) +
    scale_color_manual("", values = colors, limits = names(colors)) +
    theme(legend.key = element_blank())

  ggsave(paste0("plots/network_", w, "_", network.size(net), "_nodes.pdf"),
         g, width = 10, height = 9)

}

# ==============================================================================
# DEMO ERGM RESULTS
# ==============================================================================

E = ergm(net ~ edges +
           nodefactor("party", base = 6) + # baseline = independents
           nodematch("party") +
           mutual +
           gwesp(alpha = 1, fixed = TRUE) +
           gwdsp(alpha = 1, fixed = TRUE) +
           gwidegree(decay = 1, fixed = TRUE) +
           gwodegree(decay = 1, fixed = TRUE))

save(edges, w, E, net, file = "model/ergm.rda")
