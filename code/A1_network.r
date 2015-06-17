# ==============================================================================
#
# A1_network.r -- visualize politicians' shared followers as network ties
#
# Plots and models the one-mode network of shared follower ties in a subsample
# of strongly connected politicians.
#
# The network file (model/network.rda) contains the full network, whereas the
# model results (model/ergm.rda) contain the subset network on which the ERGM
# is run (tie strength > 0.5).
#
# ==============================================================================

library(dplyr)
library(readr)

library(network)

library(GGally)
library(ggplot2)

library(Matrix)

# ==============================================================================
# BUILD ADJACENCY MATRIX
# ==============================================================================

if(!file.exists("model/network.rda")) {

  load("model/userlist.rda")

  cat(date(), ": working on", length(filesList), "politicians x", length(userlist), "users\n")

  # sanity check
  stopifnot(length(followers_m) == length(filesList))

  cat(date(), ": building adjacency matrix...\n")

  M = list()

  pb = txtProgressBar(min = 1, max = length(followers_m), style = 3)

  for(i in 1:length(followers_m)) {

    M[[i]] = as.numeric(userlist %in% followers_m[[i]])
    setTxtProgressBar(pb, i)

  }

  cat("\n", date(), ": binding rows and converting...\n")

  M = sapply(M, rbind)
  M = Matrix(M)

  colnames(M) = gsub("followers/|\\.rda", "", filesList)
  rownames(M) = userlist

  cat(date(), ": collapsing two-mode to one-mode...\n")

  # collapse to primary mode (politicians, columns)
  m = t(M) %*% M

  cat(date(), ": building weighted edge list...\n")

  e = list()

  pb = txtProgressBar(min = 1, max = nrow(m), style = 3)

  for(i in 1:nrow(m)) {

    e[[i]] = data_frame(
      i = rownames(m)[i],
      j = colnames(m),
      n = m[i, ],
      w = m[i, ] / m[i , i]
    )

    setTxtProgressBar(pb, i)

  }

  e = bind_rows(e)

  # remove self-loops and null ties
  e = filter(e, w < 1 & n != 0) %>%
    arrange(i, j)

  # save network constructors
  save(M, m, e, file = "model/network.rda")

  cat(date(), ": saved.\n")

  # save memory
  rm(list = ls())
  gc()

}

load("model/network.rda")
rm(M) # not used below
rm(m) # not used below

d = read_csv("data/politicians.csv", col_types = list(id = col_character()))
party = d$party
names(party) = d$twitter

p = read_csv("data/parties.csv")
colors = p$color
names(colors) = p$party

# ==============================================================================
# PLOT ONE-MODE NETWORK
# ==============================================================================

# plot two networks at high tie strength
for(w in c(.66, .5)) {

  cat(date(), ": building network at w >", w, "...\n")

  edges = e[ e$w > w, ]

  net = network(edges[, 1:2 ], directed = TRUE)
  cat("Dimensions", network.size(net), "nodes,", network.edgecount(net), "edges\n")

  set.edge.attribute(net, "count", edges$n)
  cat("Mean edge count:", round(mean(edges$n), 2),
      "min:", min(edges$n),
      "max:", max(edges$n), "\n")

  set.edge.attribute(net, "weight", edges$w)
  cat("Mean edge weight:", round(mean(edges$w), 2),
      "min:", round(min(edges$w), 2),
      "max:", round(max(edges$w), 2), "\n")

  net %v% "party" = as.character(party[ network.vertex.names(net) ])

  g = ggnet(net, node.group = net %v% "party", node.color = colors,
            segment.alpha = .5, size = 3, label.nodes = FALSE) +
    scale_color_manual("", values = colors, breaks = names(colors)) +
    theme(text = element_text(size = 14),
          legend.key = element_blank(),
          legend.justification = c(1, 1), legend.position = c(1, 1))

  ggsave(paste0("plots/network_", gsub("\\.", "_", w), "_", network.size(net), "_nodes.pdf"),
         g, width = 10, height = 9)

}

rm(list = ls())
gc()
