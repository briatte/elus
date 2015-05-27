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

library(Matrix)

d = read_csv("data/politicians.csv", col_types = list(id = col_character()))

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

  pb = txtProgressBar(min = 1, max = length(followers_m))

  for(i in 1:length(followers_m)) {

    M[[i]] = as.numeric(userlist %in% followers_m[[i]])
    setTxtProgressBar(pb, i)

  }

  M = sapply(M, rbind)
  M = Matrix(M)

  colnames(M) = gsub("followers/|\\.rda", "", filesList)
  rownames(M) = userlist

  cat(date(), ": collapsing two-mode to one-mode...\n")

  # collapse to primary mode (politicians, columns)
  m = t(M) %*% M

  cat(date(), ": building weighted edge list...\n")

  e = list()

  pb = txtProgressBar(min = 1, max = nrow(m))

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

stop('temp: done')

# ==============================================================================
# PLOT ONE-MODE NETWORK
# ==============================================================================

p = read_csv("data/parties.csv")
colors = p$color
names(colors) = p$party

# plot two networks at high tie strength
for(w in c(.66, .5)) {

  edges = e[ e$w > w, ]
  net = network(edges[, 1:2 ], directed = TRUE)
  set.edge.attribute(net, "count", edges[ , 3])
  set.edge.attribute(net, "weight", edges[ , 4])

  a = d$party
  names(a) = d$twitter
  net %v% "party" = as.character(a[ network.vertex.names(net) ])

  g = ggnet(net, node.group = net %v% "party", node.color = colors,
            segment.alpha = .5, size = 3, label.nodes = FALSE) +
    scale_color_manual("", values = colors, limits = names(colors)) +
    theme(text = element_text(size = 14),
          legend.key = element_blank(),
          legend.justification = c(1, 1), legend.position = c(1, 1))

  ggsave(paste0("plots/network_", gsub("\\.", "_", w), "_", network.size(net), "_nodes.pdf"),
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

# export
coefs = gsub("nodefactor.party.", "Main effect: ", names(coef(E)))
coefs[ coefs == "edges" ] = "Edges"
coefs[ coefs == "nodematch.party" ] = "Same party"
coefs[ coefs == "mutual" ] = "Mutual ties"
coefs[ coefs == "gwesp.fixed.1" ] = "GWESP"
coefs[ coefs == "gwdsp.fixed.1" ] = "GWDSP"
coefs[ coefs == "gwidegree" ] = "GW in-degree"
coefs[ coefs == "gwodegree" ] = "GW out-degree"

texreg(E, single.row = TRUE, custom.model.names = "ERGM", custom.coef.names = coefs,
       caption = paste("Exponential random graph model of the shared followers network.",
                       "Alpha and decay parameters set at 1 for the geometrically weighted terms."),
       file = "tables/ergm.tex", label = "tbl:ergm", booktabs = TRUE, dcolumn = TRUE)

save(edges, w, E, net, file = "model/ergm.rda")
