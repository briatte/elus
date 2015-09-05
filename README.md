Code to estimate Bayesian ideal points for French politicians from Twitter data. Forked from [pablobarbera](https://github.com/pablobarbera)'s code at [`twitter_ideology`](https://github.com/pablobarbera/twitter_ideology/). Edits by [briatte](https://github.com/briatte) and [3wen](https://github.com/3wen). 

- Read the [draft paper](http://f.briatte.org/research/spq-paper.pdf) and [its appendix](http://f.briatte.org/research/spq-appendix.pdf)
- See the [HOWTO](HOWTO.md) file for details on how to run the code
- Feel free to [suggest improvements or report bugs](https://github.com/briatte/elus/issues)

# SOURCES

- Many Twitter accounts from [Elus 2.0](http://www.elus20.fr/elus-web-facebook-twitter/), [eTerritoire](http://www.eterritoire.fr/blog/category/statistiques-twitter/) and [Regards Citoyens](http://www.regardscitoyens.org/).
- Election results from Ministère de l'Intérieur via [Data.gouv.fr](https://www.data.gouv.fr).
- French administrative units from Insee via [Wikipedia Francophone](https://fr.wikipedia.org/wiki/).
- Geocodes from [Google Maps](https://developers.google.com/maps/).
- GEOFLA shapefiles from [IGN](http://professionnels.ign.fr/geofla) (not included).
- Government composition by Premier ministre via [Data.gouv.fr](https://www.data.gouv.fr).
- List of French first names by [Mike Campbell and Boris New](http://www.lexique.org/public/prenoms.php) (not included).
- Population age group figures by [Irdes](https://www.data.gouv.fr/fr/datasets/population-par-tranche-d-age-et-sexe-estimations-localisees-de-population/) (not included).

# SEEALSO

- [This blog post](http://politbistro.hypotheses.org/2589) (in French) explains how to plot the data as a network.
- [This blog post](http://politbistro.hypotheses.org/2604) (in French) shows some intermediary results of the model.
- [This paper](https://files.nyu.edu/pba220/public/barbera_twitter_ideal_points.pdf) by [pablobarbera](https://github.com/pablobarbera) explains the nuts and bolts of the model.
- [This blog post](http://stats2u.blogspot.fr/2015/03/ideologia-de-politicos-usando-o-twitter.html) (in Portuguese) does something similar with Brazilian politicians.
- [This paper](http://dx.doi.org/10.1017/S0003055414000525) reviews the relevant literature and does something quite similar with Facebook "likes".
- [This blog post](http://blogs.lse.ac.uk/europpblog/2014/12/09/political-discussions-on-twitter-during-elections-are-dominated-by-those-with-extreme-views/) by [pablobarbera](https://github.com/pablobarbera) and [griverorz](https://github.com/griverorz) discusses partisan polarisation on Twitter.
