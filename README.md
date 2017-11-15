# Search Relevance Surveys

[![By EBernhardson](docs/figures/example_human_search_relevance_survey.png)](https://phabricator.wikimedia.org/F9161493)

Analysis of the 3<sup>rd</sup> running of the search relevance surveys ([T175048](https://phabricator.wikimedia.org/T175048)).

## Setup

### Libraries

Since [T178096](https://phabricator.wikimedia.org/T178096) is done, apply the roles [`discovery::learner`](https://github.com/wikimedia/puppet/blob/production/modules/role/manifests/discovery/learner.pp) or [`discovery::allstar_cruncher`](https://github.com/wikimedia/puppet/blob/production/modules/role/manifests/discovery/allstar_cruncher.pp) to instances on Wikimedia Cloud (formerly Wikimedia Labs).

### Packages

```R
# Essentials:
install.packages(c("tidyverse", "caret", "MLmetrics", "mlbench"))
# For bnclassify:
source("https://bioconductor.org/biocLite.R")
biocLite(c("RBGL", "Rgraphviz"))
# Classifiers:
install.packages(c("xgboost", "C50", "klaR", "e1071", "randomForest", "deepnet", "bnclassify"))
```

## TODO

- [x] Tune & train a bunch of classifiers (thanks, caret!)
- [ ] Figure out which sets of features yield the best predictive performance
- [x] Investigate a multi-level approach based on Discernatron reliability (sort of?)
- [x] Investigate a [stacking / super learning approach](https://github.com/h2oai/h2o-tutorials/tree/master/tutorials/ensembles-stacking)
- [ ] Investigate how many responses & impressions we need to get reliable score estimates
- [ ] Write-up

## Scripts

1. **Data**
  - [pageviews.R](pageviews.R) uses the Wikimedia Analytics Pageviews API to fetch a month worth of daily pageview counts for the relevant articles
  - [events.R](events.R) fetches the survey data from Event Logging database
  - [discernatron.R](discernatron.R) fetches relevance scores from Discernatron's API
  - [data.R](data.R) combines fetched pageviews, survey data, and Discernatron scores into complete datasets
2. **Model Tuning & Training** via [models.R](models.R)
3. **Model Evaluation** via [evaluate.R](evaluate.R)
4. (Optional) Retrying certain combinations via [retry.R](retry.R)
5. (Optional) Visualizing trained models' accuracy via [visualize.R](visualize.R)
