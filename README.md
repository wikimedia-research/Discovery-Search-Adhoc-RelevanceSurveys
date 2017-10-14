# Search Relevance Surveys

[![By EBernhardson](docs/figures/example_human_search_relevance_survey.png)](https://phabricator.wikimedia.org/F9161493)

Analysis of the 3<sup>rd</sup> running of the search relevance surveys ([T175048](https://phabricator.wikimedia.org/T175048)).

## Setup

### Libraries

Until [T178096](https://phabricator.wikimedia.org/T178096) is done and we have `discovery::learner` role to apply to instances on Wikimedia Cloud, the following commands have to be run:

```bash
sudo apt-get update
sudo apt-get install build-essential libopenblas-dev liblapack-dev r-base r-base-dev r-recommended git-core libxml2-dev libssl-dev libcurl4-openssl-dev libssh2-1-dev
```

### Packages

```R
# Essentials:
install.packages(c("tidyverse", "caret", "MLmetrics", "mlbench"))
# For bnclassify:
source("https://bioconductor.org/biocLite.R")
biocLite(c("RBGL", "Rgraphviz"))
# Classifiers:
install.packages(c("xgboost", "C50", "klaR", "e1071", "randomForest", "bnclassify"))
```

## TODO

- [x] Tune & train a bunch of classifiers (thanks, caret!)
- [ ] Figure out which sets of features yield the best predictive performance
- [ ] Investigate a multi-level approach based on Discernatron reliability (sort of?)
- [x] Investigate a [stacking / super learning approach](https://github.com/h2oai/h2o-tutorials/tree/master/tutorials/ensembles-stacking)
- [ ] Investigate how many responses & impressions we need to get reliable score estimates
- [ ] Write-up
