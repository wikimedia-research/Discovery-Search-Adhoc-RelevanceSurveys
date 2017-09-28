# Search Relevance Surveys

[![By EBernhardson](docs/figures/example_human_search_relevance_survey.png)](https://phabricator.wikimedia.org/F9161493)

Analysis of the 3<sup>rd</sup> running of the search relevance surveys ([T175048](https://phabricator.wikimedia.org/T175048)).

## Datasets

- [search_queries.tsv](data/search_queries.tsv): table of 133 search queries we asked users about
    - **query_id**: used for joining
    - **query**: queries from our Discernatron project
- [discernatron_scores.tsv](data/discernatron_scores.tsv): table of relevance scores for 4,745 pages
    - **query_id**: used for joining with **search_queries.tsv**
    - **page_id**: article identifier used for joining
    - **page_title**: article name
    - **score**: relevance score from Discernatron ratings (0-3)
    - **daily_pvs**: average daily pageviews for this page
- [survey_responses.tsv.gz](data/survey_responses.tsv.gz): compressed table of 1,236,097 users' 1,305,511 responses & non-responses (248,465 yes/no/unsure answers; 4,569 dismisses; 1,052,477 time-outs)
    - **date**: from 2017-09-06 to 2017-09-21
    - **query_id**: used for joining with **search_queries.tsv**
    - **session_id**: randomly generated MediaWiki session ID stored in local storage; can repeat between days but is unique per day
    - **ts**: timestamp formatted as "2017-09-07T15:42:47Z"
    - **survey_id**: due to a sampling configuration bug, some users saw the survey multiple times; this is a per-session counter
    - **question_id**: coded as follows:
        1. "If you searched for '...', would this article be a good result?"
        2. "Would you click on this page when searching for '...'?"
        3. "If someone searched for '...', would they want to read this article?"
        4. "If you searched for '...', would this article be relevant?"
    - **page_id**: used for joining with **discernatron_scores.tsv**
    - **choice**:
        - *dismiss*: if the user dismissed the survey
        - *timeout*: if the user did not respond after 60 seconds
        - *unsure*: displayed as "I don't know"
        - *no*/*yes*
