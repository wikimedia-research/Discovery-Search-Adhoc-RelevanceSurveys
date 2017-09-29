# Datasets

## Search Queries

[search_queries.tsv](search_queries.tsv) is a table of 133 search queries we asked users about, and has the following columns:

- **query_id**: used for joining
- **query**: queries from our [Discernatron project](https://www.mediawiki.org/wiki/Discernatron)

## Discernatron Scores

[![File:Discernatron screenshot.png by CKoerner (WMF) [CC BY-SA 3.0 (https://creativecommons.org/licenses/by-sa/3.0), GFDL (http://www.gnu.org/copyleft/fdl.html) or CC BY-SA 4.0 (https://creativecommons.org/licenses/by-sa/4.0)], via Wikimedia Commons](https://upload.wikimedia.org/wikipedia/commons/3/37/Discernatron_screenshot.png)](https://commons.wikimedia.org/wiki/File:Discernatron_screenshot.png)

[discernatron_scores.tsv](discernatron_scores.tsv) is a table of relevance scores for 4,745 pages collected from our [Discernatron project](https://www.mediawiki.org/wiki/Discernatron), and has the following columns:

- **query_id**: used for joining with **search_queries.tsv**
- **page_id**: article identifier used for joining
- **page_title**: article name
- **score**: relevance score from Discernatron ratings (0-3)
- **daily_pvs**: average daily pageviews for this page

## Survey Responses

[survey_responses.tsv.gz](survey_responses.tsv.gz) is a compressed table of 1,236,097 users' 1,305,511 responses & non-responses:

- 248,465 yes/no/unsure answers
- 4,569 dismisses
- 1,052,477 time-outs

It has the following columns:

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
