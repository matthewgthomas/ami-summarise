# ami-summarise
Summarise output from [The Content Mine's](http://contentmine.org/) [AMI plug-in](https://github.com/ContentMine/ami-plugin) into a format that can be displayed in [ami-viz](https://github.com/matthewgthomas/ami-viz).

Before running this code, run the following Content Mine pipeline (see `mine.sh` for example commands):

1. `getpapers` to download articles
2. `norma` to create scholarly.html for all articles
3. `ami2-species` or `ami2-word` to generate the results.xml files

## Input directory/files
The data directory (stored in the `data.dir` variable in `main.r`) should contain one subfolder for each article, named after the article's ID (done automatically by `getpapers`). After processing with AMI, these subfolders should contain a 'results' folder, which will in turn contain:

- species/binomial/results.xml
- word/frequencies/results.xml

This program will take these files, summarise them across all articles and output JSON files ready to be visualised in an interactive network graph (for example: https://github.com/matthewgthomas/ami-viz).

To use, run the code in `main.r`.

## Output files
The program will write three JSON files containing nodes and edge lists:

- `words.json` -- the top X most frequent words and the articles in which they appear
- `words_tdidf.json` -- same as above but calculated using term frequency-inverse document frequency (TF-IDF)
- `species.json` -- occurrences of binomial species names and the articles in which they appear

## To do

- Add support for more AMI plugins
- Make faster -- code is slow when running over 100s of articles
