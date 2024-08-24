# Variation-of-Personal-Infinitive

Folder structure and file descriptions:
- .csv: Portuguese key-word-in-context (KWIC) files with left- and right-side contexts (the full sentences). Downloaded from SketchEngine ptTenTen20 Portuguese web-scrapped corpus data based on series of queries.
- R Script: feeds in the .csvs into dataframes for analysis. The whole point of the script is to extract all of the linguistic angles (ways to slice the data: variables) wanted for statistical analysis at the end of the script.
These angles are extracted from the KWIC column and added to new columns in the data frame, some regular expressions involved. The summary function at the bottom and the variety of models are different options for logistic regressions
used to check which angle-slice of linguistic cutting actually means anything. Consult p-values (<0.05) for significance by variable.
