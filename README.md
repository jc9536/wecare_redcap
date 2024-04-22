# WeCare Redcap Data Workflow

## Package introduction

This project is enabled by the following packages:

- The [`REDCapR`](https://ouhscbbmc.github.io/REDCapR/) package is used to extract data from RedCap and the provided API in each Redcap project (Youth & Caregiver). 

- The [`tidyverse`](https://www.tidyverse.org/) packages are used to clean the downloaded data from the RedCap API call and prepare it for reporting.

- The [`targets`](https://books.ropensci.org/targets/) package is used to streamline and automate the data cleaning and reporting process.

- The [`flextable`](https://ardata-fr.github.io/flextable-book/) package is used to build publishable tables from the cleaned dataset.

- The [`officer`](https://davidgohel.github.io/officer/) and [`officedown`](https://davidgohel.github.io/officedown/) packages are used to build formatted Word reports from a pre-programed R Markdown file.

## RedCap API Setup

- The RedCap API Tokens are available on redcap.nyu.edu, one for each RedCap project (Youth and Caregiver). They can be accessed through Application - API via the left panel.

- Please duplicate `redcap_api_example.R` in the `R` folder, and rename it to `redcap_api.R`.

- In `redcap_api.R`, please copy and paste the API tokens to `R/redcap_api.R` by replacing the values of `youth_api_token` and `caregiver_api_token`.

- The `youth_url` and `caregiver_url` should be a snippet of the site url in each of the RedCap project, ending with `/API/`. For example: `https://redcap.nyu.edu/redcap_v14.0.15/API/`.

- For data safety reasons, please **DO NOT** commit the script that contains the real API tokens and push it to GitHub. `R/redcap_api.R` in `.gitignore` makes sure that this file does not show up when you commit the changes.

## Data Cleaning

- The original codebook is available via RedCap. In `R/cleaning.R` (with an additional function in `functions.R`), I attempted to separately clean the youth and caregiver datasets, and merge them together. I also processed the data for reporting purposes.

- The cleaning mainly involves coalescing variables that are spread into multiple fields and forms on RedCap as a result of our survey design (e.g., in the contact forms and in the informed consent forms).

- When processing the data for reporting purposes, I also added manual fixes via code to social worker errors that result in unexpected data structures or variable values. For a comprehensive list of fixes, please check [this Word doc on Box](https://nyu.box.com/s/8vj7nrljzxsg7s8z68ilm80vgm2bey2m).

- Please note that all cleaning steps are wrapped in functions to be used as targets in `_targets.R`. This R script controls the dependencies across all data cleaning steps. Running `tar_make()` will check the integrity of the workflow and re-run steps that need to be updated.

- Also note that the first two targets (`dat_youth_raw`, `dat_caregiver_raw`) will be forced to run every time you run `tar_make()`, because an API call is involved in these two steps and the workflow does not know whether the online RedCap data has been updated or not. In addition, the `dat_youth_raw` target will usually take 20 seconds to run because we pre-loaded 2200 records into the survey.

- DO NOT manually change anything in the `_targets` folder -- the targets will be automatically updated after running `tar_make()`. See also `run.R` for other helper funcitons to run the pipeline. There is no need to modify `run.sh` either.

## Report Generation

- `R/table_functions.R` contains all the functions to generate the necessary tables (along with some utility functions in `R/utils.R`). These functions are used in `_targets.R` to create `flextable` tables.

- Using the `flextable` tables, a progress report is created in `inst/progress_report.Rmd`, using `inst/word_template.docx` as a template for formatting.

- In the final target in `_targets.R`, `tar_render()` is run to create the report based on the most-up-to-date data.

- Unlike a typical Word file knitted via R Markdown, this report contains special functions from `officer` and `officedown`, and is knitted via `officedown::rdocx_document` to provide more customization for the Word doc.

- The knitted report is saved to the `output` folder by default if you try to knit the Rmd file in the current R project.

## Shiny App

- `report_app.R` includes a minimal Shiny template with a password login page and a report generation page. The published webpage can be accessed [here](https://49lv5s-zezhen-wu.shinyapps.io/wecare_redcap/).

- The `Run Data Workflow` button essentially runs `tar_make()` in the backdrop and temporarily saves the Word file in the session. The `Download Report` allows you to download the file to a designated local folder of your choice.

- The `rsconnect` folder is auto-created when publishing the Shiny app.

## Miscellaneous

- `doc/randomization.R` contains the code to generate the randomization sequence on RedCap.
