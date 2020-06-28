if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

if(!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

if(!require(jsonlite)) {
  install.packages("jsonlite")
  library(jsonlite)
}

url <- "https://www.bleague.jp/game_detail/?ScheduleKey=4556"
page <- xml2::read_html(url, encoding = "utf-8")
script_tags <- page %>%
  rvest::html_nodes("script") %>%
  rvest::html_text()

for (tag in script_tags) {
  if (grepl("_contexts_s3id.data", tag, ignore.case = TRUE)) {
    jsonStr <- stringr::str_extract(tag, "\\{\".*\"\\}")
  }
}

jsonObj <- jsonlite::fromJSON(jsonStr)
View(jsonObj$PlayByPlays)
View(jsonObj$HomeBoxscores)
View(jsonObj$Summaries)
View(jsonObj$Leaders)
