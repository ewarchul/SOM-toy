library(tidyverse)
library(magrittr)
library(zeallot)
library(here)
source("utils.R")

#' Read TSV data
#' @param filepath string
#' @return tibble data frame

read_data = function(filepath, names) {
  readr::read_tsv(
    filepath,
    skip = 1,
    col_names = names,
    col_types = c("nnn")
  ) 
}

#' Process data 
#' @param filepaths list of strings
#' @param names list of strings
#' @return tibble data frame

process_data = function(filepaths, names = c("Depth", "P1", "P2")) {
  filepaths %>%
    purrr::map(function(fp) {
      fp %>%
        read_data(names) %>%
        dplyr::mutate(Sensor = "Sensor-" %++% stringr::str_extract(fp, "\\d+"))
    }) %>%
  purrr::reduce(dplyr::bind_rows)
}

#' Generate filenames with data
#' @return list of strings 

get_names = function() {
  c(3, 4, 7, 8, 15, 16, 22, 23) %>%
    purrr::map_chr(function(num) {
      here::here(
        "data",
        stringr::str_glue("dmt{num}.tsv")
      )
  })
}

#' Load dataset 
#' @return tibble data frame

get_data = function() {
  get_names() %>%
    process_data()
}
