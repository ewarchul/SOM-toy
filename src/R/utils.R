#' String concatenation operator 

'%++%' = function(str1, str2) {
  paste0(str1, str2)
}

#' Tibble -> Map k v
shapiro_test = function(dfx) {
  melted_df = tibble::tibble(reshape::melt(data.frame(dfx)))
  test_df = 
    melted_df %>%
    dplyr::group_by(variable) %>%
    dplyr::group_map(function(d, g) {
      rstatix::shapiro_test(d[[1]], "value") %>%
        dplyr::mutate(g)
    }) %>%
    purrr::reduce(dplyr::bind_rows)
  test_df %>%
    dplyr::select(c(variable, p.value)) %>%
    tibble::deframe()
}
