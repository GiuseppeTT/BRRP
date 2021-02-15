read_config <- function(
    path
) {
    path %>%
        yaml::read_yaml() %>%
        map(.listfy_big_columns) %>%  # Necessary for bind_rows
        bind_rows() %>%
        return()
}

.listfy_big_columns <- function(
    data
) {
    data %>%
        map_if(~ length(.x) > 1, list) %>%
        return()
}
