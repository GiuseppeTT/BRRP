base_theme <- function(
    # Empty
) {
    return(list(
        scale_size_manual(values = c("Benchmark" = 2, "Simple asset" = 0.5, "Portfolio" = 2)),
        scale_linetype_manual(values = c("Benchmark" = "dashed", "Simple asset" = "solid", "Portfolio" = "solid")),
        theme_bw(base_size = 16),
        theme(legend.position = "none")
    ))
}

`%outside%` <- function(
    x,
    y
) {
    x %>%
        `%in%`(y) %>%
        magrittr::not() %>%
        return()
}

humanize_column_names <- function(
    data
) {
    humane_column_names <-
        data %>%
        colnames() %>%
        humanize_string()

    data %>%
        magrittr::set_colnames(humane_column_names) %>%
        return()
}

humanize_labs <- function(
    plot
) {
    humane_labs <-
        plot %>%
        magrittr::extract2("labels") %>%
        purrr::map(humanize_string)

    plot %>%
        magrittr::inset2("labels", humane_labs) %>%
        return()
}

humanize_string <- function(
    string
) {
    string %>%
        str_replace_all(pattern = r"([_\.])", replacement = " ") %>%
        if_else(. == str_to_upper(.), ., str_to_sentence(.)) %>%
        return()
}

time_diff_length <- function(
    date,
    unit = "second"
) {
    date %>%
        range() %>%
        diff() %>%
        time_length(unit = unit) %>%
        return()
}
