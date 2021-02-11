filter_asset <- function(
    data,
    assets
) {
    data %>%
        filter(asset %in% assets) %>%
        mutate(asset = factor(asset, assets)) %>%
        enforce_common_range(date, by = asset) %>%
        return()
}

enforce_common_range <- function(
    data,
    variable,
    by
) {
    common_range <-
        data %>%
        group_by({{by}}) %>%
        summarise(min_var = min({{variable}}), max_var = max({{variable}})) %>%
        summarise(min = max(min_var), max = min(max_var)) %>%
        as.list()

    data <-
        data %>%
        filter(common_range$min <= {{variable}}, {{variable}} <= common_range$max)

    return(data)
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
        stringr::str_replace_all(pattern = r"([_\.])", replacement = " ") %>%
        stringr::str_to_sentence() %>%
        return()
}
