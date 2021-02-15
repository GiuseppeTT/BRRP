complement_data <- function(
    data
) {
    proxed_imab5p2_data <-
        data %>%
        .proxy_asset("IMA-B 5 P2", base = "IMA-B 5")

    proxed_irfmp2_data <-
        data %>%
        .proxy_asset("IRF-M P2", base = "IRF-M")

    data <-
        data %>%
        bind_rows(proxed_imab5p2_data, proxed_irfmp2_data) %>%
        arrange(asset, date)

    extended_darius_data <-
        data %>%
        .extend_asset("Giant Darius", base = "Giant Zarathustra")

    extended_sigma_data <-
        data %>%
        .extend_asset("Giant Sigma", base = "Giant Axis")

    data <-
        data %>%
        bind_rows(extended_darius_data, extended_sigma_data) %>%
        arrange(asset, date)

    mix_darius_sigma_data <-
        data %>%
        .mix_asset(c("Giant Darius", "Giant Sigma"), name = "Giant Darius + Sigma")

    mix_zarathustra_sigma_data <-
        data %>%
        .mix_asset(c("Giant Zarathustra", "Giant Sigma"), name = "Giant Zarathustra + Sigma")

    data <-
        data %>%
        bind_rows(mix_darius_sigma_data, mix_zarathustra_sigma_data) %>%
        arrange(asset, date)

    return(data)
}

.proxy_asset <- function(
    data,
    proxed,
    base
) {
    data %>%
        filter(asset == base) %>%
        mutate(asset = proxed) %>%
        return()
}

.extend_asset <- function(
    data,
    extend,
    base
) {
    model_formula <-
        str_glue("`{extend}` ~ `{base}`") %>%
        as.formula()

    model <-
        data %>%
        filter(asset %in% c(base, extend)) %>%
        pivot_wider(names_from = asset, values_from = return) %>%
        lm(model_formula, data = .)

    model_coefficient <-
        model %>%
        coefficients() %>%
        magrittr::extract(2)

    min_extend_date <-
        data %>%
        filter(asset == extend) %>%
        pull(date) %>%
        min()

    extended_data <-
        data %>%
        filter(asset == base) %>%
        filter(date < min_extend_date) %>%
        mutate(return = model_coefficient * return)

    extended_data <-
        data %>%
        filter(asset == extend) %>%
        bind_rows(extended_data) %>%
        arrange(date)

    return(extended_data)
}

.mix_asset <- function(
    data,
    assets,
    name,
    weights = NULL
) {
    if (is.null(weights)) {
        weights <-
            rep(1, length(assets)) %>%
            magrittr::divide_by(length(assets)) %>%
            magrittr::set_names(assets)
    }

    data %>%
        .filter_asset(assets) %>%
        group_by(date) %>%
        arrange(.positionize(asset, by = names(weights))) %>%
        summarise(return = sum(weights * return)) %>%
        mutate(asset = name) %>%
        select(asset, date, return) %>%
        return()
}
