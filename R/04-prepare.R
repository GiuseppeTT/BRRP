prepare_data_for_analysis <- function(
    data,
    assets,
    benchmark,
    weights = NA
){
    data <-
        data %>%
        .filter_asset(c(benchmark, assets))

    if (!is.na(weights)) {
        portfolio_data <-
            data %>%
            filter(asset != benchmark) %>%
            .build_portfolio(weights)

        data <-
            data %>%
            bind_rows(portfolio_data)
    }

    data <-
        data %>%
        mutate(type = case_when(
            asset ==   benchmark ~ "Benchmark",
            asset %in% assets    ~ "Simple asset",
            asset == "Portfolio" ~ "Portfolio"
        )) %>%
        mutate(asset = factor(asset, levels = c(benchmark, assets, "Portfolio"))) %>%
        mutate(type = factor(type, levels = c("Benchmark", "Simple asset", "Portfolio"))) %>%
        select(type, asset, date, return) %>%
        arrange(type, asset, date)

    return(data)
}

.filter_asset <- function(
    data,
    assets
) {
    data %>%
        filter(asset %in% assets) %>%
        .enforce_common_range(date, by = asset) %>%
        return()
}

.enforce_common_range <- function(
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

.build_portfolio <- function(
    data,
    weights
) {
    if (is.character(weights)) {
        weights <- .compute_weights(data, weights)
    } else if (is.numeric(weights)) {
        if (is.null(names(weights)))
            stop("The 'weights' argument must be named")

        assets <-
            data %>%
            pull(asset) %>%
            unique()

        if (length(weights) != length(assets))
            stop("The 'weights' argument must have same length as assets")

        if (any(names(weights) %outside% assets))
            stop("The 'weights' argument names must be contained in assets")
    } else {
        stop("Invalid weights value")
    }

    data <-
        data %>%
        complete(asset, date, fill = list(return = 0))

    data <-
        data %>%
        group_by(asset) %>%
        arrange(date) %>%
        mutate(price = cumprod(1 + return)) %>%
        ungroup()

    portfolio_data <-
        data %>%
        group_by(date) %>%
        arrange(.positionize(asset, by = names(weights))) %>%
        summarise(portfolio_price = sum(weights * price)) %>%
        mutate(portfolio_return = portfolio_price / lag(portfolio_price) - 1) %>%
        slice(-1) %>%
        mutate(asset = "Portfolio") %>%
        select(asset, date, return = portfolio_return)

    return(portfolio_data)
}

.compute_weights <- function(
    data,
    weights_type
) {
    if (weights_type == "naive risk contribution") {
        weights <-
            data %>%
            group_by(asset) %>%
            summarise(inverse_volatility = 1 / sd(return)) %>%
            mutate(weight = inverse_volatility / sum(inverse_volatility)) %>%
            select(asset, weight) %>%
            deframe()
    } else if (weights_type == "risk contribution") {
        covariances <-
            data %>%
            pivot_wider(names_from = asset, values_from = return) %>%
            select(-date) %>%
            drop_na() %>%
            as.matrix() %>%
            RiskPortfolios::covEstimation(control = list(type = "naive"))

        weights <-
            covariances %>%
            RiskPortfolios::optimalPortfolio(control = list(type = "erc", constraint = "lo")) %>%
            magrittr::set_names(rownames(covariances))

    } else {
        stop("Invalid weights value")
    }

    return(weights)
}

.positionize <- function(
    x,
    by
) {
    x %>%
        match(by) %>%
        return()
}
