analyze <- function(
    data,
    assets,
    benchmark = "CDI",
    portfolio = NULL,
    weights = NULL
) {
    data <-
        data %>%
        filter_asset(c(benchmark, assets))

    if (!is.null(portfolio) & !is.null(weights)) {
        portfolio_data <-
            data %>%
            filter(asset != benchmark) %>%
            build_portfolio(portfolio, weights)

        data <-
            data %>%
            bind_rows(portfolio_data)
    } else if (xor(!is.null(portfolio), !is.null(weights))) {
        stop("You must provide both or neither 'portfolio' and 'weights' arguments")
    }

    data <-
        data %>%
        mutate(type = case_when(
            asset ==   benchmark               ~ "Benchmark",
            asset %in% assets                  ~ "Simple asset",
            false_if_empty(asset == portfolio) ~ "Portfolio"
        )) %>%
        mutate(asset = factor(asset, levels = c(benchmark, assets, portfolio))) %>%
        mutate(type = factor(type, levels = c("Benchmark", "Simple asset", "Portfolio"))) %>%
        select(type, asset, date, return) %>%
        arrange(type, asset, date)

    results <-
        list()

    results$metric_table <-
        data %>%
        metrify()

    results$price_plot <-
        data %>%
        plot_price()

    # TODO: Implement relative price

    results$drawdown_plot <-
        data %>%
        plot_drawdown()

    results$rolling_year_return_plot <-
        data %>%
        plot_rolling_return(days(365))

    results$rolling_year_return_table <-
        data %>%
        table_rolling_return(days(365))

    results$rolling_month_return_table <-
        data %>%
        table_rolling_return(days(30))

    results$rolling_week_return_table <-
        data %>%
        table_rolling_return(days(7))

    # TODO: create correlation plot with dendogram
    # data %>%
    #     correlate() %>%
    #     print()
    #
    # data %>%
    #     plot_cluster() %>%
    #     print()

    if (!is.null(weights)) {
        results$rolling_year_risk_contribution_plot <-
            data %>%
            filter(type == "Simple asset") %>%
            plot_rolling_risk_contribution(days(365), threshold = 0.05)

        results$rolling_quarter_risk_contribution_plot <-
            data %>%
            filter(type == "Simple asset") %>%
            plot_rolling_risk_contribution(days(91), threshold = 0.05)

        results$weights_table <-
            data %>%
            filter(type == "Simple asset") %>%
            select(-type) %>%
            table_weights()
    }

    return(results)
}

table_weights <- function(
    data
) {
    naive_risk_contribution_weights <-
        data %>%
        compute_weights(weights_type = "naive risk contribution") %>%
        enframe() %>%
        rename(asset = name, naive_risk_contribution = value)

    risk_contribution_weights <-
        data %>%
        compute_weights(weights_type = "risk contribution") %>%
        enframe() %>%
        rename(asset = name, risk_contribution = value)

    weights <-
        naive_risk_contribution_weights %>%
        left_join(risk_contribution_weights, by = "asset") %>%
        humanize_column_names()

    return(weights)
}

false_if_empty <- function(
    x
) {
    if (length(x) == 0)
        return(FALSE)
    else
        return(x)
}

filter_asset <- function(
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

build_portfolio <- function(
    data,
    portfolio,
    weights
) {
    if (is.character(weights)) {
        weights <- compute_weights(data, weights)
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
        arrange(positionize(asset, by = names(weights))) %>%
        summarise(portfolio_price = sum(weights * price)) %>%
        mutate(portfolio_return = portfolio_price / lag(portfolio_price) - 1) %>%
        slice(-1) %>%
        mutate(asset = portfolio) %>%
        select(asset, date, return = portfolio_return)

    return(portfolio_data)
}

compute_weights <- function(
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

positionize <- function(
    x,
    by
) {
    x %>%
        match(by) %>%
        return()
}

metrify <- function(
    data
) {
    data %>%
        group_by(asset) %>%
        summarise(
            CAGR               = prod(1 + return)^(252 / length(date)) - 1,
            mean               = 252 * mean(return),
            standard_deviation = sqrt(252) * sd(return),
            sharpe             = mean / standard_deviation,
            semi_deviation     = sqrt(252) * semi_deviation(return),
            sortino            = mean / semi_deviation,
            worst_drawdown     = min(drawdown(cumprod(1 + return)))
        ) %>%
        humanize_column_names() %>%
        return()
}

plot_price <- function(
    data
) {
    data <-
        data %>%
        group_by(asset) %>%
        mutate(price = cumprod(1 + return))

    plot <-
        data %>%
        ggplot(aes(x = date, y = price, color = asset)) +
        geom_line(aes(size = type, linetype = type)) +
        scale_y_log10() +
        base_theme()

    plot <-
        plot %>%
        humanize_labs()

    return(plot)
}

plot_rolling_return <- function(
    data,
    window_size
) {
    data <-
        data %>%
        group_by(asset) %>%
        mutate(rolling_return = rolling_return(return, date, window_size)) %>%
        drop_na()

    plot <-
        data %>%
        ggplot(aes(x = date, y = rolling_return, color = asset)) +
        geom_line(aes(size = type, linetype = type)) +
        geom_hline(yintercept = 0, size = 2) +
        scale_y_continuous(labels = scales::percent) +
        base_theme()

    plot <-
        plot %>%
        humanize_labs()

    return(plot)
}

table_rolling_return <- function(
    data,
    window_size,
    probs = 0:10 / 10
) {
    data %>%
        group_by(asset) %>%
        mutate(rolling_return = rolling_return(return, date, window_size)) %>%
        summarise(as_tibble(quantile(rolling_return, probs = probs, na.rm = T), rownames = "name")) %>%
        ungroup() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        humanize_column_names() %>%
        return()
}

plot_drawdown <- function(
    data
) {
    data <-
        data %>%
        group_by(asset) %>%
        mutate(price = cumprod(1 + return)) %>%
        mutate(drawdown = drawdown(price))

    plot <-
        data %>%
        ggplot(aes(x = date, y = drawdown, color = asset)) +
        geom_line(aes(size = type, linetype = type)) +
        scale_y_continuous(labels = scales::percent) +
        base_theme()

    plot <-
        plot %>%
        humanize_labs()

    return(plot)
}

correlate <- function(
    data
) {
    data %>%
        pivot_wider(names_from = asset, values_from = return) %>%
        select(-date) %>%
        corrr::correlate() %>%
        return()
}

plot_cluster <- function(
    data
) {
    data %>%
        pivot_wider(names_from = asset, values_from = return) %>%
        select(-date) %>%
        drop_na() %>%
        cor() %>%
        dist() %>%
        hclust() %>%
        factoextra::fviz_dend(main = NULL, xlab = "") %>%
        return()
}

plot_rolling_risk_contribution <- function(
    data,
    window_size,
    threshold
) {
    data <-
        data %>%
        group_by(asset) %>%
        mutate(rolling_inverse_volatility = slider::slide_index_dbl(return, date, ~ 1 / sd(.x), .before = window_size, .complete = TRUE))

    data <-
        data %>%
        group_by(date) %>%
        mutate(rolling_risk_contribution = rolling_inverse_volatility / sum(rolling_inverse_volatility)) %>%
        ungroup() %>%
        mutate(rolling_risk_contribution = if_else(rolling_risk_contribution - lag(rolling_risk_contribution) > threshold, lag(rolling_risk_contribution), rolling_risk_contribution)) %>%
        drop_na()

    plot <-
        data %>%
        ggplot(aes(x = date, y = rolling_risk_contribution , color = asset)) +
        geom_smooth(method = "lm", formula = y ~ 1, se = FALSE, linetype = "dashed") +
        geom_line(aes(size = type, linetype = type)) +
        scale_y_continuous(labels = scales::percent) +
        base_theme()

    plot <-
        plot %>%
        humanize_labs()

    return(plot)
}

table_risk_contribution <- function(
    data
) {
    data %>%
        group_by(asset) %>%
        summarise(inverse_volatility = 1 / sd(return)) %>%
        mutate(risk_contribution = inverse_volatility / sum(inverse_volatility)) %>%
        select(asset, risk_contribution) %>%
        return()
}
