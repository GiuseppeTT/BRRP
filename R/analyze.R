# TODO: Implement plot_performance / plot_relative_price
quick_analyze <- function(
    data
) {
    data %>%
        metrify() %>%
        print()

    data %>%
        plot_price() %>%
        print()

    data %>%
        plot_rolling_return(days(365)) %>%
        print()

    print("365 days rolling return quantiles")
    data %>%
        table_rolling_return(days(365)) %>%
        print()

    print("30 days rolling return quantiles")
    data %>%
        table_rolling_return(days(30)) %>%
        print()

    print("7 days rolling return quantiles")
    data %>%
        table_rolling_return(days(7)) %>%
        print()

    data %>%
        plot_drawdown() %>%
        print()

    # TODO: create correlation plot with dendogram
    data %>%
        correlate() %>%
        print()

    data %>%
        plot_cluster() %>%
        print()
}

metrify <- function(
    data
) {
    data %>%
        group_by(asset) %>%
        summarise(
            mean = 252 * mean(return),
            sd = sqrt(252) * sd(return),
            sharpe = mean / sd,
            semi_d = sqrt(252) * semi_deviation(return),
            sortino = mean / semi_d,
            value = prod(1 + return),
            max_drawdown = min(drawdown(cumprod(1 + return)))
        ) %>%
        return()
}

plot_price <- function(
    data
) {
    data <-
        data %>%
        group_by(asset) %>%
        mutate(price = cumprod(1 + return))

    label_data <-
        data %>%
        group_by(asset) %>%
        filter(date == max(date))

    plot <-
        data %>%
        ggplot(aes(x = date, y = price, color = asset)) +
        geom_line() +
        geom_label_repel(aes(label = asset), data = label_data) +
        scale_y_log10(n.breaks = 10) +
        theme_bw(base_size = 16)

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

    label_data <-
        data %>%
        group_by(asset) %>%
        filter(date == max(date))


    plot <-
        data %>%
        ggplot(aes(x = date, y = rolling_return, color = asset)) +
        geom_line() +
        geom_hline(yintercept = 0, size = 2) +
        geom_label_repel(aes(label = asset), data = label_data) +
        scale_y_continuous(n.breaks = 10, labels = scales::percent) +
        theme_bw(base_size = 16)

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

    label_data <-
        data %>%
        group_by(asset) %>%
        filter(date == max(date))

    plot <-
        data %>%
        ggplot(aes(x = date, y = drawdown, color = asset)) +
        geom_line() +
        geom_hline(yintercept = 0, size = 2) +
        geom_label_repel(aes(label = asset), data = label_data) +
        scale_y_continuous(n.breaks = 10, labels = scales::percent) +
        theme_bw(base_size = 16)

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
        mutate(rolling_inverse_volatility = slide_index_dbl(return, date, ~ 1 / sd(.x), .before = window_size, .complete = TRUE))

    data <-
        data %>%
        group_by(date) %>%
        mutate(rolling_risk_contribution = rolling_inverse_volatility / sum(rolling_inverse_volatility)) %>%
        ungroup() %>%
        mutate(rolling_risk_contribution = if_else(rolling_risk_contribution - lag(rolling_risk_contribution) > threshold, lag(rolling_risk_contribution), rolling_risk_contribution)) %>%
        drop_na()

    label_data <-
        data %>%
        group_by(asset) %>%
        filter(date == max(date))

    plot <-
        data %>%
        ggplot(aes(x = date, y = rolling_risk_contribution , color = asset)) +
        geom_smooth(method = "lm", formula = y ~ 1, se = FALSE, linetype = "dashed") +
        geom_line(size = 2) +
        geom_label_repel(aes(label = asset), data = label_data) +
        scale_y_continuous(n.breaks = 10, labels = scales::percent) +
        theme_bw(base_size = 16)

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

check_risk_weights <- function(
    data
) {
    covariances <-
        data %>%
        pivot_wider(names_from = asset, values_from = return) %>%
        select(-date) %>%
        drop_na() %>%
        cov()

    covariances %>%
        RiskPortfolios::optimalPortfolio(control = list(type = "invvol")) %>%
        print()

    covariances %>%
        RiskPortfolios::optimalPortfolio(control = list(type = "erc")) %>%
        magrittr::set_names(rownames(covariances)) %>%
        print()
}
