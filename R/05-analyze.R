analyze <- function(
    data
) {
    results <-
        list()

    results$metric_table <-
        data %>%
        table_metrics()

    results$price_plot <-
        data %>%
        plot_price()

    results$relative_price_plot <-
        data %>%
        plot_relative_price()

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

    results$drawdown_plot <-
        data %>%
        plot_drawdown()

    results$relative_rolling_year_return_plot <-
        data %>%
        plot_relative_rolling_return(days(365))

    results$relative_rolling_year_return_table <-
        data %>%
        table_relative_rolling_return(days(365))

    results$relative_rolling_month_return_table <-
        data %>%
        table_relative_rolling_return(days(30))

    results$relative_rolling_week_return_table <-
        data %>%
        table_relative_rolling_return(days(7))

    results$relative_drawdown_plot <-
        data %>%
        plot_relative_drawdown()

    results$correlation_table <-
        data %>%
        filter(type != "Benchmark") %>%
        select(-type) %>%
        table_correlation()

    results$dendogram_plot <-
        data %>%
        filter(type == "Simple asset") %>%
        select(-type) %>%
        plot_dendogram()

    results$contains_portfolio <-
        data %>%
        pull(type) %>%
        magrittr::equals("Portfolio") %>%
        any()

    if (results$contains_portfolio) {
        results$rolling_year_weight_plot <-
            data %>%
            filter(type == "Simple asset") %>%
            plot_rolling_weight(days(365), threshold = 0.05)

        results$rolling_month_weight_plot <-
            data %>%
            filter(type == "Simple asset") %>%
            plot_rolling_weight(days(30), threshold = 0.05)

        results$weights_table <-
            data %>%
            filter(type == "Simple asset") %>%
            select(-type) %>%
            table_weights()
    }

    return(results)
}

table_metrics <- function(
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

plot_relative_price <- function(
    data
) {
    data <-
        data %>%
        complete(asset, date, fill = list(return = 0)) %>%
        group_by(asset) %>%
        mutate(type = if_else(is.na(type), .most_common(type), type)) %>%
        ungroup()

    data <-
        data %>%
        group_by(asset) %>%
        mutate(price = cumprod(1 + return)) %>%
        ungroup()

    benchmark_data <-
        data %>%
        filter(type == "Benchmark") %>%
        select(date, benchmark_price = price)

    data <-
        data %>%
        left_join(benchmark_data, by = "date") %>%
        mutate(relative_price = price / benchmark_price)

    plot <-
        data %>%
        ggplot(aes(x = date, y = relative_price, color = asset)) +
        geom_line(aes(size = type, linetype = type)) +
        scale_y_continuous(labels = scales::percent) +
        base_theme()

    plot <-
        plot %>%
        humanize_labs()

    return(plot)
}

.most_common <- function(
    x
) {
    unique_values <-
        x %>%
        unique()

    mode_position <-
        x %>%
        match(unique_values) %>%
        tabulate() %>%
        which.max()

    mode <-
        unique_values %>%
        magrittr::extract(mode_position)

    return(mode)
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

plot_relative_rolling_return <- function(
    data,
    window_size
) {
    data <-
        data %>%
        complete(asset, date, fill = list(return = 0)) %>%
        group_by(asset) %>%
        mutate(type = if_else(is.na(type), .most_common(type), type)) %>%
        ungroup()

    data <-
        data %>%
        group_by(asset) %>%
        mutate(rolling_return = rolling_return(return, date, window_size)) %>%
        ungroup() %>%
        drop_na()

    benchmark_data <-
        data %>%
        filter(type == "Benchmark") %>%
        select(date, benchmark_rolling_return = rolling_return)

    data <-
        data %>%
        left_join(benchmark_data, by = "date") %>%
        mutate(relative_rolling_return = rolling_return / benchmark_rolling_return)

    plot <-
        data %>%
        ggplot(aes(x = date, y = relative_rolling_return, color = asset)) +
        geom_line(aes(size = type, linetype = type)) +
        scale_y_continuous(labels = scales::percent) +
        base_theme()

    plot <-
        plot %>%
        humanize_labs()

    return(plot)
}

table_relative_rolling_return <- function(
    data,
    window_size,
    probs = 0:10 / 10
){
    data <-
        data %>%
        complete(asset, date, fill = list(return = 0)) %>%
        group_by(asset) %>%
        mutate(type = if_else(is.na(type), .most_common(type), type)) %>%
        ungroup()

    data <-
        data %>%
        group_by(asset) %>%
        mutate(rolling_return = rolling_return(return, date, window_size)) %>%
        ungroup() %>%
        drop_na()

    benchmark_data <-
        data %>%
        filter(type == "Benchmark") %>%
        select(date, benchmark_rolling_return = rolling_return)

    data <-
        data %>%
        left_join(benchmark_data, by = "date") %>%
        mutate(relative_rolling_return = rolling_return / benchmark_rolling_return)

    data <-
        data %>%
        group_by(asset) %>%
        summarise(as_tibble(quantile(relative_rolling_return, probs = probs, na.rm = T), rownames = "name")) %>%
        ungroup() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        humanize_column_names()

    return(data)
}

plot_relative_drawdown <- function(
    data
) {
    data <-
        data %>%
        complete(asset, date, fill = list(return = 0)) %>%
        group_by(asset) %>%
        mutate(type = if_else(is.na(type), .most_common(type), type)) %>%
        ungroup()

    data <-
        data %>%
        group_by(asset) %>%
        mutate(price = cumprod(1 + return)) %>%
        ungroup()

    benchmark_data <-
        data %>%
        filter(type == "Benchmark") %>%
        select(date, benchmark_price = price)

    data <-
        data %>%
        left_join(benchmark_data, by = "date") %>%
        mutate(relative_price = price / benchmark_price)

    data <-
        data %>%
        group_by(asset) %>%
        mutate(relative_drawdown = drawdown(relative_price))

    plot <-
        data %>%
        ggplot(aes(x = date, y = relative_drawdown, color = asset)) +
        geom_line(aes(size = type, linetype = type)) +
        scale_y_continuous(labels = scales::percent) +
        base_theme()

    plot <-
        plot %>%
        humanize_labs()

    return(plot)
}

table_correlation <- function(
    data
) {
    data %>%
        pivot_wider(names_from = asset, values_from = return) %>%
        select(-date) %>%
        drop_na() %>%
        cor() %>%
        as_tibble(rownames = "Asset") %>%
        return()
}

plot_dendogram <- function(
    data
) {
    plot_data <-
        data %>%
        pivot_wider(names_from = asset, values_from = return) %>%
        select(-date) %>%
        drop_na() %>%
        cor() %>%
        dist() %>%
        hclust() %>%
        as.dendrogram() %>%
        ggdendro::dendro_data(type = "rectangle")

    plot <-
        ggplot() +
        geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = plot_data$segments) +
        geom_text(aes(x = x, y = y, label = label), data = plot_data$labels, size = 5, nudge_y = -0.1) +
        ggdendro::theme_dendro()

    return(plot)
}

plot_rolling_weight <- function(
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
        mutate(rolling_weight = rolling_inverse_volatility / sum(rolling_inverse_volatility)) %>%
        ungroup() %>%
        mutate(rolling_weight = if_else(rolling_weight - lag(rolling_weight) > threshold, lag(rolling_weight), rolling_weight)) %>%
        drop_na()

    plot <-
        data %>%
        ggplot(aes(x = date, y = rolling_weight , color = asset)) +
        geom_smooth(method = "lm", formula = y ~ 1, se = FALSE, linetype = "dashed") +
        geom_line(aes(size = type, linetype = type)) +
        scale_y_continuous(labels = scales::percent) +
        base_theme()

    plot <-
        plot %>%
        humanize_labs()

    return(plot)
}

table_weights <- function(
    data
) {
    naive_risk_contribution_weights <-
        data %>%
        .compute_weights(weights_type = "naive risk contribution") %>%
        enframe() %>%
        rename(asset = name, naive_risk_contribution = value)

    risk_contribution_weights <-
        data %>%
        .compute_weights(weights_type = "risk contribution") %>%
        enframe() %>%
        rename(asset = name, risk_contribution = value)

    weights <-
        naive_risk_contribution_weights %>%
        left_join(risk_contribution_weights, by = "asset") %>%
        humanize_column_names()

    return(weights)
}
