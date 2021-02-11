download_BCB_data <- function(
    # Empty
) {
    .BCB_DATA_ARGS %>%
        pmap(.download_BCB_data) %>%
        bind_rows() %>%
        arrange(asset, date) %>%
        return()
}

.download_BCB_data <- function(
    name,
    symbol
) {
    data <-
        symbol %>%
        Quandl::Quandl(transform = "rdiff") %>%
        as_tibble()

    data <-
        data %>%
        rename_with(str_to_lower) %>%
        mutate(asset = name) %>%
        select(asset, date, return = value)

    return(data)
}

download_IF_data <- function(
    # Empty
) {
    # TODO: automatic download data
    indexes_data <-
        readxl::read_excel("data/IF_indexes.xlsx") %>%
        select(asset = `Índice`, date = Data, return = `Variação`) %>%
        mutate(date = as_date(date)) %>%
        drop_na()

    funds_data <-
        readxl::read_excel("data/IF_funds.xlsx") %>%
        select(asset = `Fundo`, date = Data, return = `Variação`) %>%
        mutate(date = as_date(date)) %>%
        mutate(asset = asset %>% str_extract(r"([^\s]+\s+[^\s]+)") %>% str_to_title()) %>%
        drop_na()

    data <-
        bind_rows(indexes_data, funds_data) %>%
        arrange(asset, date)

    return(data)
}

download_yahoo_data <- function(
    # Empty
) {
    .YAHOO_DATA_ARGS %>%
        tidyquant::tq_get() %>%
        return()
}

fake_asset <- function(
    data,
    base,
    fake
) {
    model_formula <-
        glue("`{fake}` ~ `{base}`") %>%
        as.formula()

    model <-
        data %>%
        filter(asset %in% c(base, fake)) %>%
        pivot_wider(names_from = asset, values_from = return) %>%
        lm(model_formula, data = .)

    model_coefficient <-
        model %>%
        coefficients() %>%
        magrittr::extract(2)

    min_fake_date <-
        data %>%
        filter(asset == fake) %>%
        pull(date) %>%
        min()

    fake_data <-
        data %>%
        filter(asset == base) %>%
        filter(date < min_fake_date) %>%
        mutate(return = model_coefficient * return)

    fake_data <-
        data %>%
        filter(asset == fake) %>%
        bind_rows(fake_data)

    fake_data <-
        fake_data %>%
        mutate(asset = as.character(glue("Fake {fake}"))) %>%
        arrange(date)

    return(fake_data)
}

merge_asset <- function(
    data,
    assets,
    merge_name
) {
    data %>%
        filter_asset(assets) %>%
        group_by(date) %>%
        summarise(return = mean(return)) %>%
        mutate(asset = merge_name, .before = everything()) %>%
        return()
}

build_rp_portfolio <- function(
    data,
    portfolio_name
) {
    weights <-
        assets %>%
        filter_asset(old_selection) %>%
        table_risk_contribution() %>%
        pull(risk_contribution)

    data <-
        data %>%
        filter_asset(old_selection) %>%
        complete(asset, date, fill = list(return = 0))

    data <-
        data %>%
        group_by(asset) %>%
        arrange(date) %>%
        mutate(price = cumprod(1 + return))

    data <-
        data %>%
        group_by(date) %>%
        summarise(portfolio_price = sum(weights * price)) %>%
        mutate(portfolio_return = portfolio_price / lag(portfolio_price) - 1) %>%
        drop_na() %>%
        mutate(portfolio = portfolio_name) %>%
        select(asset = portfolio, date, return = portfolio_return)

    return(data)
}
