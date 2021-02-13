download_data <- function(
    # Empty
) {
    Quandl::Quandl.api_key(Sys.getenv("QUANDL_API_KEY"))

    BCB_data <- .download_BCB_data()
    IF_data <- .download_IF_data()

    data <-
        bind_rows(BCB_data, IF_data) %>%
        arrange(asset, date)

    return(data)
}

.download_BCB_data <- function(
    # Empty
) {
    # TODO: move Fake to outside logic
    data_arguments <- tribble(
        ~name            , ~symbol    ,
        "IMA"            , "BCB/12469",
        "IMA-S"          , "BCB/12462",
        "IMA-B"          , "BCB/12466",
        "IMA-B 5"        , "BCB/12467",
        "IMA-B 5+"       , "BCB/12468",
        "Fake IMA-B 5 P2", "BCB/12467",  # IMA-B 5 is used as proxy for IMA-B 5 P2
        "IRF-M"          , "BCB/12461",
        "IRF-M 1"        , "BCB/17626",
        "IRF-M 1+"       , "BCB/17627",
        "Fake IRF-M P2"  , "BCB/12461",  # IRF-M is used as proxy for IRF-M P2
    )

    data_arguments %>%
        pmap(.download_each_BCB_data) %>%
        bind_rows() %>%
        arrange(asset, date) %>%
        return()
}

.download_each_BCB_data <- function(
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

# TODO: automatic download data
.download_IF_data <- function(
    # Empty
) {
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
