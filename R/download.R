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
    data_arguments <- tribble(
        ~name            , ~symbol    ,
        "IMA"            , "BCB/12469",
        "IMA-S"          , "BCB/12462",
        "IMA-B"          , "BCB/12466",
        "IMA-B 5"        , "BCB/12467",
        "IMA-B 5+"       , "BCB/12468",
        "IRF-M"          , "BCB/12461",
        "IRF-M 1"        , "BCB/17626",
        "IRF-M 1+"       , "BCB/17627",
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

# TODO: automate download process
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

# TODO: automate download process
# http://www.b3.com.br/pt_br/market-data-e-indices/servicos-de-dados/market-data/historico/mercado-a-vista/cotacoes-historicas/
.download_B3_data <- function(
    # Empty
) {
    column_positions <- fwf_cols(
        tipreg = c(  1,   2),
        date   = c(  3,  10),
        codbdi = c( 11,  12),
        codneg = c( 13,  24),
        tpmerc = c( 25,  27),
        nomres = c( 28,  39),
        especi = c( 40,  49),
        prazot = c( 50,  52),
        modref = c( 53,  56),
        preabe = c( 57,  69),
        premax = c( 70,  82),
        premin = c( 83,  95),
        premed = c( 96, 108),
        preult = c(109, 121),
        preofc = c(122, 134),
        totneg = c(148, 152),
        quatot = c(153, 170),
        voltot = c(171, 188),
        preexe = c(189, 201),
        indopc = c(202, 202),
        datven = c(203, 210),
        fatcot = c(211, 217),
        ptoexe = c(218, 230),
        cdoisi = c(231, 242),
        dismes = c(243, 245)
    )

    column_types <- cols(
        tipreg = col_number(),
        date   = col_date("%Y%m%d"),
        codbdi = col_character(),
        codneg = col_character(),
        tpmerc = col_number(),
        nomres = col_character(),
        especi = col_character(),
        prazot = col_character(),
        modref = col_character(),
        preabe = col_number(),
        premax = col_number(),
        premin = col_number(),
        premed = col_number(),
        preult = col_number(),
        preofc = col_number(),
        totneg = col_number(),
        quatot = col_number(),
        voltot = col_number(),
        preexe = col_number(),
        indopc = col_number(),
        datven = col_number(),
        fatcot = col_number(),
        ptoexe = col_number(),  # TODO: check what the fuck is "(07)V06" type
        cdoisi = col_character(),
        dismes = col_number()
    )

    without_decimal_columns <- c(
        "preabe",
        "premax",
        "premin",
        "premed",
        "preult",
        "preofc",
        "voltot",
        "preexe"
    )

    path <-
        here::here("data/COTAHIST_A2021.ZIP")

    line_count <-
        path %>%
        count_lines()

    data <-
        path %>%
        read_fwf(column_positions, column_types, skip = 1, n_max = line_count - 1 - 1)

    data <-
        data %>%
        mutate(across(all_of(without_decimal_columns), magrittr::divide_by, e2 = 100))

    data <-
        data %>%
        group_by(codneg) %>%
        arrange(date) %>%
        mutate(return = preult / lag(preult) - 1) %>%
        ungroup()

    data <-
        data %>%
        select(asset = codneg, date, return) %>%
        arrrange(date)

    return(data)
}

count_lines <- function(
    path
) {
    path %>%
        read_file() %>%
        str_count("\\n") %>%
        return()
}
