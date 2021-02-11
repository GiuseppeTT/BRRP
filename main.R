# Libraries --------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(glue)
library(ggrepel)
library(slider)

library(here)



# Functions --------------------------------------------------------------------
here("R/") %>%
    fs::dir_ls() %>%
    walk(source)



# Analysis ---------------------------------------------------------------------
## Load data -------------------------------------------------------------------
Quandl::Quandl.api_key(Sys.getenv("QUANDL_API_KEY"))

BCB_data <- download_BCB_data()
IF_data <- download_IF_data()

assets <-
    bind_rows(BCB_data, IF_data) %>%
    arrange(asset, date)

assets <-
    assets %>%
    bind_rows(
        fake_asset(assets, "Giant Zarathustra", "Giant Darius"),
        fake_asset(assets, "Giant Axis", "Giant Sigma")
    ) %>%
    arrange(asset, date)

assets <-
    assets %>%
    bind_rows(
        merge_asset(assets, c("Fake Giant Darius", "Fake Giant Sigma"), "Giant Mix (Darius)"),
        merge_asset(assets, c("Giant Zarathustra", "Fake Giant Sigma"), "Giant Mix (Zara)")
    ) %>%
    arrange(asset, date)

old_selection <- c(
    "Fake IMA-B 5 P2",
    "IFIX",
    "IBrX"
)

new_selection <- c(
    "Fake IMA-B 5 P2",
    "IFIX",
    "IBrX",
    "Giant Mix (Darius)"
)

new_selection2 <- c(
    "Fake IMA-B 5 P2",
    "IFIX",
    "IBrX",
    "Giant Mix (Zara)"
)

assets <-
    assets %>%
    bind_rows(
        build_rp_portfolio(assets %>% filter_asset(old_selection), portfolio_name = "Portfolio Old"),
        build_rp_portfolio(assets %>% filter_asset(new_selection), portfolio_name = "Portfolio New (Darius)"),
        build_rp_portfolio(assets %>% filter_asset(new_selection2), portfolio_name = "Portfolio New (Zara)"),
    ) %>%
    arrange(asset, date)

## Bond ------------------------------------------------------------------------
bonds <- c(
    "IMA",
    "IMA-S",
    "IMA-B",
    "IMA-B 5",
    "IMA-B 5+",
    "Fake IMA-B 5 P2",
    "IRF-M",
    "IRF-M 1",
    "IRF-M 1+",
    "Fake IRF-M P2"
)

assets %>%
    filter_asset(bonds) %>%
    quick_analyze()

## Equities --------------------------------------------------------------------
equities <- c(
    "IBrX 50",
    "Ibovespa",
    "IBrX",
    "IBrA",
    "IDIV"
)

assets %>%
    filter_asset(equities) %>%
    quick_analyze()

## Funds -----------------------------------------------------------------------
# TODO: add fake Darius, Sigma, Giant and Quant to assets
funds <- c(
    "Giant Axis",
    "Fake Giant Darius",
    "Fake Giant Sigma",
    "Giant Zarathustra",
    "Pandhora Essencial"
)

assets %>%
    filter_asset(funds) %>%
    quick_analyze()

## Old Selection ---------------------------------------------------------------
old_selection <- c(
    "Fake IMA-B 5 P2",
    "IFIX",
    "IBrX",
    "Portfolio Old"
)

assets %>%
    filter_asset(old_selection) %>%
    quick_analyze()

assets %>%
    filter_asset(old_selection[-4]) %>%
    plot_rolling_risk_contribution(days(365), threshold = 0.1)

assets %>%
    filter_asset(old_selection[-4]) %>%
    table_risk_contribution()

## New Selection ---------------------------------------------------------------
new_selection <- c(
    "Fake IMA-B 5 P2",
    "IFIX",
    "IBrX",
    "Giant Mix (Darius)",
    "Portfolio New (Darius)"
)

assets %>%
    filter_asset(new_selection) %>%
    quick_analyze()

assets %>%
    filter_asset(new_selection[-5]) %>%
    plot_rolling_risk_contribution(days(365), threshold = 0.1)

assets %>%
    filter_asset(new_selection[-5]) %>%
    table_risk_contribution()

assets %>%
    filter_asset(new_selection[-5]) %>%
    check_risk_weights()

## New Selection 2 -------------------------------------------------------------
new_selection2 <- c(
    "Fake IMA-B 5 P2",
    "IFIX",
    "IBrX",
    "Giant Mix (Zara)",
    "Portfolio New (Zara)"
)

assets %>%
    filter_asset(new_selection2) %>%
    quick_analyze()

assets %>%
    filter_asset(new_selection2[-5]) %>%
    plot_rolling_risk_contribution(days(365), threshold = 0.1)

assets %>%
    filter_asset(new_selection2[-5]) %>%
    table_risk_contribution()

## Investable Selection --------------------------------------------------------
investables <- c(
    "B5P211",
    "Giant",
    "XFIX11",
    "BRAX11",
)

assets %>%
    filter_asset(investables) %>%
    quick_analyze()

assets %>%
    filter_asset(investables) %>%
    plot_rolling_risk_contribution(days(365), threshold = 0.1)

assets %>%
    filter_asset(investables) %>%
    table_risk_contribution()

