# Libraries --------------------------------------------------------------------
library(targets)
library(tarchetypes)

library(here)

`%>%` <- magrittr::`%>%`



# Functions --------------------------------------------------------------------
here("R/") %>%
    fs::dir_ls() %>%
    purrr::walk(source)



# Options ----------------------------------------------------------------------
options(dplyr.summarise.inform = FALSE)
tar_option_set(packages = c("lubridate", "tidyverse"))



# Targets ----------------------------------------------------------------------
list(
    # Data ---------------------------------------------------------------------
    tar_target(
        downloaded_assets,
        download_data()
    ),
    tar_target(
        complemented_assets,
        complement_data(downloaded_assets)
    ),
    # Analysis -----------------------------------------------------------------
    tar_target(
        benchmark,
        "CDI"
    ),
    ## Bonds -------------------------------------------------------------------
    tar_target(
        bonds,
        c(
            "IMA",
            #"IMA-S",
            "IMA-B",
            #"IMA-B 5",
            #"IMA-B 5+",
            "Fake IMA-B 5 P2",
            "IRF-M",
            #"IRF-M 1",
            #"IRF-M 1+",
            "Fake IRF-M P2"
        )
    ),
    tar_target(
        base_bond_analysis,
        base_analyze(complemented_assets, assets = bonds, benchmark = benchmark)
    ),
    ## Equities ----------------------------------------------------------------
    tar_target(
        equities,
        c(
            "IBrX 50",
            "Ibovespa",
            "IBrX",
            "IBrA"#,
            #"IDIV"
        )
    ),
    tar_target(
        base_equity_analysis,
        base_analyze(complemented_assets, assets = equities, benchmark = benchmark)
    ),
    ## Funds -------------------------------------------------------------------
    tar_target(
        funds,
        c(
            "Giant Zarathustra + Extended Giant Sigma",
            "Extended Giant Darius + Extended Giant Sigma",
            "Pandhora Essencial"
        )
    ),
    tar_target(
        base_fund_analysis,
        base_analyze(complemented_assets, assets = funds, benchmark = benchmark)
    ),
    ## Index Portfolio ---------------------------------------------------------
    tar_target(
        indexes,
        c(
            "IBrX",
            "IFIX",
            "Fake IMA-B 5 P2"
        )
    ),
    tar_target(
        base_index_analysis,
        base_analyze(
            complemented_assets,
            assets = indexes,
            benchmark = benchmark,
            portfolio = "Index portfolio",
            weights = "inverse volatility"
        )
    ),
    ## Index + Darius-Sigma Portfolio ------------------------------------------
    tar_target(
        indexes_darius_sigma,
        c(
            "IBrX",
            "IFIX",
            "Fake IMA-B 5 P2",
            "Extended Giant Darius + Extended Giant Sigma"
        )
    ),
    tar_target(
        base_index_darius_sigma_analysis,
        base_analyze(
            complemented_assets,
            assets = indexes_darius_sigma,
            benchmark = benchmark,
            portfolio = "Index + Darius-Sigma portfolio",
            weights = "inverse volatility"
        )
    ),
    ## Index + Zarathustra-Sigma Portfolio -------------------------------------
    tar_target(
        indexes_zarathustra_sigma,
        c(
            "IBrX",
            "IFIX",
            "Fake IMA-B 5 P2",
            "Giant Zarathustra + Extended Giant Sigma"
        )
    ),
    tar_target(
        base_index_zarathustra_sigma_analysis,
        base_analyze(
            complemented_assets,
            assets = indexes_zarathustra_sigma,
            benchmark = benchmark,
            portfolio = "Index + Zarathustra-Sigma portfolio",
            weights = "inverse volatility"
        )
    ),
    # Dashboard ----------------------------------------------------------------
    tar_target(
        base_analysis_template,
        here("dashboard/base-analysis-template.Rmd"),
        format = "file"
    ),
    tar_target(
        portfolio_analysis_template,
        here("dashboard/portfolio-analysis-template.Rmd"),
        format = "file"
    ),
    tar_render(
        dashboard,
        here("dashboard/dashboard.Rmd"),
        params = list(
            base_analysis_template = base_analysis_template,
            portfolio_analysis_template = portfolio_analysis_template
        )
    )
)
