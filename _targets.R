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
            "IMA-B 5 P2",
            "IRF-M",
            #"IRF-M 1",
            #"IRF-M 1+",
            "IRF-M P2"
        )
    ),
    tar_target(
        bond_analysis,
        analyze(complemented_assets, assets = bonds, benchmark = benchmark)
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
        equity_analysis,
        analyze(complemented_assets, assets = equities, benchmark = benchmark)
    ),
    ## Funds -------------------------------------------------------------------
    tar_target(
        funds,
        c(
            "Giant Zarathustra + Sigma",
            "Giant Darius + Sigma",
            "Pandhora Essencial"
        )
    ),
    tar_target(
        fund_analysis,
        analyze(complemented_assets, assets = funds, benchmark = benchmark)
    ),
    ## Index Portfolio ---------------------------------------------------------
    tar_target(
        indexes,
        c(
            "IBrX",
            "IFIX",
            "IMA-B 5 P2"
        )
    ),
    tar_target(
        index_analysis,
        analyze(
            complemented_assets,
            assets = indexes,
            benchmark = benchmark,
            portfolio = "Index portfolio",
            weights = "naive risk contribution"
        )
    ),
    ## Index + Darius-Sigma Portfolio ------------------------------------------
    tar_target(
        indexes_darius_sigma,
        c(
            "IBrX",
            "IFIX",
            "IMA-B 5 P2",
            "Giant Darius + Sigma"
        )
    ),
    tar_target(
        index_darius_sigma_analysis,
        analyze(
            complemented_assets,
            assets = indexes_darius_sigma,
            benchmark = benchmark,
            portfolio = "Index + Darius-Sigma portfolio",
            weights = "naive risk contribution"
        )
    ),
    ## Index + Zarathustra-Sigma Portfolio -------------------------------------
    tar_target(
        indexes_zarathustra_sigma,
        c(
            "IBrX",
            "IFIX",
            "IMA-B 5 P2",
            "Giant Zarathustra + Sigma"
        )
    ),
    tar_target(
        index_zarathustra_sigma_analysis,
        analyze(
            complemented_assets,
            assets = indexes_zarathustra_sigma,
            benchmark = benchmark,
            portfolio = "Index + Zarathustra-Sigma portfolio",
            weights = "naive risk contribution"
        )
    ),
    # Dashboard ----------------------------------------------------------------
    tar_render_rep(
        dashboard,
        here("dashboard/dashboard.Rmd"),
        params = tibble::tibble(
            analysis = list(
                bond_analysis,
                equity_analysis,
                fund_analysis,
                index_analysis,
                index_darius_sigma_analysis,
                index_zarathustra_sigma_analysis
            ),
            output_file = c(
                here("dashboard/dashboard-bond.html"),
                here("dashboard/dashboard-equity.html"),
                here("dashboard/dashboard-fund.html"),
                here("dashboard/dashboard-index.html"),
                here("dashboard/dashboard-index-darius-sigma.html"),
                here("dashboard/dashboard-index-zarathustra-sigma.html")
            )
        )
    )
)
