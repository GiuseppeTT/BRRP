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
config_targets <- list(
    tar_file(
        config_path,
        here("config.yaml")
    ),
    tar_target(
        config,
        read_config(config_path)
    )
)

data_targets <- list(
    tar_target(
        downloaded_data,
        download_data()
    ),
    tar_target(
        complemented_data,
        complement_data(downloaded_data)
    )
)
analysis_targets <- list(
    tar_target(
        analysis_data,
        prepare_data_for_analysis(
            complemented_data,
            config$assets[[1]],
            config$benchmark,
            config$portfolio,
            config$weights
        ),
        pattern = map(config),
        iteration = "list"
    ),
    tar_target(
        analysis,
        analyze(analysis_data),
        pattern = map(analysis_data),
        iteration = "list"
    )
)

dashboard_targets <- list(
    tar_file(
        base_analysis_path,
        here("dashboard/base-analysis.Rmd")
    ),
    tar_file(
        portfolio_analysis_path,
        here("dashboard/portfolio-analysis.Rmd")
    ),
    tar_target(
        output_file,
        here("docs", name_dashboard_file(config$name)),
        pattern = map(config)
    ),
    tar_render_rep(
        dashboard,
        here("dashboard/dashboard.Rmd"),
        params = tibble::tibble(
            base_analysis_path = base_analysis_path,
            portfolio_analysis_path = portfolio_analysis_path,
            analysis = analysis,
            output_file = output_file
        )
    )
)

list(
    config_targets,
    data_targets,
    analysis_targets,
    dashboard_targets
)
