.BCB_DATA_ARGS <- tribble(
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

.IF_DATA_ARGS <- c(
    "CDI",
    "SELIC",
    "IPCA",

    "Ibovespa",
    "IBrX 50",
    "IBrX",
    "IBrA",
    "IDIV",
    "IFIX",

    "Giant Axis",
    "Giant Darius",
    "Giant Sigma",
    "Giant Zarathustra",
    "Pandhora Essencial"
)

.YAHOO_DATA_ARGS <- c(
    "B5P211.SA",
    "IRFM11.SA",
    "XFIX11.SA",
    "BRAX11.SA"
)
