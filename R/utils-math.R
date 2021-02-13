semi_deviation <- function(
    x
) {
    x_downside <- x[x < 0]
    x_semi_deviation <- sd(x_downside)

    return(x_semi_deviation)
}

rolling_return <- function(
    x,
    index,
    window_size
) {
    x %>%
        slider::slide_index_dbl(
            index,
            ~ prod(1 + .x) - 1,
            .before = window_size,
            .complete = TRUE
        ) %>%
        return()
}

drawdown <- function(
    x
) {
    return((x - cummax(x)) / cummax(x))
}
