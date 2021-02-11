semi_deviation <- function(
    x
) {
    x_downside <- x[x < mean(x)]
    x_semid <- sd(x_downside)

    return(x_semid)
}

rolling_return <- function(
    x,
    index,
    window_size
) {
    x %>%
        slide_index_dbl(index, ~ prod(1 + .x) - 1, .before = window_size, .complete = TRUE) %>%
        return()
}

drawdown <- function(
    x
) {
    return((x - cummax(x)) / cummax(x))
}
