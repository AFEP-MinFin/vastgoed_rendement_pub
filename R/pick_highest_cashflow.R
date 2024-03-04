#' Pick Highest Cashflow
#'
#' For each unique id and policy variant, one of multiple mutually exclusive cash flow options is chosen.
#' The chosen option is the one that has the highest total discounted value.
#' The discounting is perfomed with the provided discount value.
#'
#' @param houses Houses df
#' @param discount_rate Single value such as 0.058

#' @param ... Arbitrary number of cashflow generating functions
#' @param name Optional name for the resulting column
#'
#' @return A single cashflow
#' @export


pick_highest_cashflow <- function(houses, discount_rate, ..., name = 'highest_cashflow') {
  funs <- list(...)

  nyears <- length(unique(houses$simulation_year))
  discount_single_house <- rep(1 + discount_rate, nyears)

  nhouses <- length(unique(houses$huis_id))
  discount_all <- rep(discount_single_house, nhouses)

  flows <- lapply(funs, function(x) x(houses)) %>%
    bind_cols()

  discounted_flows <-
    flows %>%
    mutate(
      across(everything(), ~.x / discount_all)
    ) %>%
    bind_cols(
      houses %>% select(huis_id, beleidsvariant)
    )

  discounted_flows_sum <-
    discounted_flows %>%
    group_by(
      huis_id,
      beleidsvariant
    ) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-huis_id, -beleidsvariant)

  group_max_flow_idx <- apply(discounted_flows_sum, 1, which.max)
  # Make the vector the same length as houses by repeating the value for nyears.
  max_flow_idx <- unlist(lapply(group_max_flow_idx, rep, nyears))
  # Get the cash value for every year of the chosen column.
  max_flows <- unlist(
    mapply(
    function(row, idx) flows[row, idx], seq_len(nrow(flows)),
    max_flow_idx)
  )


res <-
  tibble(
    highest_cashflow = max_flows
  )
colnames(res)[1] <- name

return(res)
}


pick_highest_cashflow_mem <- memoise::memoise(pick_highest_cashflow)

#' @export
call_pick_highest_cashflow_mem <- function(
    houses,
    discount_rate,
    cashflow_funcs
    ) {
  do.call(
    pick_highest_cashflow,
    c(
      list(
        houses = houses,
        discount_rate = discount_rate
      ),
      cashflow_funcs
    )
  )
}

