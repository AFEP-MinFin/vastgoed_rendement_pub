#' Register in Houses the Values of Selling and Renting, and indicate which one is preferable
#'
#'
#' @param houses Houses df
#' @param sale_func
#' @param keep_rent_func
#'
#' @return A single cashflow
#' @export


pick_sale_or_rent <- function(houses, parameters, funcs) {

  nyears <- length(unique(houses$simulation_year))
  discount_single_house <- rep(1 + parameters$discount_rate, nyears)

  nhouses <- length(unique(houses$huis_id))
  discount_all <- rep(discount_single_house, nhouses)

  flows <- lapply(
    list(funcs$sale_func[[1]], funcs$keep_rent_func[[1]]), function(x) x(houses)
    ) %>%
    bind_cols()

  taxes <- lapply(
      list(funcs$sale_tax_func[[1]], funcs$keep_rent_tax_func[[1]]),
      function(x) x(houses)
    ) %>%
    bind_cols()

  flows_after_tax <- flows + taxes

  discounted_flows <-
    flows_after_tax %>%
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

  idx_to_name <- tidyr::tribble(
    ~max_flow_idx, ~rent_or_sell,
    1, 'sell',
    2, 'keep_for_rent'
  )

  res <-
    tibble(
      sale_cashflow = flows[[1]],
      rent_cashflow = flows[[2]],
      max_flow_idx = max_flow_idx
    ) %>%
    left_join(
      idx_to_name,
      by = 'max_flow_idx'
    )

  return(res)
}

get_sale_or_rent_cashflow <- function(houses) {
  houses %>%
    mutate(
      end_transaction = case_when(
        rent_or_sell == 'sell' ~ sale_cashflow,
        rent_or_sell == 'keep_for_rent' ~ rent_cashflow
      )
    ) %>%
    select(
      end_transaction
    )
}

