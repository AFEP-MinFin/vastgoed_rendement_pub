#' Functions to analyse the rent outcomes of a policy versus rents necessary to get a particular return
#'
#'
#' @param return_variation a list with 'name' and 'returns' members
#' @param goal_return the average investor return goal
#'
#' @export

compare_rents_to_return_goal <- function(return_variation, goal_return = 0.0513) {
  outcome_versus_goal_data <- calculate_rent_for_return(
    return_variation,
    goal_return
  )

  list(
    data = outcome_versus_goal_data,
    counts = count_rent_deviations(outcome_versus_goal_data),
    histogram = plot_wws_rent_frac(outcome_versus_goal_data),
    woz_plot = plot_return_against_woz(outcome_versus_goal_data)
  )
}

#' @param return_variation a list with 'name' and 'returns' members
#' @param goal_return the average investor return goal
#'
#' @export
calculate_rent_for_return <- function(returns_variation, goal_return = 0.0513) {
  .returns_data = returns_variation$returns
  .returns_data$houses %>%
    bind_cols(.returns_data$cashflows %>% select(-huis_id, -beleidsvariant)) %>%
    left_join(.returns_data$stats %>% select(irr), by = 'huis_id') %>%
    group_by(huis_id) %>%
    slice(1) %>%
    mutate(rent_cat = cut(wws_points, c(-Inf, 142, 187, Inf), c('social', 'middle', 'free'))) %>%
    mutate(
      irr_diff = goal_return - irr,
      rent_diff = aankoop * -1 * irr_diff,
      rent = rent_income / 12,
      year_rent_for_return = rent_income + rent_diff,
      rent_for_return = year_rent_for_return / 12,
      wws_rent = wws_rent / 12,
      diff_wws_return_rent = wws_rent - rent_for_return,
      frac_wws_return_rent = ((wws_rent / rent_for_return)) * 100,
      frac_rent_return_rent = ((rent / rent_for_return)) * 100,
      name = returns_variation$name
    ) %>%
    select(name, rent_for_return, wws_rent, rent, rent_cat, diff_wws_return_rent, frac_wws_return_rent, frac_rent_return_rent, woz_indexed) %>%
    filter(is.finite(rent_cat))
}

count_rent_deviations <- function(.data) {
  .data %>%
    group_by(
      wws_cat = cut(
        frac_wws_return_rent,
        c(0, 50, 80, 100, 120, 150, Inf),
        c(
          'Ver onder marktconform rendement',
          'Onder marktconform rendement',
          'Net onder marktconform rendement',
          'Net boven marktconform rendement',
          'Boven marktconform rendement',
          'Ver boven marktconform rendement'
        )
      )
    ) %>%
    count(wws_cat, rent_cat) %>%
    group_by(rent_cat) %>%
    mutate(perc = n / sum(n))
}

plot_wws_rent_frac <- function(.data) {
  ggplot(
    .data %>% filter(frac_wws_return_rent < 200),
    aes(
      x = frac_wws_return_rent
    )
  ) +
    geom_histogram(bins = 100) +
    facet_grid(~rent_cat) +
    geom_vline(xintercept = 100, linetype = 'dashed', color = 'grey20') +
    theme_bw() +
    coord_cartesian(xlim = c(0, 150)) +
    xlab('% WWS huur van huur marktconform rendement') +
    ylab('aantal woningen') +
    ggtitle(.data$name[1])
}

plot_return_against_woz <- function(.data) {
  .data %>% filter(frac_wws_return_rent < 200) %>%
    ggplot(aes(x = woz_indexed, frac_rent_return_rent)) +
    geom_point(aes(shape = rent_cat, color = rent_cat), alpha = 0.2) +
    geom_smooth() +
    coord_cartesian(xlim = c(0, 700000), ylim = c(0, 150)) +
    geom_hline(yintercept = 100, linetype = 'dashed', color = 'grey50') +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 700000, 100000)) +
    ylab('% (Gemaximeerde) huur van huur marktconform rendement')  +
    ggtitle(.data$name[1])
}
