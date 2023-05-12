#' Function to plot the values of a management performance metric (e.g.
#' depletion probability, escapement level) under a range of considered harvest
#' rates (gammas), for a set of alternative scenarios
#' 
#' @param dt data.frame with values of the performance metric at a range of
#'   harvest rates (`gamma`)
#' @param gamma <data-masking> Name of the column with considered harvest rates
#' @param rule_value <data-masking> Name of the column with metric values
#' @param scen <data-masking> Name of the column scenario identification
#' @param thresh numeric, the critical sustainability threshold for the
#'   considered metric, displayed as an horizontal dashed line in the plot
#' @param title character string, plot title
#' @param ylab
#' @param xlab

plot_gammas_vs_rule <- function(dt, gamma, rule_value, scen, thresh, title, 
                                scen_label = "Scenario", ylab, xlab){
  
  n_scenarios <- pull(dt, {{scen}}) |> unique() |> length()
  
  dt |>
    ggplot(aes(x = {{gamma}}, y = {{rule_value}}, colour = {{scen}})) +
    geom_line() +
    geom_point(aes(fill = {{scen}}), shape = 21, colour = "white", size = 1.5, alpha = 0.5) +
    geom_hline(yintercept = thresh, linetype = "dashed") +
    labs(x = xlab, y = ylab, title = title, fill = scen_label, colour = scen_label) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_color_manual(values = MetBrewer::met.brewer("Juarez", n = n_scenarios)) +
    scale_fill_manual(values = MetBrewer::met.brewer("Juarez", n = n_scenarios)) +
    theme(panel.grid.minor.y = element_blank())
}
