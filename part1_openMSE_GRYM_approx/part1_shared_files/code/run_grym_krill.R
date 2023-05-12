# --------------------------------------------------------------------------------
# Run GRYM simulations for the assessment/managements of antarctic krill
# --------------------------------------------------------------------------------
# 
#  This is a wrapper on code provided for krill's assessement base-case, useful
#  for setting up Grym runs under different input scenarios. Specifically:
#   - Uses the projection function `KrillProjection()`
#   - Derives gammas based on decision rules
#
# --------------------------------------------------------------------------------

run_grym_krill <- function(nsteps, Ages, spawnI, monitorI, fishingI,
        t0, K, Linf, f0, f1, a, b, sel50Min, sel50Max, selrange,
        mat50Min, mat50Max, matrange, B0logsd, prRecruitPars, prRecruit, 
        gamma, n.years, ms, Fmax, n_iter, scenario_id, pr_scen_id, 
        mat_scen_id, outputs_path, ...){
  
  cli::cli_h2("Running GRYM for scenario {scenario_id} [{pr_scen_id}:{mat_scen_id}]")

  # ---- Set-up projection function
  Project <- KrillProjection(nsteps, Ages, spawnI, monitorI, fishingI, 
    t0, K, Linf, f0, f1, a, b, sel50Min, sel50Max, selrange, 
    mat50Min, mat50Max, matrange, B0logsd, prRecruitPars, prRecruit, 
    gamma, n.years, ms, Fmax)
  
  
  # ---- Run projections for all gamma values, paralellizing over iterations
  
  cli::cli_alert_info("Running projections")
  
  #future::plan(multisession, workers = future::availableCores()-1)

  # set progressor counter  
  p <- progressr::progressor(steps = n_iter)
  
  # run projections (for all gammas) 
  scen_proj <- furrr::future_map_dfr(
    1:n_iter, function(x){
      out <- Project(x)
      p()
      out
    },
    .options = furrr_options(seed = TRUE)
  ) |>
    mutate(scenario_id = scenario_id, .before = 1)
  
 # future::plan(sequential)
  
  #cat("\n\n")
  cli::cli_alert_success("Finished projections for scenario {scenario_id}")
  
  
  # ----  Deriving gammas (using code from workshop)
  cli::cli_alert_info("Deriving gamma1, gamma2, and final chosen gamma")

  gamma_results <- list()
  #gamma_results$Scenario <- scenario_id
  gamma_results$dep_rule <- scen_proj |>
    group_by(Gamma, Run) |>
    summarize(min_pop_status = min(SSB/SSB0), .groups = "drop_last") |>
    summarize(Pr_depleted = mean(min_pop_status < 0.2)) |>
    mutate(scenario_id = scenario_id, .before = 1)

  # Given the gamma values tested, gamma 1 is:
  gamma_results$Gamma_1 <- max(gamma_results$dep_rule$Gamma[gamma_results$dep_rule$Pr_depleted<=0.1])

  # What is the approximate Gamma that meets 10% depletion to test.
  gamma_results$test_gamma_1 <- approx(gamma_results$dep_rule$Pr_depleted,gamma_results$dep_rule$Gamma,0.1, ties = mean)$y

  # Gamma 2:
  gamma_results$esc_rule <- scen_proj |>
    group_by(Gamma) |>
    filter(Year %in% max(Year)) |>
    summarise(ssb_med = median(SSB), ssb0_med = median(SSB0)) |>
    mutate(
      Escapement = ssb_med/ssb0_med,
      scenario_id = scenario_id
    ) 
  
  #gamma_results$esc_rule$Escapement<-gamma_results$esc_rule$ssb_med/gamma_results$esc_rule$ssb0_med
  
  #Given the gamma values tested, gamma 2 is:
  gamma_results$Gamma_2<-max(gamma_results$esc_rule$Gamma[gamma_results$esc_rule$Escapement>=0.75])

  #What is the approximate Gamma that meets 75% escapement to test.
  gamma_results$test_gamma_2 <- approx(gamma_results$esc_rule$Escapement,gamma_results$esc_rule$Gamma,0.75, ties = mean)$y

  #The actual Gamma is the smallest of the two gammas:
  gamma_results$GammaToUse <- which(c(gamma_results$Gamma_1,gamma_results$Gamma_2)==
                                      min(gamma_results$Gamma_1,gamma_results$Gamma_2)) #Which gamma is min?
  if(length(gamma_results$GammaToUse)==2){gamma_results$GammaToUse=3} #when gamma1 and gamma2 are equal
  gamma_results$Selected_gamma<-as.data.frame(cbind(gamma_results$Gamma_1, gamma_results$Gamma_2,
                                                    gamma_results$GammaToUse, scenario_id))
  names(gamma_results$Selected_gamma) <- c("Gamma_1", "Gamma_2", "Gamma_choice", "scenario_id")

  # writing out projections
  cli::cli_alert_info("Writing out results")

  results <- list(scen_proj = scen_proj, gamma_results = gamma_results)

  write_rds(
    results,
    file = fs::path(outputs_path, glue::glue("grym_results_{scenario_id}_{pr_scen_id}_{mat_scen_id}.rds")),
    compress = "gz",
    compression = 9
  )
  
  results
}
