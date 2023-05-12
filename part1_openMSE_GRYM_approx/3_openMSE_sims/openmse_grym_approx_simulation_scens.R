# ------------------------------------------------------------------------------------------------
#  Script to run simulations (historical period and forward projections) under
#  the openMSE approach as an approximation to the Grym framework, and
#  specifically, its base-case application to krill, for a set of alternative
#  input scenarios
#
#  Outputs and the final MSE step, where results are evaluated by performance
#  metrics, are shown in the quarto document `openmse_sims.qmd`
# -----------------------------------------------------------------------------------------------

#' ------------------------
#'        Preamble
#' ------------------------


#remotes::install_github("https://github.com/bcaneco/MSEtool")

library(openMSE)
library(Grym)
library(tidyverse)
library(readxl)
library(tictoc)
library(cli)
library(rlang)
library(mvtnorm)
library(openxlsx)


model_outputs_path <- "part1_openMSE_GRYM_approx/part1_shared_files/outputs/openmse/model_outputs/dynB0_MPs_2yrhist"
fs::dir_create(outputs_path)

results_path <- "part1_openMSE_GRYM_approx/part1_shared_files/outputs/openmse"

# Load scenario setups
grym_setups <- read_rds("part1_openMSE_GRYM_approx/part1_shared_files/inputs/grym_scen_setups.rds")
openmse_scen_OMs <- read_rds("part1_openMSE_GRYM_approx/part1_shared_files/inputs/openmse_scen_OMs.rds")


# load functions defining management plans for each gamma and performance metrics
source("part1_openMSE_GRYM_approx/3_openMSE_sims/build_gammaB0_dynamic.R")



#' --------------------------------
#'     Run MSE for each scenario
#' --------------------------------

openmse_scen_mse <- openmse_scen_OMs |>
  imap(\(x, y){
    
    #x <- openmse_scen_OMs$`scn-1`
    #y <- "scn-1"
    # x@nsim <- 2000
    
    cli::cli_h1("Starting MSE run for {y} @ {Sys.time()}")
    
    # Simulate observational errors in survey estimates of B0. `B0logsd` is the
    # SD of survey estimates of (log) B0 conveying the  observational error in
    # surveys, assumed to be log-Normally distributed. `B0logsd` is calculated
    # externally from survey data.
    b0lgsd <- grym_setups |> filter(scenario_id == y) |> pull(B0logsd)
    B0err_draws <- rlnorm(x@nsim, -b0lgsd^2/2, b0lgsd)
    
    # build MPs for considered gammas
    gammas <- grym_setups |> filter(scenario_id == y) |> pull(gamma) |> pluck(1)
    gamma_B0_MPs <- build_gammaB0(
      gammas = gammas, 
      nyears = x@nyears,
      B0err = B0err_draws)
    
    # run MSE for current scenario    
    tictoc::tic()
    mse_output <- runMSE(
      OM = x, 
      MPs = gamma_B0_MPs,
      parallel = "sac"
    )
    
    runtime <- tictoc::toc(quiet = TRUE)
    
    # write out mse outputs
    write_rds(
      mse_output,
      file = fs::path(model_outputs_path, glue::glue("openmse_mse_outputs_{y}.rds")), 
      compress  = "gz"
    )
    
    cli::cli_alert_success("Finished MSE for {y}: {runtime$callback_msg}")
    
    return(mse_output)
  })





#' ---------------------------------------------------------------------
#'             Simulation results and gamma selection
#' ---------------------------------------------------------------------
 
# ------------------------------------------------------------------------------
# --- Utility function to extract relevant results and perform gammas selection

mse_extract_gammas <- function(mseObj_file, SSB0_type = "equilibrium"){

  cli::cli_alert("\n Processing file {mseObj_file} @ {Sys.time()}")
  
  mse_out <- read_rds(mseObj_file)
  
  # SSB0 data and source metrics functions, based on choice of type of reference point
  if(SSB0_type == "equilibrium"){
    SSB0 <- mse_out@OM$SSB0  
    source("part1_openMSE_GRYM_approx/3_openMSE_sims/krill_mngnt_ccamlr_metrics_equilibrium.R")
  }else 
    if(SSB0_type == "dyn_unfished"){
      SSB0 <- mse_out@RefPoint$Dynamic_Unfished$SSB0[, mse_out@nyears]
      source("part1_openMSE_GRYM_approx/3_openMSE_sims/krill_mngnt_ccamlr_metrics_dynamic.R")
    }
  
  # get considered gammas
  gammas <- as.numeric(str_replace(mse_out@MPs, "gammaB0_", ""))
  
  
  # extract projected SSB and cast it into a dataframe
  dimnames(mse_out@SSB) <- list(sim = 1:mse_out@nsim, gamma = gammas, Year = 1:mse_out@proyears)
  ssb_proj <- reshape2::melt(mse_out@SSB, value.name = "SSB") |> as_tibble()
  ssb0 <- tibble(
    sim = 1:mse_out@nsim, 
    SSB0 = SSB0
  )
  
  # merge in SSB0, calculate yearly spawning stock status (SSS), and identify
  # simulations where SSS < 0.2 at any point in the time-series
  ssb_proj <- left_join(ssb_proj, ssb0, by = "sim") |>
    group_by(sim, gamma) |>
    mutate(
      SSS = SSB/SSB0,
      below_dpl = if_else(min(SSS) < 0.2, TRUE, FALSE)
    ) |>
    ungroup()
  
  # Compute depletion rule metric for each gamma
  dep_metric <- PD(mse_out)
  
  # Compute escapement rule metric for each gamma
  esc_metric <- ESC(mse_out)
  
  mse_metrics <- tibble(gamma = gammas, PD = dep_metric@Mean, ESC = esc_metric@Mean)
  
  # Derive gammas that satisfy the depletion and the escapement rules, and the
  # final gamma_s as min(gamma_1, gamma_2)
  gamma_results <- mse_metrics |>
    summarise(
      gamma1 = max(max(gamma[PD <= 0.1]), 0), 
      gamma1_approx = approx(PD, gamma, 0.1)$y, 
      gamma2 = max(gamma[ESC >= 0.75]),
      gamma2_approx = approx(ESC, gamma, 0.75)$y
    ) |>
    mutate(selected_gamma = if_else(gamma1 < gamma2, 1, 2))
  
  # return results in tibble with list-columns
  tibble(
    ssb_proj = list(ssb_proj), dep_metric = list(dep_metric), 
    esc_metric = list(esc_metric), mse_metrics = list(mse_metrics), 
    gamma_results = list(gamma_results)
  )
}


#-------------------------------------------------------------------------
# ---    Results and gammas for equilibrium SSB0 and dynamic B0

openmse_2yrhst_dynB0_eqlSSB0_scen <- fs::dir_ls(model_outputs_path, regexp = "scn") |>
  map_df(
    ~mse_extract_gammas(.x, SSB0_type = "equilibrium"), 
    .id = "fname") |>
  mutate(scen_id = str_extract(fname, "scn-\\d+"), .before = 1, .keep = "unused")


openmse_2yrhst_dynB0_eqlSSB0_scen_gamma_select <- openmse_2yrhst_dynB0_eqlSSB0_scen |>
  select(scen_id, gamma_results) |>
  unnest(gamma_results) |>
  select(-contains("approx"))


openmse_2yrhst_dynB0_eqlSSB0_scen_metrics <- openmse_2yrhst_dynB0_eqlSSB0_scen |>
  select(scen_id, mse_metrics) |>
  unnest(mse_metrics)

openmse_2yrhst_dynB0_eqlSSB0_scen_ssb <- openmse_2yrhst_dynB0_eqlSSB0_scen |>
  select(scen_id, ssb_proj) |>
  unnest(ssb_proj)


# write-out
write_rds(
  openmse_2yrhst_dynB0_eqlSSB0_scen_metrics,
  file = fs::path(results_path, "openmse_2yrhst_dynB0_eqlSSB0_scen_metrics.rds")
)

write_rds(
  openmse_2yrhst_dynB0_eqlSSB0_scen_gamma_select,
  file = fs::path(results_path, "openmse_2yrhst_dynB0_eqlSSB0_scen_gamma_select.rds")
)

write_rds(
  openmse_2yrhst_dynB0_eqlSSB0_scen_ssb,
  file = fs::path(results_path, "openmse_2yrhst_dynB0_eqlSSB0_scen_ssb.rds"),
  compress = "gz"
)


openmse_2yrhst_dynB0_eqlSSB0_scen_ssb |>
  group_by(scen_id) |>
  summarise(min(SSS), max(SSS))



#-------------------------------------------------------------------------
# ---    Results and gammas for dynamic SSB0 and dynamic B0

openmse_2yrhst_dynB0_dynSSB0_scen <- fs::dir_ls(model_outputs_path, regexp = "scn") |>
  map_df(~mse_extract_gammas(.x, SSB0_type = "dyn_unfished"), .id = "fname") |>
  mutate(scen_id = str_extract(fname, "scn-\\d+"), .before = 1, .keep = "unused")


openmse_2yrhst_dynB0_dynSSB0_scen_gamma_select <- openmse_2yrhst_dynB0_dynSSB0_scen |>
  select(scen_id, gamma_results) |>
  unnest(gamma_results) |>
  select(-contains("approx"))


openmse_2yrhst_dynB0_dynSSB0_scen_metrics <- openmse_2yrhst_dynB0_dynSSB0_scen |>
  select(scen_id, mse_metrics) |>
  unnest(mse_metrics)

openmse_2yrhst_dynB0_dynSSB0_scen_ssb <- openmse_2yrhst_dynB0_dynSSB0_scen |>
  select(scen_id, ssb_proj) |>
  unnest(ssb_proj)


# write-out
write_rds(
  openmse_2yrhst_dynB0_dynSSB0_scen_metrics,
  file = fs::path(results_path, "openmse_2yrhst_dynB0_dynSSB0_scen_metrics.rds")
)

write_rds(
  openmse_2yrhst_dynB0_dynSSB0_scen_gamma_select,
  file = fs::path(results_path, "openmse_2yrhst_dynB0_dynSSB0_scen_gamma_select.rds")
)

write_rds(
  openmse_2yrhst_dynB0_dynSSB0_scen_ssb,
  file = fs::path(results_path, "openmse_2yrhst_dynB0_dynSSB0_scen_ssb.rds"),
  compress = "gz"
)




openmse_2yrhst_dynB0_dynSSB0_scen_ssb |>
  group_by(scen_id) |>
  summarise(min(SSS), max(SSS))






# openmse_scen_gamma_select
# 
# scn1_out <- read_rds("part1_openMSE_GRYM_approx/part1_shared_files/outputs/openmse/model_outputs/eqlbr_2yrhst/openmse_mse_outputs_scn-1.rds")
# 
# scn1_out
# 
# scn7_out <- read_rds("part1_openMSE_GRYM_approx/part1_shared_files/outputs/openmse/model_outputs/openmse_mse_outputs_scn-7.rds")
#  
# length(scn7_out@OM$SSB0)
# 
# sd(scn7_out@RefPoint$ByYear$SSB0[, 1])
# cv(scn7_out@RefPoint$ByYear$SSB0[, 1])
# 
# sd(scn7_out@RefPoint$Dynamic_Unfished$SSB0[, 5])
# cv(scn7_out@RefPoint$Dynamic_Unfished$SSB0[, 5])
# 
# sd(scn7_out@RefPoint$Dynamic_Unfished$SSB0[, 1])
# cv(scn7_out@RefPoint$Dynamic_Unfished$SSB0[, 1])
# 
# apply(scn7_out@RefPoint$Dynamic_Unfished$SSB0[, 1:5], 2, cv)
# apply(scn7_out@RefPoint$Dynamic_Unfished$B0[, 1:5], 2, cv)
# 
# 
# apply(scn7_out@RefPoint$ByYear$SSB0, 2, cv)


# scn7_out@OM$hs[442]
# scn7_out@SSB[442, 1, ]
# 
# scn7_out@RefPoint$Dynamic_Unfished$SSB0[442, 1:7]
# 
# plot(scn7_out@RefPoint$Dynamic_Unfished$SSB0[442, 1:7], type = "l")
# lines(scn7_out@SSB[442, 1, 1:2], col = "green")
# lines(scn7_out@SSB_hist[442, ], col = "red")
# 
# 
# scn7_out@OM$procsd[442]
# scn7_out@RefPoint$Dynamic_Unfished$Rec[442, ]
# scn7_out@N[442, 1, 1, , ]
# dim(scn7_out@N)
# scn7_out@RefPoint$Dynamic_Unfished$Rec[442, ]
# 
# 
# scn7_out@RefPoint$Dynamic_Unfished$SSB0[442, 1:7]
# scn7_out@SSB_hist[442, ]
# scn7_out@RefPoint$Dynamic_Unfished$Rec[442, 1:5]
# scn7_out@RefPoint$ByYear$SSB0[442, ]
# scn7_out@RefPoint$Dynamic_Unfished$SSB0[442, ]
# 
#  
# range(scn7_out@RefPoint$ByYear$B0[, 1])
# range(scn7_out@RefPoint$Dynamic_Unfished$B0[, 5])

