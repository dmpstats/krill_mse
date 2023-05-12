#' -------------------------------------------------------------------------
#' Aim: To approximate {openMSE} framework to the GRYM approach currently used
#'    in the stock assessment of the Antarctic krill. This involves:
#'    
#'    - Specify an Operating Model (OM), Management Plans (MPs) and Performance
#'    Metrics (PMs) that follow, as closely as possible, the workings of the
#'    GRYM framework applied in krill stock assessment.
#'    
#'    - Compare the two frameworks in terms of main outputs (i.e. estimated
#'    gammas) obtained under a recent krill assessment in FAO subArea 48.1
#'    (Maschette et al, 2021)
#'    
#'    - For robustness, comparisons are provided for a set of simulation
#'    scenarios, each under a different Proportional Recruitment premise
#'    
#'    - Simulation values under each scenario are generated in file 
#'    "recruitment_scenarios/generate_recCV_M_scenarios.qmd"
#' 
#' -------------------------------------------------------------------------

# -------------------- #
#       Preamble       #
# -------------------- #

library(DLMtool)
library(readxl)
library(openxlsx)
library(mvtnorm)
library(rlang)
library(tictoc)
library(cli)
library(tidyverse)
library(MetBrewer)
library(flextable)

source("openmse_grym_approximation/R/ccamlr_krill_mngnt_fcts.R")
source("openmse_grym_approximation/R/OMdoc_dmp.r")


maschette_selected_gammas <- read_csv("openmse_grym_approximation/grym_files/grym_selected_gammas_maschette.csv")


# --------------------------------------------------- #
#     1. Initialize base-case OM infrastructure       #
# --------------------------------------------------- #

# This step is only required to run once!

# initialize Operating Model for GRYM approximation 
OMinit("OM_grym_approx_base", overwrite = FALSE, dir = "openmse_grym_approximation/")


# ------------------------------------------------ #
#     2. Populate and document base-case OM        #
# ------------------------------------------------ #

# Step performed outside this script. Requires filling up the OM Excel workbook
# 'OM_grym_approx_base.xlsx' with the choice of inputs for model parameters
# while documenting those choices on the OM RMarkdown file
# 'OM_grym_approx_base.rmd'



# ----------------------------------- #
#       3. Import base-case OM        #
# ----------------------------------- #

# Import the Operating Model from the excel workbook
OM_krill_grym_base <- XL2OM("openmse_grym_approximation/OM_grym_approx_base.xlsx")


OM_krill_grym_base@M
OM_krill_grym_base@sigmaRbiascv

  

# -------------------------------------------------- #
#       4. Compile OM report for one scenario        #
# -------------------------------------------------- #

# Scenarios differ only in terms of parameters `M` and `Perr`, which are obtained
# from a specific scenario of Proportional Recruitment (PR).
#
# Simulation values of `M` and `Perr` generated from a given PR scenario are
# plugged-in to the openMSE model via the custom parameters `cpar` feature.
#
# So, using simulation values from one scenario to generate the OM report, for
# documentation purposes.

cvR_M_draws_scen <- read_rds("openmse_grym_approximation/recruitment_scenarios/cvR_M_draws_scen.rds")

# --- Add Custom parameters to base OM

# Switch off age-plus group
OM_krill_grym_base@cpars$plusgroup <- 0

# Simulation values for M and Perr, from proportional recruitment estimation process
sample_idx <- sample(OM_krill_grym_base@nsim)
cvR_M_draws <- cvR_M_draws_scen$Initial_values[sample_idx, ] #cvR_M_draws_scen$Initial_values[sample_idx, ]
OM_krill_grym_base@cpars$M <- cvR_M_draws$M
OM_krill_grym_base@cpars$Perr <- sqrt(log(cvR_M_draws$CV^2 + 1)) # converting CV from PR to lognormal SD

#Simulate(OM_krill_grym_base)

# Compile the Operating model report
OMdoc(
  OM_krill_grym_base, 
  rmd.source = "OM_grym_approx_base.rmd", 
  dir = "openmse_grym_approximation/", 
  openFile = TRUE
)


OMdoc_dmp(
  OM_krill_grym_base,
  rmd.source = "OM_grym_approx_base.rmd",
  dir = "openmse_grym_approximation/",
  out.file = "test", html_theme = "lumen",
  openFile = TRUE
)

# ----------------------------------------------------- #
#             5. Run MSE for each scenario              #
# ----------------------------------------------------- #

# Run an MSE for each scenario of proportional recruitment (PR), 
#
# For each MSE, the base OM is updated with a set of draws of recruitment
# variance and natural mortality generated from PR estimates from specific
# survey data

OM_krill_grym_scn <- OM_krill_grym_base

# overwriting number of projected years
OM_krill_grym_scn@proyears <- 20

OM_krill_grym_scn@nsim <- 2000  # nrow(cvR_M_draws_scen[[1]])

gammas <- seq(0, 0.17, by = 0.0025) #c(0, seq(0.0025, 0.19, by = 0.0025))
gamm_B0_MPs <- build_gammaB0(gammas)


krill_mse_scen <- cvR_M_draws_scen |>
  imap(function(x, y){
    
    cli::cli_h1("Starting MSE run for {y}")
    
    # x <- cvR_M_draws_scen[[4]]
    
    # M and Recruitment Variance, from proportional recruitment estimation
    cvR_M_draws <- mutate(x, M = M + 1e-5)
    
    OM_krill_grym_scn@cpars$M <- cvR_M_draws$M
    OM_krill_grym_scn@cpars$Perr <- sqrt(log(cvR_M_draws$CV^2 + 1)) # converting CV from PR to lognormal SD
  
    # This is required to switch off age-plus group
    OM_krill_grym_scn@cpars$plusgroup <- 0
    
    # random number seed
    set.seed(OM_krill_grym_scn@seed)
    
    tictoc::tic()
    mse_output <- runMSE(OM_krill_grym_scn, MPs = gamm_B0_MPs)# , parallel = TRUE)
    runtime <- tictoc::toc(quiet = TRUE)
    
      cli::cli_alert_success("Finished MSE for {y}: {runtime$callback_msg}")
    
    return(mse_output)
  })

write_rds(krill_mse_scen, "openmse_grym_approximation/krill_mse_scen.rds", compress = "gz")

# krill_mse_scen <-read_rds("openmse_grym_approximation/krill_mse_scen.rds")

# check fishing mortality/catches
matplot(t(apply(krill_mse_scen$Initial_values@FM, c(2, 3), median)), type = "l")
matplot(t(apply(krill_mse_scen$Initial_values@Catch, c(2, 3), median)), type = "l")



dim(krill_mse_scen$Initial_values@Catch)

# depletion samples vs biomass
krill_mse_scen$Initial_values@OM$Depletion



# ---------------------------------------- #
# ----    6. MPs Performance Metrics    ---- 
# ---------------------------------------- #

krill_mse_metrics <- map(
  krill_mse_scen, 
  ~ summary(., "PD", "ESC") |> mutate(gamma = gammas)
)


krill_select_gammas <- map_df(
  krill_mse_metrics, 
  ~ summarise(.,
    gamma1 = max(max(gamma[PD <= 0.1]), 0), 
    gamma1_approx = approx(PD, gamma, 0.1)$y, 
    gamma2 = max(gamma[ESC >= 0.75]),
    gamma2_approx = approx(ESC, gamma, 0.75)$y
  ), .id = "Scenarios"
) |>
  mutate(
    selected_gamma = if_else(gamma1 < gamma2, 1, 2)
  )

  





# ------------------------------------------- #
# ----       7. Tabulate results          ---- 
# ------------------------------------------- #

# combine selected gammas under openMSE and GRYM
gammas_by_framework <- bind_rows(
  krill_select_gammas |>
    select(Scenarios, gamma1, gamma2, selected_gamma) |>
    mutate(Framework = "openMSE", .before = 1),
  
  maschette_selected_gammas |>
    select(Scenarios, gamma_1, gamma_2, selected_gamma) |>
    rename_with(~stringr::str_replace(., "_", ""), .cols = !c(Scenarios, selected_gamma)) |>
    mutate(Framework = "GRYM", .before = 1)
)



# Combine with  more info on each PR scenario
scens <- read_xlsx(
  path = "openmse_grym_approximation/grym_files/recruitment_scenarios/maschetteetal_scenarios/Grym_parameter_combinations.xlsx"
) |>
  # filter scenarios under consideration
  filter(
    Scenarios %in% 
      c("Initial_values", "Scenario_06", "Scenario_12", "Scenario_18",
        "Scenario_24", "Scenario_30")
  ) |>
  select(Scenarios:`Recruitment group`, `Mean proportional recruitment`:`N. surveys`) |>
  rename(Description = `Recruitment group`)


gammas_by_framework <- right_join( scens, gammas_by_framework)


# write out as image
gammas_by_framework |>
  arrange(Scenarios, Framework) |>
  rename(`Selected gamma` = selected_gamma) |>
  flextable::flextable() |>
  flextable::theme_vanilla() |>
  flextable::fontsize(size = 9)|>
  flextable::autofit() |>
  flextable::width(j = ~ Source + Description, width = 4) |>
  flextable::colformat_double(j = ~ . -`Selected gamma` - `N. surveys`, digits = 4) |>
  flextable::merge_v(j = 1:6 , combine = TRUE) |>
  flextable::align(j = c(4:6, 10), align = "center", part = "all") |>
  flextable::fix_border_issues() |>
  flextable::save_as_image(path = "openmse_grym_approximation/outputs/table_grym_vs_openmse_selected_gammas.png")





gammas_by_framework |>
  group_by(Scenarios, `Mean proportional recruitment`, `SD of proportional recruitment`) |>
  summarise(diff_gamma1 = abs(diff(gamma1))*100, diff_gamma2 = abs(diff(gamma2))*100) |>
  arrange( `SD of proportional recruitment`)
  


# ------------------------------------------- #
# ----          7. Plot  results           ---- 
# ------------------------------------------- #

# -- Performance metrics and selected gammas

krill_mse_metrics_df <- krill_mse_metrics |>
  bind_rows(.id = "Scenarios")


theme_set(
  theme_bw() +
    theme(
      panel.grid.minor.y = element_blank(),
      #legend.position="bottom"
      )
)


ylab <- PD(krill_mse_scen$Scenario_06)@Caption
p_PD <- krill_mse_metrics_df |>
  ggplot(aes(x = gamma, y = PD, colour = Scenarios)) +
  geom_line() +
  geom_point(aes(fill = Scenarios), shape = 21, colour = "white", size = 2, alpha = 0.5) +
  #geom_point(aes(colour = Scenarios), fill = "white", shape = 21, size = 3) +
  geom_hline(yintercept = 0.1, linetype = "dashed") +
  labs(y = ylab, title = "Depletion Rule") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(values = MetBrewer::met.brewer("Juarez")) +
  scale_fill_manual(values = MetBrewer::met.brewer("Juarez"))


ylab <- ESC(krill_mse_scen$Scenario_06)@Caption
p_ESC <- krill_mse_metrics_df |>
  ggplot(aes(x = gamma, y = ESC, colour = Scenarios)) +
  geom_line() +
  geom_point(aes(fill = Scenarios), shape = 21, colour = "white", size = 2, alpha = 0.5) +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  labs(y = ylab, title = "75% Escapement Rule") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(values = MetBrewer::met.brewer("Juarez")) +
  scale_fill_manual(values = MetBrewer::met.brewer("Juarez"))


p <- p_PD/p_ESC + plot_layout(guides = 'collect')
p

ggsave(p, filename = "openmse_grym_approximation/outputs/plot_gammas_vs_depletion_escapement_2.png", width = 11, height = 8)









# dt <- expand_grid(
#   year = 1:krill_mse_scen$Initial_values@proyears,
#   gamma = gammas,
#   sim = 1:krill_mse_scen$Initial_values@nsim
# ) |>
#   mutate(`SSB/SSB0` = as.vector(krill_mse_scen$Initial_values@SSB/krill_mse_scen$Initial_values@OM$SSB0))
# 
# 
# dt %>%
#   #filter(sim < 100) |>
#   group_by(gamma, sim) |>
#   mutate(below_20pct = if_else(min(`SSB/SSB0`) < 0.2, TRUE, FALSE)) |>
#   ggplot(aes(x = year, y = `SSB/SSB0`, group = sim, col = below_20pct)) +
#   geom_line(alpha = 0.3) +
#   geom_hline(yintercept = 0.2, linetype = "dashed") +
#   scale_color_manual(values = c("green", "red"), guide = NULL) +
#   facet_wrap(~gamma) +
#   theme_bw()











OM_krill_grym_scn@D <- c(1.5, 2)

hist_krill <- Simulate(OM_krill_grym_scn)

hist_krill@OM@D
range(hist_krill@OMPars$D)
range(hist_krill@SampPars$Stock$Depletion)
range(hist_krill@OMPars$qs)
range(hist_krill@TSdata$Find)
range(hist_krill@TSdata$Landings)
range(hist_krill@AtAge$F.Mortality)
range(hist_krill@Data@CAA)

mse_krill <- runMSE(OM_krill_grym_scn, "AvC")

mse_krill@Hist@Data


hist_krill@SampPars$Fleet$qs
hist_krill@SampPars$Fleet$Find
sum(hist_krill@Data@CAA)




mse_krill@RefPoint$Dynamic_Unfished$SSB0[, mse_krill@nyears]



hist_krill@Ref$ReferencePoints$B0[1]
hist_krill@Data@OM$B0[1]


hist_krill@Ref$Dynamic_Unfished$B0[1, ]


hist_krill@Data@Misc$ReferencePoints$ByYear$B0[1, ]
hist_krill@Ref$ByYear$B0[1, ]
hist_krill@Ref$ReferencePoints$B0[1]



apply(hist_krill@Data@Misc$StockPars$Biomass, c(1, 3), sum)[1, ]
hist_krill@Ref$Dynamic_Unfished$B0[x, length(Data@Year)]

hist_krill@Data@Misc$ReferencePoints$Dynamic_Unfished$B0[1, ]



hist_krill@OM@nyears

hist_krill@Data@Cat


sum(hist_krill@Data@Misc$StockPars$Biomass[1,  , 5, ])





matplot(t(hist_krill@Ref$ByYear$B0[1:10, 1:5]), type = "l", xlab="Year", ylab="SSB0")
matplot(t(hist_krill@Ref$Dynamic_Unfished$B0[1:10, 1:5]), type = "l", xlab="Year", ylab="Dynamic SSB0")



sim <- 1
# Depletion value for this simulation
hist_krill@SampPars$Stock$D
hist_krill@SampPars$Stock$Depletion

hist_krill@OM@D
hist_krill@OMPars$Depletion
hist_krill@OMPars$D


hist_krill@SampPars$Fleet$qs

## [1] 0.06173859

# SSB time-series (sum over areas)
SSB <- rowSums(Hist@TSdata$SBiomass[sim,,])

# SSB0 for this sim
SSB0 <- Hist@Ref$ReferencePoints$SSB0[sim]

plot(1:OM@nyears, SSB/SSB0, ylim=c(0, 1), lwd=2, 
     xlab="Year", ylab="Depletion (SB/SB0)", type="l", las=1,
     bty="n")
abline(h=Hist@SampPars$Stock$Depletion[sim], lty=3)



stockpars <- SampleStockPars(OM_krill_grym_base, 10)
fleetpars <- SampleFleetPars(OM_krill_grym_base, Stock = stockpars, 10)





OM <- new("OM", Albacore, DecE_Dom, Imprecise_Unbiased, Overages)
OM@D <- c(0.95, 1)
OM@nsim <- 10
OM@Perr

test <- Simulate(OM)

test@OM@D
test@OMPars$D
test@SampPars$Stock$Depletion

