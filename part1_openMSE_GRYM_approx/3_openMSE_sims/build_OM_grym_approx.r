#' ------------------------------------------------------------------------------
#'  Build OM object that approximates Krill's Grym base case implementation
#' ------------------------------------------------------------------------------
#' 
#' This function allows to create Operating Models configured to approximate the
#' base case of krill fishery assessment implemented under the Grym framework
#' (https://github.com/ccamlr/Grym_Base_Case/tree/Simulations)
#'
#' Within the Grym-approximated configuration, the function allows to specifying
#' alternative input values for stock parameters such as M, maturity,
#' selectivity, etc. In this way, it is a convenience function to generate
#' multiple OMs for alternative input scenarios.
#'
#' The two OM components that are configurable are the 'Stock' and 'Fleet'
#' objects
#'
#' --------------------------------------------------------------------------------


build_OM_grym_approx <- function(om_name, maxage, 
                                 Linf, K, t0, mat50Min, mat50Max, matrange, 
                                 sel50Min, sel50Max, selrange, a, b, 
                                 M_draws, RCV_draws,
                                 n_iter, proj_yrs, maxF, seed = 1001){
  
  require(openMSE)
  
  # ---------------------------------- #
  #           Stock Object
  # ---------------------------------- #
  # class?Stock
  
  krill_stock <- new("Stock")
  
  krill_stock@Name <- "krill_stock"
  krill_stock@Common_Name <- "Antarctic Krill"
  krill_stock@Species <- "Euphausia superba"
  # krill assumed to not live past 7 years-old
  krill_stock@maxage <- maxage
  # arbitrary value number of individuals of age-0 recruitment to the population
  krill_stock@R0 <- 1
  # M undefined as it will be provided as random draws via `cpars`
  krill_stock@M <- numeric(0) 
  # M constant over time
  krill_stock@Msd <- 0
  # approximation to Grym approach, where simulated recruitment is reduced when
  # population falls below depletion levels (i.e. SSB = 0.2 x SSB0)
  krill_stock@h <- c(0.90, 0.95)
  # Beverton Holt relationship
  krill_stock@SRrel <- 1
  # M undefined as it will be provided as random draws via the `cpars` slot
  krill_stock@Perr <- numeric(0)
  # no autocorrelation between consecutive recruitments
  krill_stock@AC <- c(0, 0)
  # l@a - Linf assumed as known, as in Grym, from Constable and de la Mare (1996)
  krill_stock@Linf <- c(Linf,	Linf)
  # l@a - no inter-annual variation in Linf
  krill_stock@Linfsd <- c(0, 0)
  # l@a - k assumed as known, as in Grym, from Thanassekos et al (2021)
  krill_stock@K <- c(K,	K)
  # l@a - no inter-annual variation in k
  krill_stock@Ksd <- c(0, 0)
  # l@a - t0 assumed as known, as in Grym, from Thanassekos et al (2021)
  krill_stock@t0 <- c(t0, t0)
  # l@a - low variation in length-at-age, assumed as known
  krill_stock@LenCV <-	c(0.01,	0.01)
  # m@l - mid-point of maturity-at-size, from Maschette et al. (2021)
  krill_stock@L50 <- c(mat50Min,	mat50Max)
  # m@l - Difference in lengths between 50% and 95% maturity - based on width of ogive
  # ramp, as provided by Maschette et al (2021)
  L50 <- mat50Min # arbitrary as ogive width is treated as independent of L50 in Maschette et al (2021)
  L95 <- qunif(0.95, L50 - matrange/2, L50 + matrange/2)
  krill_stock@L50_95 <- c(L95 - L50 ,	L95 - L50 )
  # m@l - current stock depletion
  krill_stock@D <- c(1, 1)
  # w@l - a assumed as known, as in Grym, from CCAMLR (2000)
  krill_stock@a <- a
  # w@l - a assumed as known, as in Grym, from CCAMLR (2000)
  krill_stock@b <- b
  # areas of equal size
  krill_stock@Size_area_1 <- c(0.5, 0.5)
  # equal fraction of unfished biomass in two areas
  krill_stock@Frac_area_1 <- c(0.5, 0.5)
  # 50% chance of individuals remaining in area
  krill_stock@Prob_staying <- c(0.5, 0.5)
  # Zero discard mortality rate as discards are assumed non-existent
  krill_stock@Fdisc <- c(0, 0) # c(1, 1)
  # undefined since there are multiple sources
  krill_stock@Source <- character(0)
  
  
  # ---------------------------------- #
  #           Fleet Object
  # ---------------------------------- #
  # class?Fleet
  
  krill_fleet <- new("Fleet")
  
  krill_fleet@Name <- "krill_fleet"
  # Minimum allowed number of years
  krill_fleet@nyears <- 2
  # final year of historic period
  krill_fleet@CurrentYr <- 2022
  # Arbitrary value for grym approximation - at least 1 value required
  krill_fleet@EffYears <- 1
  # stock remains unfished over historical period
  krill_fleet@EffLower <- 0
  krill_fleet@EffUpper <- 0
  krill_fleet@Esd <- c(0, 0)
  # No change in mean gear efficiency over projection years, as in Grym
  krill_fleet@qinc <- c(0, 0)
  # no inter-annual variability in gear efficiency over projection years, as in Grym
  krill_fleet@qcv <- c(0, 0)
  # Shortest lt at which 5% of fish of that length-class are selected by fishery,
  # based on data from Thanassekos et al (2021)
  L50 <- c(sel50Min, sel50Max)
  L5 <- qunif(0.05, L50 - selrange/2, L50 + selrange/2)
  krill_fleet@L5 <- L5
  # Shortest length at which 100% of fish of that length are selected by the fishery
  krill_fleet@LFS <- L50 + selrange/2
  # All krill selected at Linf
  krill_fleet@Vmaxlen <- c(1, 1)
  # selectectivity in absolute size
  krill_fleet@isRel <- FALSE
  # no discards - i.e. all individuals selected by the gear are retained
  krill_fleet@LR5 <- c(0, 0)  # L5
  krill_fleet@LFR <- c(0, 0)  #LFS
  # All krill retained at Linf
  krill_fleet@Rmaxlen <- c(1, 1)
  # No discards
  krill_fleet@DR <- c(0, 0)
  # fishing effort allocated across areas in proportion to the population density of that area
  krill_fleet@Spat_targ <- c(1, 1)
  # No MPA
  krill_fleet@MPA <- FALSE
  
  
  # ---------------------------------- #
  #           Obs Object
  # ---------------------------------- #
  # class?Obs
  
  # The Obs component is irrelevant for the approximation with GRYM, as specified
  # MPs (one for each level of harvest rate 'gamma') don't require
  # observed/sampled data from the fishery. So assigning 0s to all slots
  
  krill_obs <- new("Obs")
  krill_obs@Name <- "No bias and no observational errors"
  krill_obs@Cobs <- c(0, 0)
  krill_obs@Cbiascv <- 0
  krill_obs@CAA_nsamp <- c(0, 0)
  krill_obs@CAA_ESS <- c(0, 0)
  krill_obs@CAL_nsamp <- c(0, 0)
  krill_obs@CAL_ESS <- c(0, 0)
  krill_obs@Iobs <- c(0, 0)
  krill_obs@Btobs <- c(0, 0)
  krill_obs@Btbiascv <- 0	
  krill_obs@beta <- c(0, 0)
  krill_obs@LenMbiascv <- 0
  krill_obs@Mbiascv	<- 0	
  krill_obs@Kbiascv	<- 0	
  krill_obs@t0biascv <- 0	
  krill_obs@Linfbiascv <- 0	
  krill_obs@LFCbiascv	<- 0	
  krill_obs@LFSbiascv	<- 0	
  krill_obs@FMSY_Mbiascv <- 0	
  krill_obs@BMSY_B0biascv	<- 0	
  krill_obs@Irefbiascv <- 0	
  krill_obs@Brefbiascv <- 0	
  krill_obs@Crefbiascv <- 0	
  krill_obs@Dbiascv	<- 0	
  krill_obs@Dobs <- c(0, 0)
  krill_obs@hbiascv	<- 0	
  krill_obs@Recbiascv	<- c(0, 0)
  krill_obs@sigmaRbiascv <- 0	
  krill_obs@Eobs <- c(0, 0)
  krill_obs@Ebiascv	<- 0	
  
  
  
  # ---------------------------------- #
  #           Imp Object
  # ---------------------------------- #
  # class?Imp
  
  # To mimic Grym approach, assuming the imposed TACs are enforced perfectly -
  # catches never exceed nor fall short of the impose quota
  
  krill_imp <- MSEtool::Perfect_Imp
  
  
  # ---------------------------------- #
  #        Create Operating Model
  # ---------------------------------- #
  
  krill_OM <- new("OM", krill_stock, krill_fleet, krill_obs, krill_imp)
  
  
  # ---------------------------------- #
  #     Additional OM information 
  # ---------------------------------- #
  # class?OM
  
  krill_OM@Name <- om_name
  krill_OM@Agency <- "CCAMLR"
  krill_OM@Region <- "FAO Area 48"
  krill_OM@Sponsor <- "Norwegian Polar Institute"
  krill_OM@Latitude <- -60
  krill_OM@Longitude <- -40
  krill_OM@nsim <- n_iter
  krill_OM@proyears <- proj_yrs
  krill_OM@interval <- 1
  # MPs based on the median of distribution of recommendations
  krill_OM@pstar <- 0.5
  krill_OM@maxF <- maxF
  # MPs are the median value of distribution of recommendations 
  krill_OM@reps <- 1
  krill_OM@seed <- seed
  
  
  
  # ---------------------------------- #
  #         Custom Parameters 
  # ---------------------------------- #
  
  # Switch off age-plus group
  krill_OM@cpars$plusgroup <- 0
  # Pre-generated samples of M
  M_draws[which(M_draws == 0)] <- 1e-5 # openMSE crashes if M == 0, so a slightly naughty work-around
  krill_OM@cpars$M <- M_draws
  # Pre-generated samples of Perr
  krill_OM@cpars$Perr <- sqrt(log(RCV_draws^2 + 1)) # converting CV from PR to lognormal SD
  # switch off depletion optimization step - thus, depletion calculated from the selectivity and pattern in F
  krill_OM@cpars$qs <- rep(1, n_iter)
  
  return(krill_OM)
}


