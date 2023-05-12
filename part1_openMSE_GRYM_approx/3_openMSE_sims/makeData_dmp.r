makeData <- function (Biomass, CBret, Cret, N, SSB, VBiomass, StockPars, 
          FleetPars, ObsPars, ImpPars, RefPoints, SampCpars, initD, 
          Sample_Area, Name, nyears, proyears, nsim, nareas, reps, 
          CurrentYr, silent = FALSE, control = list()) 
{
  if (!silent) 
    message("Simulating observed data")
  Data <- new("Data")
  if (reps == 1) 
    Data <- OneRep(Data)
  Data <- replic8(Data, nsim)
  Data@Name <- Name
  Data@Year <- (CurrentYr - nyears + 1):CurrentYr
  Data@Cat <- ObsPars$Cobs_y[, 1:nyears] * apply(CBret * Sample_Area$Catch[, 
                                                                           , 1:nyears, ], c(1, 3), sum)
  Data@CV_Cat <- matrix(Data@CV_Cat[, 1], nrow = nsim, ncol = nyears)
  II <- (apply(Biomass * Sample_Area$BInd[, , 1:nyears, ], 
               c(1, 3), sum)^ObsPars$I_beta) * ObsPars$Ierr_y[, 1:nyears]
  II <- II/apply(II, 1, mean)
  Data@Ind <- II
  Data@CV_Ind <- matrix(Data@CV_Ind[, 1], nrow = nsim, ncol = nyears)
  II <- (apply(SSB * Sample_Area$SBInd[, , 1:nyears, ], c(1, 
                                                          3), sum)^ObsPars$SpI_beta) * ObsPars$SpIerr_y[, 1:nyears]
  II <- II/apply(II, 1, mean)
  Data@SpInd <- II
  Data@CV_SpInd <- matrix(Data@CV_SpInd[, 1], nrow = nsim, 
                          ncol = nyears)
  II <- (apply(VBiomass * Sample_Area$VInd[, , 1:nyears, ], 
               c(1, 3), sum)^ObsPars$VI_beta) * ObsPars$VIerr_y[, 1:nyears]
  II <- II/apply(II, 1, mean)
  Data@VInd <- II
  Data@CV_VInd <- matrix(Data@CV_VInd[, 1], nrow = nsim, ncol = nyears)
  Data@Rec <- apply(N[, 1, , ] * Sample_Area$RecInd[, 1:nyears, 
  ], c(1, 2), sum) * ObsPars$Recerr_y[, 1:nyears]
  Data@t <- rep(nyears, nsim)
  Data@AvC <- apply(Data@Cat, 1, mean, na.rm = TRUE)
  Depletion <- apply(SSB[, , nyears, ], 1, sum)/RefPoints$SSB0
  Data@Dt <- Depletion * ObsPars$Derr_y[, nyears]
  Data@Dep <- Depletion * ObsPars$Derr_y[, nyears]
  Data@vbLinf <- StockPars$Linfarray[, nyears] * ObsPars$Linfbias
  Data@vbK <- StockPars$Karray[, nyears] * ObsPars$Kbias
  Data@vbt0 <- StockPars$t0array[, nyears] + ObsPars$t0bias
  Data@Mort <- StockPars$Marray[, nyears] * ObsPars$Mbias
  Data@L50 <- StockPars$L50array[, nyears] * ObsPars$lenMbias
  Data@L95 <- StockPars$L95array[, nyears] * ObsPars$lenMbias
  Data@L95[Data@L95 > 0.95 * Data@vbLinf] <- 0.95 * Data@vbLinf[Data@L95 > 
                                                                  0.95 * Data@vbLinf]
  Data@L50[Data@L50 > 0.95 * Data@L95] <- 0.95 * Data@L95[Data@L50 > 
                                                            0.95 * Data@L95]
  Data@LenCV <- StockPars$LenCV
  Data@sigmaR <- StockPars$procsd * ObsPars$sigmaRbias
  Data@MaxAge <- StockPars$maxage
  hs <- StockPars$hs
  Data@steep <- hs * ObsPars$hbias
  ntest <- 20
  test <- array(RefPoints$SSBMSY_SSB0 * ObsPars$BMSY_B0bias, 
                dim = c(nsim, ntest))
  indy <- array(rep(1:ntest, each = nsim), c(nsim, ntest))
  indy[test > max(0.9, max(RefPoints$SSBMSY_SSB0))] <- NA
  ObsPars$BMSY_B0bias <- ObsPars$BMSY_B0bias[cbind(1:nsim, 
                                                   apply(indy, 1, min, na.rm = T))]
  Data@FMSY_M <- RefPoints$FMSY_M * ObsPars$FMSY_Mbias
  Data@BMSY_B0 <- RefPoints$SSBMSY_SSB0 * ObsPars$BMSY_B0bias
  Data@Cref <- RefPoints$MSY * ObsPars$Crefbias
  Data@Bref <- RefPoints$VBMSY * ObsPars$Brefbias
  I3 <- apply(Biomass, c(1, 3), sum)^ObsPars$I_beta
  I3 <- I3/apply(I3, 1, mean)
  if (!is.null(initD)) {
    b1 <- apply(Biomass, c(1, 3), sum)
    b2 <- matrix(RefPoints$BMSY, nrow = nsim, ncol = nyears)
    ind <- apply(abs(b1/b2 - 1), 1, which.min)
    Iref <- diag(I3[1:nsim, ind])
  }
  else {
    Iref <- apply(I3[, 1:nyears], 1, mean) * RefPoints$BMSY_B0
  }
  Data@Iref <- Iref * ObsPars$Irefbias
  M_array <- array(0.5 * StockPars$M_ageArray[, , nyears], 
                   dim = c(nsim, StockPars$maxage + 1, nareas))
  A <- apply(VBiomass[, , nyears, ] * exp(-M_array), 1, sum)
  Asp <- apply(SSB[, , nyears, ] * exp(-M_array), 1, sum)
  OFLreal <- A * (1 - exp(-RefPoints$FMSY))
  Data@Abun <- A * ObsPars$Aerr_y[, nyears]
  Data@SpAbun <- Asp * ObsPars$Aerr_y[, nyears]
  Cret2 <- apply(Cret * Sample_Area$CAA[, , 1:nyears, ], 1:3, 
                 sum)
  Data@CAA <- MSEtool:::simCAA(nsim, nyears, StockPars$maxage + 1, Cret2, 
                     ObsPars$CAA_ESS, ObsPars$CAA_nsamp)
  CALdone <- FALSE
  if (!is.null(control$CAL)) {
    if (control$CAL == "removals") {
      vn <- apply(N * Sample_Area$CAL[, , 1:nyears, ], 
                  c(1, 2, 3), sum) * FleetPars$V_real[, , 1:nyears]
      vn <- aperm(vn, c(1, 3, 2))
      CALdat <- MSEtool:::simCAL(nsim, nyears, StockPars$maxage, 
                       ObsPars$CAL_ESS, ObsPars$CAL_nsamp, StockPars$nCALbins, 
                       StockPars$CAL_binsmid, StockPars$CAL_bins, vn, 
                       FleetPars$SLarray_real, StockPars$Linfarray, 
                       StockPars$Karray, StockPars$t0array, StockPars$LenCV)
      CALdone <- TRUE
    }
    else {
      warning("Invalid entry in OM@cpars$control$CAL (use OM@cpars$control$CAL=\"removals\")\nSimulating CAL from retained catch")
    }
  }
  if (!CALdone) {
    vn <- apply(N * Sample_Area$CAL[, , 1:nyears, ], c(1, 
                                                       2, 3), sum) * FleetPars$retA_real[, , 1:nyears]
    vn <- aperm(vn, c(1, 3, 2))
    CALdat <- MSEtool:::simCAL(nsim, nyears, StockPars$maxage, ObsPars$CAL_ESS, 
                     ObsPars$CAL_nsamp, StockPars$nCALbins, StockPars$CAL_binsmid, 
                     StockPars$CAL_bins, vn, FleetPars$retL_real, StockPars$Linfarray, 
                     StockPars$Karray, StockPars$t0array, StockPars$LenCV)
  }
  Data@CAL_bins <- StockPars$CAL_bins
  Data@CAL_mids <- StockPars$CAL_binsmid
  Data@CAL <- CALdat$CAL
  Data@ML <- CALdat$ML
  Data@Lc <- CALdat$Lc
  Data@Lbar <- CALdat$Lbar
  Data@LFC <- FleetPars$L5_y[, nyears] * ObsPars$LFCbias
  Data@LFS <- FleetPars$LFS_y[, nyears] * ObsPars$LFSbias
  Data@Vmaxlen <- FleetPars$Vmaxlen_y[, nyears]
  Data@MPrec <- apply(CBret, c(1, 3), sum)[, nyears]
  Data@MPeff <- rep(1, nsim)
  ind <- which(lapply(StockPars, length) == nsim)
  drop_srr <- which(names(ind) == "SRRpars")
  ind <- ind[-drop_srr]
  stock <- as.data.frame(StockPars[ind])
  stock$Fdisc <- NULL
  stock$CAL_bins <- NULL
  stock$CAL_binsmid <- NULL
  ind <- which(lapply(FleetPars, length) == nsim)
  fleet <- as.data.frame(FleetPars[ind])
  ind <- which(lapply(ImpPars, length) == nsim)
  imp <- as.data.frame(ImpPars[ind])
  refs <- RefPoints[!names(RefPoints) %in% names(stock)]
  OMtable <- data.frame(stock, fleet, imp, refs, ageM = StockPars$ageMarray[, 
                                                                            nyears], L5 = FleetPars$L5_y[, nyears], LFS = FleetPars$LFS_y[, 
                                                                                                                                          nyears], Vmaxlen = FleetPars$Vmaxlen_y[, nyears], LR5 = FleetPars$LR5_y[, 
                                                                                                                                                                                                                  nyears], LFR = FleetPars$LFR_y[, nyears], Rmaxlen = FleetPars$Rmaxlen_y[, 
                                                                                                                                                                                                                                                                                          nyears], DR = FleetPars$DR_y[, nyears], OFLreal, maxF = StockPars$maxF, 
                        A = A, Asp = Asp, CurrentYr = CurrentYr)
  OMtable <- OMtable[, order(names(OMtable))]
  Data@OM <- OMtable
  ObsParsDF <- ObsPars
  ind <- which(lapply(ObsParsDF, length) == nsim) %>% as.numeric()
  ObsParsDF <- ObsParsDF[ind]
  ObsTable <- as.data.frame(ObsParsDF)
  ObsTable <- ObsTable[, order(names(ObsTable))]
  Data@Obs <- ObsTable
  Data@Units <- "unitless"
  Data@Ref_type <- "Simulated OFL"
  Data@wla <- rep(StockPars$a, nsim)
  Data@wlb <- rep(StockPars$b, nsim)
  Data@nareas <- nareas
  Data@Ref <- OFLreal
  Data@LHYear <- CurrentYr
  Data@Misc <- vector("list", nsim)
  Data
}
