#' Hack to the function `MSEtool::OMdoc` by adding the argument `html_theme` to
#' set-up the html theme of the rendered output. The main motivation for the
#' change was to avoid style clashes when the rendered html is embedded in other
#' documents
OMdoc_dmp <- function (OM = NULL, rmd.source = NULL, overwrite = FALSE, out.file = NULL, 
          inc.plot = TRUE, render = TRUE, output = "html_document", 
          openFile = TRUE, quiet = FALSE, dir = NULL, bib_file = NULL, html_theme = "flatly", ...) 
{
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package \"rmarkdown\" needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  toc = TRUE
  color = "blue"
    theme = html_theme
    if (is.null(dir)) 
      dir <- getwd()
    OMXLname <- NULL
    if (methods::is(OM, "OM")) {
    }
    else if (methods::is(OM, "character")) {
      OMXLname <- OM
      OM <- XL2OM(file.path(dir, OM), msg = FALSE)
    }
    else if (is.null(OM)) {
      fls <- list.files(path = dir, pattern = ".xlsx", ignore.case = TRUE)
      fls <- fls[!grepl("~", fls)]
      if (length(fls) == 1) 
        OM <- XL2OM(file.path(dir, fls), msg = FALSE)
      if (length(fls) > 1) 
        stop("argument \"OM\" not provided and multiple .xlsx files in ", 
             dir, call. = FALSE)
    }
    else stop("OM must be class \"OM\" or name of OM xlsx file.", 
              call. = FALSE)
    if (is.null(OM)) 
      stop("OM not imported. Is the name correct?", call. = FALSE)
    if (is.null(rmd.source)) {
      rmd.source <- list.files(path = dir, pattern = ".rmd", 
                               ignore.case = TRUE)
      if (length(rmd.source) == 0) 
        stop("rmd.source' not specified and no .rmd files found in ", 
             dir, call. = FALSE)
      if (length(rmd.source) == 1) {
        textIn <- readLines(file.path(dir, rmd.source))
      }
      else {
        NoExt <- tools::file_path_sans_ext(rmd.source)
        if (!is.null(OMXLname)) 
          ind <- which(tolower(NoExt) == tolower(paste0(OMXLname, 
                                                        "_source")))
        if (is.null(OMXLname)) 
          ind <- which(tolower(NoExt) == tolower(paste0(OM@Name, 
                                                        "_source")))
        if (length(ind) > 0) {
          rmd.source <- rmd.source[ind]
          message("Reading ", file.path(dir, rmd.source))
          textIn <- readLines(file.path(dir, rmd.source))
        }
        else {
          stop("'rmd.source' not specified and multiple .rmd files found in ", 
               dir, call. = FALSE)
        }
      }
    }
    else {
      if (nchar(tools::file_ext(rmd.source)) == 0) {
        rmd.source <- paste0(rmd.source, ".rmd")
      }
      else if (tools::file_ext(rmd.source) != "rmd") 
        stop("rmd.source extension must be rmd", call. = FALSE)
      if (!file.exists(file.path(dir, rmd.source))) 
        stop(rmd.source, " not found in ", dir, call. = FALSE)
      MSEtool:::message_info("Reading ", file.path(dir, rmd.source))
      textIn <- readLines(file.path(dir, rmd.source))
    }
    if (!dir.exists(file.path(dir, "build"))) {
      dir.create(file.path(dir, "build"))
      tt <- file.create(file.path(dir, "build/readme.txt"))
      cat("This directory was created by MSEtool function OMdoc\n\n", 
          sep = "", append = TRUE, file = file.path(dir, "build/readme.txt"))
      cat("Files in this directory are used to generate the OM report.\n\n", 
          sep = "", append = TRUE, file = file.path(dir, "build/readme.txt"))
    }
    if (dir.exists(file.path(dir, "images"))) {
      dir.create(file.path(dir, "build/images"), showWarnings = FALSE)
      cpy <- file.copy(file.path(dir, "images"), file.path(dir, 
                                                           "build"), overwrite = TRUE, recursive = TRUE)
    }
    if (is.null(out.file)) 
      out.file <- tools::file_path_sans_ext(rmd.source)
    RMDfile <- file.path(dir, paste0("build/", out.file, ".Rmd"))
    tt <- file.create(RMDfile)
    ind <- grep("^# Title", textIn)
    if (length(ind) > 0) {
      title <- trimws(textIn[ind + 1])
      if (nchar(title) == 0) 
        title <- paste("Operating Model:", OM@Name)
    }
    else {
      title <- paste("Operating Model:", OM@Name)
    }
    ind <- grep("^# Subtitle", textIn)
    if (length(ind) > 0) {
      subtitle <- trimws(textIn[ind + 1])
      if (nchar(subtitle) == 0) 
        subtitle <- NULL
    }
    else {
      subtitle <- NULL
    }
    ind <- grep("# Author", textIn)
    if (length(ind) > 0) {
      temp <- min(which(nchar(textIn[(ind + 1):length(textIn)]) == 
                          0))
      if (temp > 1) {
        authors <- trimws(textIn[(ind + 1):(ind + temp - 
                                              1)])
      }
      else {
        authors <- "No author provided"
      }
    }
    else {
      authors <- "No author provided"
    }
    ind <- grep("# Date", textIn)
    if (length(ind) > 0) {
      date <- trimws(textIn[(ind + 1)])
      if (grepl("Optional. Date", date)) 
        date <- NULL
    }
    else {
      date <- NULL
    }
    cat("---\n", sep = "", append = TRUE, file = RMDfile)
    cat("title: '", title, "'\n", append = TRUE, file = RMDfile, 
        sep = "")
    if (!is.null(subtitle)) 
      cat("subtitle: '", subtitle, "'\n", append = TRUE, file = RMDfile, 
          sep = "")
    if (length(authors) > 1) {
      cat("author:", "\n", append = TRUE, file = RMDfile, sep = "")
      for (xx in 1:length(authors)) {
        cat("- ", authors[xx], "\n", append = TRUE, file = RMDfile, 
            sep = "")
      }
    }
    else {
      cat("author: ", authors, "\n", append = TRUE, file = RMDfile, 
          sep = "")
    }
    if (is.null(date)) 
      date <- format(Sys.time(), "%d %B %Y")
    cat("date: ", date, "\n", append = TRUE, file = RMDfile, 
        sep = "")
    if (toc) {
      cat("output: ", "\n", append = TRUE, file = RMDfile, 
          sep = "")
      cat("   ", output, ":", "\n", append = TRUE, file = RMDfile, 
          sep = "")
      cat("     toc: true\n", append = TRUE, file = RMDfile, 
          sep = "")
      cat("     toc_depth: 3\n", append = TRUE, file = RMDfile, 
          sep = "")
      if (output == "html_document") {
        cat("     toc_float: true\n", append = TRUE, file = RMDfile, 
            sep = "")
        cat("     theme: ", theme, "\n", append = TRUE, file = RMDfile, 
            sep = "")
      }
    }
    else {
      cat("output: ", output, "\n", append = TRUE, file = RMDfile, 
          sep = "")
    }
    if(!is.null(bib_file)){
      cat("bibliography: ../", bib_file, "\n", append = TRUE, file = RMDfile, 
          sep = "")
    }
    
    cat("---\n\n", sep = "", append = TRUE, file = RMDfile)
    
    
    cat("\n
```{css toc-content, echo = FALSE}
    
  #TOC {
  right: 50px;
  margin: 20px 0px 25px 0px;
  }
  
 .col-md-3 {
	width: 0%;
	}

  .main-container {
  margin-left: 5px;
  }
``` \n", append = TRUE, file = RMDfile)
    
    Pars <- NULL
    out <- NULL
    if (inc.plot) {
      runSims <- FALSE
      fileName <- OM@Name
      fileName <- gsub(" ", "", gsub("[[:punct:]]", "", fileName))
      if (nchar(fileName) > 15) 
        fileName <- substr(fileName, 1, 15)
      if (file.exists(paste0(file.path(dir, "build/", paste0(fileName, 
                                                             ".dat"))))) {
        chkFile <- file.exists(paste0(file.path(dir, "build/", 
                                                paste0(fileName, "Hist.dat"))))
        if (chkFile) {
          testOM <- readRDS(file.path(dir, paste0("build/", 
                                                  fileName, ".dat")))
          if (methods::is(testOM, "OM")) {
            changed <- rep(FALSE, length(slotNames(OM)))
            for (sl in seq_along(slotNames(OM))) {
              oldOM <- slot(OM, slotNames(OM)[sl])
              newOM <- slot(testOM, slotNames(OM)[sl])
              oldOM <- oldOM[!is.na(oldOM)]
              newOM <- newOM[!is.na(newOM)]
              if (!methods::is(oldOM, "character")) {
                if (!methods::is(oldOM, "list")) {
                  if (length(oldOM) < 1 || !is.finite(oldOM)) 
                    oldOM <- 0
                  if (length(newOM) < 1 || !is.finite(newOM)) 
                    newOM <- 0
                  if (any(oldOM != newOM)) 
                    changed[sl] <- TRUE
                }
                else {
                  if (length(oldOM) != length(newOM)) {
                    changed[sl] <- TRUE
                  }
                  else if (length(oldOM) > 0) {
                    for (xx in 1:length(oldOM)) {
                      if (!inherits(oldOM[[xx]], "Data")) 
                        if (any(oldOM[[xx]] != newOM[[xx]])) 
                          changed[sl] <- TRUE
                    }
                  }
                }
              }
            }
            if (sum(changed) > 0) 
              runSims <- TRUE
            if (sum(changed) == 0) {
              out <- readRDS(file.path(dir, paste0("build/", 
                                                   fileName, "Hist.dat")))
              Pars <- c(out@AtAge, out@TSdata, out@Ref, 
                        out@SampPars)
            }
          }
          else {
            file.remove(file.path(dir, paste0("build/", 
                                              fileName, ".dat")))
            runSims <- TRUE
          }
        }
        else {
          file.remove(file.path(dir, paste0("build/", fileName, 
                                            ".dat")))
          runSims <- TRUE
        }
      }
      else {
        saveRDS(OM, file = file.path(dir, paste0("build/", 
                                                 fileName, ".dat")))
        runSims <- TRUE
      }
      if (runSims) {
        message("\nRunning Historical Simulations")
        OM2 <- updateMSE(OM)
        if (OM2@nsim > 48) {
          message("nsim too high for documentation purposes. Running here with nsim=48")
          OM2@nsim <- 48
          OM2 <- SubCpars(OM2, 1:48)
        }
        out <- Simulate(OM2, parallel = FALSE, silent = TRUE, 
                        ...)
        Pars <- c(out@AtAge, out@TSdata, out@Ref, out@SampPars)
        saveRDS(out, file = file.path(dir, paste0("build/", 
                                                  fileName, "Hist.dat")))
      }
    }
    ind <- grep("# Introduction", textIn)
    if (length(ind) > 1) 
      stop("# Introduction heading found more than once", call. = FALSE)
    if (length(ind) > 0) {
      textIn <- textIn[ind:length(textIn)]
    }
    else {
      ind <- grep("# Stock Parameters", textIn)
      if (length(ind) > 1) 
        stop("# Stock Parameters heading found more than once", 
             call. = FALSE)
      if (length(ind) == 0) 
        stop("# Stock Parameters not found", call. = FALSE)
      textIn <- textIn[ind:length(textIn)]
    }
    MSEtool:::writeSection(class = "Intro", OM, Pars, textIn, RMDfile, 
                 color = color, inc.plot = inc.plot)
    OMdesc <- MSEtool::OMDescription
    cat("# Operating Model \n", append = TRUE, file = RMDfile, 
        sep = "")
    chkFile <- file.exists("OM.rdata")
    if (chkFile) {
      cat("The OM rdata file can be downloaded from [here](OM.rdata)\n\n", 
          append = TRUE, file = RMDfile, sep = "")
      cat("Download and import into R using `myOM <- readRDS('OM.rdata')` \n\n", 
          append = TRUE, file = RMDfile, sep = "")
    }
    if (.hasSlot(OM, "Species") && !is.na(OM@Species)) {
      cat("## Species Information \n\n", append = TRUE, file = RMDfile, 
          sep = "")
      cat("**Species**: *", OM@Species, "*\n\n", append = TRUE, 
          file = RMDfile, sep = "")
      cat("**Common Name**: *", OM@Common_Name, "*\n\n", append = TRUE, 
          file = RMDfile, sep = "")
      cat("**Management Agency**: ", OM@Agency, "\n\n", append = TRUE, 
          file = RMDfile, sep = "")
      cat("**Region**: ", OM@Region, "\n\n", append = TRUE, 
          file = RMDfile, sep = "")
      if (length(OM@Sponsor) > 0) 
        cat("**Sponsor**: ", OM@Sponsor, "\n\n", append = TRUE, 
            file = RMDfile, sep = "")
      if (length(OM@Latitude) > 0) {
        lat <- paste0(OM@Latitude, sep = "", collapse = ", ")
        cat("**Latitude**: ", lat, "\n\n", append = TRUE, 
            file = RMDfile, sep = "")
      }
      if (length(OM@Longitude) > 0) {
        long <- paste0(OM@Longitude, sep = "", collapse = ", ")
        cat("**Longitude**: ", long, "\n\n", append = TRUE, 
            file = RMDfile, sep = "")
      }
    }
    cat("## OM Parameters \n", append = TRUE, file = RMDfile, 
        sep = "")
    cat("**OM Name**: ", OMdesc$Description[OMdesc$Slot == "Name"], 
        ": ", "<span style='color:", color, "'>", " ", OM@Name, 
        "</span>", "\n\n", append = TRUE, file = RMDfile, sep = "")
    cat("**nsim**: ", OMdesc$Description[OMdesc$Slot == "nsim"], 
        ": ", "<span style='color:", color, "'>", " ", OM@nsim, 
        "</span>", "\n\n", "\n\n", append = TRUE, file = RMDfile, 
        sep = "")
    cat("**proyears**: ", OMdesc$Description[OMdesc$Slot == "proyears"], 
        ": ", "<span style='color:", color, "'>", " ", OM@proyears, 
        "</span>", "\n\n", "\n\n", append = TRUE, file = RMDfile, 
        sep = "")
    cat("**interval**: ", OMdesc$Description[OMdesc$Slot == "interval"], 
        " ", "<span style='color:", color, "'>", " ", OM@interval, 
        "</span>", "\n\n", append = TRUE, file = RMDfile, sep = "")
    cat("**pstar**: ", OMdesc$Description[OMdesc$Slot == "pstar"], 
        ": ", "<span style='color:", color, "'>", " ", OM@pstar, 
        "</span>", "\n\n", append = TRUE, file = RMDfile, sep = "")
    cat("**maxF**: ", OMdesc$Description[OMdesc$Slot == "maxF"], 
        ": ", "<span style='color:", color, "'>", " ", OM@maxF, 
        "</span>", "\n\n", append = TRUE, file = RMDfile, sep = "")
    cat("**reps**: ", OMdesc$Description[OMdesc$Slot == "reps"], 
        " ", "<span style='color:", color, "'>", " ", OM@reps, 
        "</span>", "\n\n", append = TRUE, file = RMDfile, sep = "")
    cat("**Source**: ", OMdesc$Description[OMdesc$Slot == "Source"], 
        "\n\n", "<span style='color:", color, "'>", " ", OM@Source, 
        "</span>", "\n\n", append = TRUE, file = RMDfile, sep = "")
    useCpars <- length(OM@cpars) > 0
    if (useCpars) {
      message("writing cpars section")
      MSEtool:::writeSection(class = "cpars", OM, Pars, textIn, RMDfile, 
                   color = color, inc.plot = inc.plot)
    }
    message("writing Stock section")
    MSEtool:::writeSection(class = "Stock", OM, Pars, textIn, RMDfile, 
                 color = color, inc.plot = inc.plot)
    message("writing Fleet section")
    MSEtool:::writeSection(class = "Fleet", OM, Pars, textIn, RMDfile, 
                 color = color, inc.plot = inc.plot)
    message("writing Obs section")
    MSEtool:::writeSection(class = "Obs", OM, Pars, textIn, RMDfile, color = color, 
                 inc.plot = inc.plot)
    message("writing Imp section")
    MSEtool:::writeSection(class = "Imp", OM, Pars, textIn, RMDfile, color = color, 
                 inc.plot = inc.plot)
    if (inc.plot) {
      cat("\n# Historical Simulation Plots\n", append = TRUE, 
          file = RMDfile, sep = "")
      cat("```{r, echo=FALSE,include=FALSE}\n", append = TRUE, 
          file = RMDfile, sep = "")
      cat(paste0("input <- file.path(system.file(package = 'MSEtool'),'Rmd/Hist/Hist.Rmd')\n"), 
          append = TRUE, file = RMDfile, sep = "")
      cat(" out <- knitr::knit_child(input) \n", append = TRUE, 
          file = RMDfile, sep = "")
      cat("```\n\n", append = TRUE, file = RMDfile, sep = "")
      cat("```{r, echo=FALSE, results='asis'}\n", append = TRUE, 
          file = RMDfile, sep = "")
      cat("cat(out)\n", append = TRUE, file = RMDfile, sep = "")
      cat("```\n\n", append = TRUE, file = RMDfile, sep = "")
    }
    message("writing Reference section")
    MSEtool:::writeSection(class = "References", OM, Pars, textIn, RMDfile, 
                 color = color, inc.plot = inc.plot)
    if (render) {
      RMDfileout <- gsub("_compiled", "", tools::file_path_sans_ext(RMDfile))
      if (output == "html_document") 
        RMDfileout <- paste0(basename(RMDfileout), ".html")
      if (output == "pdf_document") 
        RMDfileout <- paste0(basename(RMDfileout), ".pdf")
      message("\n\nRendering markdown document as ", RMDfileout)
      if (all(OM@EffYears < 2000)) {
        fstYr <- (OM@CurrentYr - OM@nyears + 1)
        lstYr <- OM@CurrentYr
        EffYears <- (fstYr:lstYr)[OM@EffYears]
        EffYears <- round(EffYears, 0)
      }
      else {
        EffYears <- OM@EffYears
      }
      if (length(OM@cpars$Find) > 0) {
        fstYr <- (OM@CurrentYr - OM@nyears + 1)
        lstYr <- OM@CurrentYr
        EffYears <- fstYr:lstYr
        lower <- as.numeric(signif(apply(OM@cpars$Find, 2, 
                                         min), 3))
        upper <- as.numeric(signif(apply(OM@cpars$Find, 2, 
                                         max), 3))
        Effvals <- data.frame(EffYears = EffYears, EffLower = lower, 
                              EffUpper = upper)
      }
      else {
        Effvals <- data.frame(EffYears = EffYears, EffLower = signif(OM@EffLower, 
                                                                     3), EffUpper = signif(OM@EffUpper, 3))
      }
      Pars$CurrentYr <- OM@CurrentYr
      Pars$MPA <- OM@MPA
      Pars$Hist <- out
      params <- list(OM = OM, Pars = Pars, Effvals = Effvals)
      params$tabs <- TRUE
      params$nyears <- OM@nyears
      params$proyears <- OM@proyears
      nsim <- dim(out@SampPars$Stock$Marray)[1]
      params$its <- sample(1:nsim, 3)
      params$plotPars <- list(breaks = 10, col = "darkgray", 
                              axes = FALSE, cex.main = 1, lwd = 2)
      knitr::knit_meta(class = NULL, clean = TRUE)
      rmarkdown::render(input = RMDfile, output_file = RMDfileout, 
                        output_format = output, output_dir = dir, param = params, 
                        quiet = quiet)
      if (openFile) 
        utils::browseURL(RMDfileout)
    }
    else {
    }
}
