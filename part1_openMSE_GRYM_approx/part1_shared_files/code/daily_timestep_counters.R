days_since_ref <- function(ddmm, ref_ddmm, base_year = "2021"){
  
  date <- dmy(paste(ddmm, base_year, sep = "/"))  
  ref_date <- dmy(paste(ref_ddmm, base_year, sep = "/"))
  
  if(month(date) < month(ref_date)){
    date <- date + years(1)
  }
  
  if(date < ref_date) {
    stop("Dates on same month, so day in `ddmm` must be greater than day in `ref_ddmm`")
  }
  
  #as.numeric(date - ref_date + 1)
  as.double(difftime(date, ref_date, units = "days") + 1)
}

# days_since_ref("12/Feb", "1/Oct")
# days_since_ref("10/8", "30/8")
# days_since_ref("10/5", "30/3")


get_daily_steps <- function(start_ddmm, end_ddmm, ref_ddmm, base_year = 2021){
  
  start_date <- dmy(paste(start_ddmm, base_year, sep = "/"))
  end_date <- dmy(paste(end_ddmm, base_year, sep = "/"))  
  ref_date <- dmy(paste(ref_ddmm, base_year, sep = "/"))
  
  if(month(start_date) < month(ref_date)){
    start_date <- start_date + years(1)
    end_date <- end_date + years(1)
  }
  
  if(month(start_date) ==  month(end_date)){
    if(day(end_date) < day(start_date)) {
      stop("Start and end dates on same month, so day element in `end_ddmm` must be greater than day element `start_ddmm`")
    }
  }else if(month(end_date) < month(start_date)){
    end_date <- end_date + years(1)
  }
  
  step_start <- start_date - ref_date + 1
  step_end <- end_date - ref_date + 1
  
  steps <- seq(step_start, step_end) %% 365
  steps[steps == 0] <- 365
  steps
}

# get_daily_steps("20/09", "28/09", "25/09")
# get_daily_steps("29/09", "28/09", "25/08")






