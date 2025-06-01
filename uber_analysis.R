library(plyr)
library(ggplot2)
library(lubridate)


months <- c("apr", "may", "jun", "jul", "aug", "sep")
data_list <- lapply(months, function(month) {
  read.csv(paste0("data/uber-raw-data-", month, "14.csv"), stringsAsFactors = FALSE)
})
apr_to_sep_dat <- do.call(rbind, data_list)

tryCatch({
  apr_to_sep_dat$Date.Time <- as.POSIXct(apr_to_sep_dat$Date.Time, format = "%m/%d/%Y %H:%M:%S")
}, error = function(e) {
  cat("Error during Date.Time conversion:", conditionMessage(e), "\n")
  apr_to_sep_dat$Hour <- NA
})

if ("Hour" %in% colnames(apr_to_sep_dat)) {
  apr_to_sep_dat$Hour <- format(apr_to_sep_dat$Date.Time, "%H")
} else {
  cat("Hour column not found. Check the error message above for details.\n")
}


# Feature extraction
print("Data preprocessing ...")

# Function to get rides for each day of the week
apr_to_sep_dat$Date <- as.Date(apr_to_sep_dat$Date.Time)

tryCatch({
  apr_to_sep_dat$hr <- as.numeric(format(apr_to_sep_dat$Date.Time, "%H"))
}, error = function(e) {
  cat("Error during 'hr' conversion:", conditionMessage(e), "\n")
  apr_to_sep_dat$hr <- NA
})

tryCatch({
  apr_to_sep_dat$hr_map_r <- factor(floor(as.numeric(apr_to_sep_dat$hr)/24*8))
}, error = function(e) {
  cat("Error during 'hr_map_r' conversion:", conditionMessage(e), "\n")
  apr_to_sep_dat$hr_map_r <- NA
})

tryCatch({
  apr_to_sep_dat$hr_map <- mapvalues(apr_to_sep_dat$hr_map_r,
                                     from = 0:7,
                                     to = c("0:00-03:00",
                                            "03:00-06:00",
                                            "06:00-09:00",
                                            "09:00-12:00",
                                            "12:00-15:00",
                                            "15:00-18:00",
                                            "18:00-21:00",
                                            "21:00-24:00"))
}, error = function(e) {
  cat("Error during 'hr_map' conversion:", conditionMessage(e), "\n")
  apr_to_sep_dat$hr_map <- NA
})

  
apr_to_sep_dat$wd <- wday(apr_to_sep_dat$Date, label = TRUE)

tt <- table(apr_to_sep_dat$Date)
tt_names <- names(tt)

tryCatch({
  first_indices <- match(as.Date(tt_names), apr_to_sep_dat$Date)
}, error = function(e) {
  cat("Error during 'first_indices' calculation:", conditionMessage(e), "\n")
  first_indices <- NA
})

if (any(is.na(first_indices))) {
  cat("Error: Some indices could not be calculated. Check the error message above for details.\n")
}


i=1
tt <- table(apr_to_sep_dat$Date)
tt_names <- names(tt)

first_indices <- match(as.Date(tt_names), apr_to_sep_dat$Date)

for (i in seq_along(tt_names)) {
  progress <- sprintf("%.2f", i / length(tt_names) * 100)
  cat(paste(progress, "percent of data preprocessing done ...\n"))
}
