
covariate_pipeline <- list(
	cohort = function(cohort, verified_cohort) {
		cohort <- verified_cohort 
		return(cohort)
	},
	age_group = function(detection_date, verified_cohort, birthday, breaks) {
		if (is.na(verified_cohort)) {
			return(as.numeric(NA))
		} else {
			age_group <- sapply(
				X=detection_date, FUN=age, birthday=birthday, breaks=breaks)
			return(age_group)
		}
		stop("Something is rotten in Denmark.")
	},
	start_date = function(detection_date) {
		l <- length(detection_date)
		date <- c(ymd_hms(NA),detection_date[1:(l-1)])
		return(date)
	},
	stop_date = function(detection_date) {
		l <- length(detection_date)
		date <- detection_date[1:l]
		return(date)
	},
	zst = function(start_date, stop_date, covariate_table) {
		measurement_dates <- covariate_table[['date_ct']]
		measurements <- covariate_table[['zst']]
		
		## Is covariate defined:
		if (is.na(start_date) || is.na(stop_date)) return(as.numeric(NA))
	
		## Subset:
		included <- 	
			measurement_dates >  start_date & 
			measurement_dates <= stop_date &
			!is.na(measurements)
	
		## Quality check:
		measurement_dates <- measurement_dates[included]
		days_in_interval <- suppressWarnings(expr={days(round(stop_date - start_date))})
		days_measured <- days(sum(included))
		if ( (days_measured / days_in_interval) < 0.9 ) return(as.numeric(NA))
	
		## Reduction:
		summary <- subset_reduction(included, measurements, mean)
		
		return(summary)	
	},
	zsd = function(start_date, stop_date, covariate_table) {
		measurement_dates <- covariate_table[['date_ct']]
		measurements <- covariate_table[['zsd']]
		
		## Is covariate defined:
		if (is.na(start_date) || is.na(stop_date)) return(as.numeric(NA))
	
		## Subset:
		included <- 	
			measurement_dates >  start_date & 
			measurement_dates <= stop_date &
			!is.na(measurements)
	
		## Quality check:
		measurement_dates <- measurement_dates[included]
		days_in_interval <- suppressWarnings(expr={days(round(stop_date - start_date))})
		days_measured <- days(sum(included))
		if ( (days_measured / days_in_interval) < 0.9 ) return(as.numeric(NA))
	
		## Reduction:
		summary <- subset_reduction(included, measurements, mean)
		
		return(summary)	
	},
	surviving = function(stop_date, recruit_date, last_detection) {
		surviving <- rep(0,length(stop_date))
		known_surviving <-
			recruit_date < stop_date &
			stop_date <= last_detection
		surviving[known_surviving] <- 1
		return(surviving)
	}
)



