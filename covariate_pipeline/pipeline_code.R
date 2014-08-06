
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
	},
	tagged = function(status) {
		tagged <- vector(mode='numeric', length=length(status))
		first <- min(which(status == 'recaptured'))
		tagged[(first+1):length(tagged)] <- 1
		return(tagged)
	},
	censored = function(status) {
		censored <- vector(mode='numeric', length=length(status))
		if (!any(status == 'boundary_detection') &&
				!any(status == 'trap_recapture'    )
		) {
			return(censored)
		} else {
			last <- max(which(status %in% c('trap_recapture','boundary_detection')))
			censored[(last+1):length(censored)] <- 1
		}
		return(censored)
	},
	cjs_classification = function(status, surviving, censored, tagged) {
		cjs_classification <- vector(mode='numeric', length=length(status))
		cjs_classification[tagged == 1 & surviving == 1 & status == 'recaptured'] <- 1
		cjs_classification[tagged == 1 & surviving == 1 & status == 'uncaptured'] <- 2
		cjs_classification[tagged == 1 & surviving == 1 & status == 'boundary_detection'] <- 3
		cjs_classification[tagged == 1 & surviving == 1 & status == 'trap_recapture'] <- 4
		cjs_classification[tagged == 1 & surviving == 1 & status == 'season_break'] <- 5
		cjs_classification[tagged == 1 & surviving == 0 & censored == 0 & 
											 (status == 'season_break' | status == 'uncaptured')] <- 6
		cjs_classification[tagged == 1 & surviving == 0 & censored == 1] <- 7
		return(cjs_classification)
	},
	cjs_row_type = function(cjs_classification) {
		cjs_row_type <- vector(mode='character', length=length(cjs_classification))
		cjs_row_type[ cjs_classification == 6 ] <- 'ambiguous'
		cjs_row_type[ cjs_classification != 6 ] <- 'unambiguous'
		return(cjs_row_type)
	},
	antennas_functional = function(stop_date, antennas_start) {
		antennas_functional <- vector(mode='numeric', length=length(stop_date))
		antennas_functional[stop_date >= antennas_start] <- 1
		return(antennas_functional)
	},
	interval = function(start_date, stop_date) {
		interval <- difftime(stop_date, start_date, units='days')
		return(interval)
	},
	pivot_row = function(cohort, stop_date) {
		pivot_row <- vector(mode='numeric', length=length(stop_date))
		if (!is.na(cohort)) {
			center <- ymd(paste0(as.numeric(cohort)+1,'-05-20'))
		} else {
			center <- round(length(stop_date)/2)
		}
		distances <- as.numeric(stop_date - center)
		pivot <- which(abs(distances) == min(abs(distances)))
		pivot_row[1:(pivot-1)] <- 0
		pivot_row[pivot] <- 1
		pivot_row[(pivot+1):length(cohort)] <- 2
		return(pivot_row)
	}
)



