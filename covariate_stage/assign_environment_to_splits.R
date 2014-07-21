source('../analysis/environmental_smoothing/form_table_temperature_discharge.R')

time_windowed_average <- function(
	start_date, end_date, measurement_dates, measurements
) {
	if (is.na(start_date) || is.na(end_date)) return(as.numeric(NA))
	included <- 	
		measurement_dates >= start_date & 
		measurement_dates <  end_date &
		!is.na(measurements)
	measurements <- measurements[included]
	measurement_dates <- measurement_dates[included]
	if (length(measurements) == 0) return(as.numeric(NA))
	days_in_interval <- suppressWarnings(expr={days(round(end_date - start_date))})
	days_measured <- days(sum(included))
	if ( (days_measured / days_in_interval) < 0.9 ) return(as.numeric(NA))
	return(mean(measurements))	
}

if (!all(names(split_state) == names(id_tables))) {
	stop("'split_state' data frame and 'id_tables' list must have identical sort order.")
}

split_state <- mcmapply(
	FUN=function(state, covariate_names, covariate_table) {
		nr <- nrow(state)
		start_date <- state[['detection_date']][1:nr]
		end_date <- c(state[['detection_date']][2:nr],NA)
		measurement_dates <- covariate_table[['date_ct']]
		for (cov in covariate_names) {
			measurements <- covariate_table[[cov]]
			avg_covariates <- mapply(
				FUN = time_windowed_average,
				start_date = start_date,
				end_date = end_date,
				MoreArgs = list(
					measurement_dates = measurement_dates,
					measurements = measurements
				)
			)
		state[[cov]] <- avg_covariates
		}
		return(state)
	},
	state = split_state,
	MoreArgs = list(
		covariate_names = c('ZST','ZSD'),
		covariate_table = edj[,c('date_ct','ZST','ZSD')]
	),
	SIMPLIFY=FALSE
)

