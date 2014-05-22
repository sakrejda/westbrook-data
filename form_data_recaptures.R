column_code <- list(
	tag = function(tag) {
		return(tag)
	},
	species = function(species) return(species),
	cohort = function(cohort) return(cohort),
	sample_number = function(sample_name) {
		sample_number <- sample_name_to_sample_number(sample_name)
		return(sample_number)
	},
	detection_date = function(date) {
		require(lubridate)
		detection_date <- parse_date_time(x=date, orders='mdyhms')
		detection_date[detection_date > now()] <- 
			detection_date[detection_date > now()] - years(100)
		return(detection_date)
	},
	season_number =  function(detection_date) {
		season <- day_of_year_to_season(yday(detection_date), output='season_number')
		return(season)
	},
	river = function(river) return(river),
	area = function(area) return(area),
	section = function(section) return(section),
	observed_length = function(measured_length) {	
		observed_length <- as.numeric(measured_length)
	},
	survey = function(survey) return(survey),
	sample_name = function(sample_name) return(sample_name)
)


source_data <- dbGetQuery(link$conn, "SELECT * FROM tags_recaptures;")
source_data <- pipeline_data_transformation(
	data=source_data, pipeline=column_code)

dbWriteTable(link$conn, 'data_recaptures', source_data, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



