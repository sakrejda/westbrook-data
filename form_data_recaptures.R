## formalArgs can get arg name, use it for mapping!
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


column_code_args <- lapply(
	X = column_code,
	FUN = function(f) {
		arg_names <- formalArgs(f)
		return(arg_names)
	}
)
new_columns <- names(column_code)
source_columns <- sapply(column_code_args,`[`,1, USE.NAMES=FALSE)


source_data <- dbGetQuery(link_1$conn, "SELECT * FROM tags_recaptures;")

for (i in seq_along(source_columns)) {
	print(source_columns[i])
	f <- column_code[[ new_columns[i] ]]
	args <- list()
	for ( arg in column_code_args[[i]] ) {
		args[[arg]] <- with(data=source_data, expr=get(x=arg))
	}
	source_data[[ new_columns[i] ]] <- do.call(what=f, args=args)
}
source_data <- source_data[,new_columns]

dbWriteTable(link_2$conn, 'data_recaptures', source_data, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



