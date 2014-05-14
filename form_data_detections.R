## formalArgs can get arg name, use it for mapping!
column_code <- list(
	tag = function(tag) {
		return(tag)
	},
	detection_date = function(earliest_detection_date_time) {
		require(lubridate)
		detection_date <- parse_date_time(x=earliest_detection_date_time, orders='mdyhms')
		detection_date[detection_date > now()] <- 
			detection_date[detection_date > now()] - years(100)
		return(detection_date)
	},
	river = function(river) return(river),
	area = function(area) return(area),
	section = function(section) return(section),
	survey = function(survey) return(survey),
	sample_name = function(sample_name) return(sample_name),
	reader_id = function(reader_id) return(reader_id)
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


source_data <- dbGetQuery(conn, "SELECT * FROM tags_detected;")

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

dbWriteTable(conn_write, 'data_detections', source_data, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



