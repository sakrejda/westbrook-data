## formalArgs can get arg name, use it for mapping!
column_code <- list(
	tag = function(tag) {
		return(tag)
	},
	tag_number = function(tag, conn) {
		tag_number <- as.numeric(map_tags(tag,conn))	
		return(tag_number)
	},
	detection_date = function(date) {
		require(lubridate)
		detection_date <- parse_date_time(x=date, orders='mdyhms') - years(100)
		return(detection_date)
	},
	
)

