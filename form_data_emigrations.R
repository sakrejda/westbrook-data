data <- dbGetQuery(link_1$conn, "SELECT * FROM data_detections WHERE
									 detection_date IS NOT NULL;")

emigration_antennas <- c('a1','a2','03','04','05','06','wb above')

boundary_detections <- data[data[['reader_id']] %in% emigration_antennas,]
emigrations <- by(
	data=boundary_detections,
	INDICES=boundary_detections['tag'],
	FUN=function(dat) {
		last_date <- max(dat$detection_date,na.rm=TRUE)		
		dat <- dat[dat[['detection_date']] == last_date,,drop=FALSE]
		dat <- dat[1,,drop=FALSE]
		return(dat)
	}
)
emigrations <- do.call(what=rbind, args=emigrations)

dbWriteTable(link_2$conn, 'data_emigrations', emigrations, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



