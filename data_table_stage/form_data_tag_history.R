recaptures <- dbGetQuery(link$conn, 
	statement = "SELECT * FROM data_seasonal_recaptures;")

recaptures[['status']] <- 'recaptured'

tags <- unique(recaptures[['tag']])

trap_recaptures <- dbGetQuery(link$conn, "SELECT * FROM data_trap_recaptures;")
trap_recaptures <- trap_recaptures[ trap_recaptures[['tag']] %in% tags,]
boundary_detections <- dbGetQuery(link$conn, "SELECT * FROM data_boundary_detections;")
boundary_detections <- boundary_detections[ boundary_detections[['tag']] %in% tags,]

detections <- rbind(
	data.frame(
		trap_recaptures[,c('tag','detection_date','observed_length','survey')],
		sample_name='trap', status='trap_recapture'),
	data.frame(boundary_detections[,c('tag','detection_date','survey')], 
		observed_length=NA, sample_name='antenna_detection',
		status='boundary_detection')
)

extra_columns_for_detections <- names(recaptures)[!(names(recaptures) %in% names(detections))]
extra_columns_for_recaptures <- names(detections)[!(names(detections) %in% names(recaptures))]
for (col in extra_columns_for_detections) { detections[[col]] <- NA }
for (col in extra_columns_for_recaptures) { recaptures[[col]] <- NA }

detections <- detections[,names(recaptures)] ## Sets column order!
tag_history <- do.call(what=rbind, args=list(recaptures, detections))
tag_history <- tag_history[order(tag_history[['tag']], tag_history[['detection_date']]),]

dbWriteTable(link$conn, 'data_tag_history', tag_history, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



