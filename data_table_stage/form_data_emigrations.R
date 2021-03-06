data <- dbGetQuery(link$conn, "SELECT * FROM data_detections WHERE
									 detection_date IS NOT NULL;")

boundary_antennas <- c('a1','a2','03','04','05','06','wb above')

boundary_detections <- data[data[['reader_id']] %in% boundary_antennas,]

dbWriteTable(link$conn, 'data_boundary_detections', boundary_detections, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



