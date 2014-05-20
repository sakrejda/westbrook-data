recaptures <- dbReadTable(link_1$conn,'data_recaptures')
sampling <- dbReadTable(link_1$conn, 'data_sampling')

seasonal_recaptures <- recaptures[ 
	recaptures[['sample_number']] %in%
	sampling[sampling[['seasonal']],'sample_number']
,]

seasonal_recaptures <- seasonal_recaptures[
	order(seasonal_recaptures[['species']],
				seasonal_recaptures[['tag']],
				seasonal_recaptures[['detection_date']]),]

pl <- ggplot(
	data=seasonal_recaptures, 
	aes(x=sample_number, y=as.numeric(as.factor(tag))  )
) + geom_raster()

dbWriteTable(link_1$conn, 'data_seasonal_recaptures', seasonal_recaptures, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)

trap_recaptures <- recaptures[
	recaptures[['survey']] %in% c('box trap','screw trap','duda fyke')
,]

trap_recaptures <- unique(trap_recaptures[
	order(trap_recaptures[['species']],
				trap_recaptures[['tag']],
				trap_recaptures[['detection_date']]),])

dbWriteTable(link_1$conn, 'data_trap_recaptures', trap_recaptures,
						 row.names=FALSE, overwrite=TRUE, append=FALSE)




