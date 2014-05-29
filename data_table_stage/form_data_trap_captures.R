recaptures <- dbReadTable(link$conn,'data_recaptures')

trap_recaptures <- recaptures[
	recaptures[['survey']] %in% c('box trap','screw trap','duda fyke')
,]

trap_recaptures <- unique(trap_recaptures[
	order(trap_recaptures[['species']],
				trap_recaptures[['tag']],
				trap_recaptures[['detection_date']]),])

dbWriteTable(link$conn, 'data_trap_recaptures', trap_recaptures,
						 row.names=FALSE, overwrite=TRUE, append=FALSE)




