recaptures <- dbGetQuery(link_1$conn, "SELECT * FROM data_seasonal_recaptures;")

## Below this size it appears that tagging was not always done---
## the final decision to tag or not tag was a field judgment.
deleting_length_measurement <- recaptures[['observed_length']] < 60
recaptures[['observed_length']][ deleting_length_measurement ] <- NA



smolts <- dbGetQuery(link_1$conn, "SELECT * FROM data_smolts;")
emigrants <- dbGetQuery(link_1$conn, "SELECT * FROM data_emigrations;")
sampling <- dbGetQuery(link_1$conn, "SELECT * FROM data_sampling;")

emigrants <- rbind(
	data.frame(smolts[,c('tag','detection_date','observed_length')],sample_name='smolt_trap'),
	data.frame(emigrants[,c('tag','detection_date')], observed_length=NA, sample_name='antenna_detection')
)






