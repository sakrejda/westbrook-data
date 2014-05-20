root <- '~/data_store/westbrook'
adjusted_data <- file.path(root,'adjusted_data')
source('~/data_management/generic/data_cleaning_functions.R')

## Data cleaning:
ed <- read.csv(
	file = file.path(adjusted_data,'westbrook_core_environmental_data.csv'), 
	stringsAsFactors=FALSE, colClasses='character',check.names=FALSE) 
names(ed) <- tolower(names(ed))

name_map <- list(
	temperature = 'avg daily temp',  
	temperature_source = 'temp source',
	`discharge_source` = 'discharge source'
)

colnames(ed) <- map(x=colnames(ed), map=name_map)

keep <- c(
	'drainage', 'river', 
	'date',
	'temperature', 'temperature_source',
	'discharge', 'discharge_source'
)

ed <- ed[,keep]

ed[['date_ct']] <- parse_date_time(x=ed[['date']], orders=c('mdyhms')) 
bad <- ed[['date_ct']] > now()
ed[['date_ct']][bad] <- ed[['date_ct']][bad] - years(100)
ed[['temperature']] <- as.numeric(ed[['temperature']])
ed[['discharge']] <- as.numeric(ed[['discharge']])

ed <- ed[tolower(ed[['temperature_source']]) == 'sec 6 dl',]
ed <- ed[tolower(ed[['discharge_source']]) == 'flow extension',]
ed <- ed[order(ed[['date_ct']]),]
ed[['month']] <- month(ed[['date_ct']])
ed[['year']] <- year(ed[['date_ct']])
ed[['yday']] <- yday(ed[['date_ct']])

## Construct smooth for temperature:
temperature_smooth <- loess(formula = temperature ~ yday, data=ed, span =1/5)
temperature_average <- data.frame(
	yday=1:366, 
	temperature=predict(object=temperature_smooth, newdata=data.frame(yday=1:366))
)
temperature_plot <- ggplot(
	data=ed, 
	aes(x=yday, y=temperature)
) + geom_point(size=1/2) + 
		geom_line(
			data=temperature_average, 
			aes(x=yday, y=temperature)
		) + xlab("Julian Day") + ylab("Temperature (Celsius)")


## Construct smooth for discharge:
discharge_smooth <- loess(formula = log10(discharge) ~ yday, data=ed, span =1/4)
discharge_average <- data.frame(
	yday=1:366, 
	discharge=predict(object=discharge_smooth, newdata=data.frame(yday=1:366))
)
discharge_plot <- ggplot(
	data=ed, 
	aes(x=yday, y=log10(discharge))
) + geom_point(size=1/2) + 
		geom_line(
			data=discharge_average, 
			aes(x=yday, y=discharge)
		) + xlab("Julian Day") + ylab("Temperature (Celsius)")



