root <- '~/data_store/westbrook'
adjusted_data <- file.path(root,'adjusted_data')
source('~/data_management/generic/data_cleaning_functions.R')
library(ggplot2)
library(reshape2)


## Data cleaning:
ed <- read.csv(
	file = file.path(adjusted_data,'westbrook_core_environmental_data.csv'), 
	stringsAsFactors=FALSE, colClasses='character',check.names=FALSE) 
names(ed) <- tolower(names(ed))

name_map <- list(
	temperature = 'avg daily temp',  
	temperature_source = 'temp source',
	discharge_source = 'discharge source'
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
ed[['day_of_year']] <- yday(ed[['date_ct']])

dbWriteTable(conn=link_1$conn, name='data_environmental', value=ed,
						 row.names=FALSE, overwrite=TRUE, append=FALSE)

## Construct smooth for temperature:
temperature_smooth <- loess(formula = temperature ~ day_of_year, data=ed, span =1/5)
temperature_average <- data.frame(
	day_of_year=1:366, 
	temperature=predict(object=temperature_smooth, newdata=data.frame(day_of_year=1:366))
)
temperature_plot <- ggplot(
	data=ed, 
	aes(x=day_of_year, y=temperature)
) + geom_point(size=1/2) + 
		geom_line(
			data=temperature_average, 
			aes(x=day_of_year, y=temperature)
		) + xlab("Julian Day") + ylab("Temperature (Celsius)")


## Construct smooth for discharge:
discharge_smooth <- loess(formula = log10(discharge) ~ day_of_year, data=ed, span =1/4)
discharge_average <- data.frame(
	day_of_year=1:366, 
	discharge=predict(object=discharge_smooth, newdata=data.frame(day_of_year=1:366))
)
discharge_plot <- ggplot(
	data=ed, 
	aes(x=day_of_year, y=log10(discharge))
) + geom_point(size=1/2) + 
		geom_line(
			data=discharge_average, 
			aes(x=day_of_year, y=discharge)
		) + xlab("Julian Day") + ylab("Discharge (Log 10)")

## Average environmental data:
average_ed <- data.frame(
	day_of_year = temperature_average[['day_of_year']], 
	typical_temperature = temperature_average[['temperature']],
	typical_log10_discharge=discharge_average[['discharge']]
)
dbWriteTable(conn=link_1$conn, name='data_environmental_average',
						 value=average_ed, row.names=FALSE, overwrite=TRUE, append=FALSE)

# Merge:
edj <- merge(x=ed, y=average_ed, by='day_of_year')
edj[['daily_deviation_from_typical_temperature']] <-
	edj[['temperature']] - edj[['typical_temperature']]
edj[['ZST']] <- scale(edj[['daily_deviation_from_typical_temperature']])
edj[['daily_deviation_from_typical_log10_discharge']] <-
	log10(edj[['discharge']]) - edj[['typical_log10_discharge']]
edj[['ZSD']] <- scale(edj[['daily_deviation_from_typical_log10_discharge']])

## Features:
environmental_covariance_is_seasonal <- ggplot(
	data=edj, 
	aes(x=ZST, y=ZSD, colour=factor(month))
) + geom_point()
	+ facet_wrap( ~ month)

related_to_seasonal_dynamics <- ggplot(
	data=edj, 
	aes(x=day_of_year, y=log10(discharge), colour=factor(month))
) + geom_point(size=1) + 
		geom_line(aes(x=day_of_year, y=typical_log10_discharge)) +
		xlab("Day of Year") + ylab("Discharge (Log 10)")


## Comparison:
comp <- by(
	data=edj,
	INDICES=list(month=edj[['month']], year=edj[['year']]),
	FUN=function(edj) {
		month <- unique(edj[['month']])
		year <- unique(edj[['year']])
		ZSAT <- mean(edj[['temperature']])
		ZSAD <- mean(log(edj[['discharge']]))
		ZST <- mean(edj[['ZST']])
		ZSD <- mean(edj[['ZSD']])
		return(data.frame(
			year=rep(year,2),
			month=rep(month,2), 
			average = c(ZSAT,ZSAD),
			seasonal = c(ZST,ZSD),
			source=c('temperature','discharge')))
	}
)
comp <- do.call(what=rbind, args=comp)
comp_plot <- ggplot(
	data=comp, 
	aes(x=average, y=seasonal, colour=factor(source))
) + geom_point() + 
		facet_wrap( ~ month + source, scales='free')

## So the mean used is different but ultimately the relationship is
## linear. Woohoo!











