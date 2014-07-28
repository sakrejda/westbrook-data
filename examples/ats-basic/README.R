do <- list(
	'.' = c(
		'load_data.R',
		'covariate_calculations.R',
		'bind_state_table.R'
	)
)

source('shared_data.R', echo=TRUE, verbose=TRUE)
for (stage in names(do)) {
	for (script in do[[stage]]) {
		temp <- new.env(parent=shared_data)
		temp[['shared_data']] <- shared_data
		with(
			data=temp,
			expr= {
				s <- file.path(stage,script)
				cat(s,"\n")
				source(file=s, local=TRUE, echo=TRUE, verbose=TRUE)
			}
		)
		rm(envir=temp, list='shared_data')
		rm(temp)
	}
}



