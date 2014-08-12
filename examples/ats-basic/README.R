root <- '~/projekty/westbrook_ats_survival'

do <- list(
	data_preparation = c(
		'load_data.R',
		'covariate_calculations.R',
		'bind_state_table.R'
	)
)

source(
	file=file.path(root,'data_preparation','shared_data.R'), 
	echo=TRUE, verbose=TRUE
)
for (stage in names(do)) {
	for (script in do[[stage]]) {
		temp <- new.env(parent=shared_data)
		temp[['shared_data']] <- shared_data
		with(
			data=temp,
			expr= {
				s <- file.path(root,stage,script)
				cat(s,"\n")
				source(file=s, local=TRUE, echo=TRUE, verbose=TRUE)
			}
		)
		rm(envir=temp, list='shared_data')
		rm(temp)
	}
}



