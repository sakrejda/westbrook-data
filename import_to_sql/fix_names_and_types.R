types_list <- list()
for ( name in tag_data_names) {
  e <- new.env()
  source(standardize_files[name], local=e, echo=FALSE)
  data <- read.csv(csv_files[name], check.names=FALSE, colClasses = 'character')
  names(data) <- map(x=names(data), map=as.list(e[['column_map']]))
	for ( nom in names(data)) {
		if (exists('unknowns'))
			data[[nom]] <- map_unknowns(data[[nom]], unknowns)
		if (isTRUE(is.character(data[[nom]]))) {
			data[[nom]] <- tolower(data[[nom]])
			data[[nom]] <- gsub(x=data[[nom]], pattern='\\A[ ]*', replacement='', perl=TRUE)
			data[[nom]] <- gsub(x=data[[nom]], pattern='[ ]*\\z', replacement='', perl=TRUE)
		}
		if (exists('value_map[[nom]]')) {
			data[[nom]] <- map_values(data[[nom]], 
				value_map[[nom]][['input']], value_map[[nom]][['output']])
		}
	}
	write.csv(x=data, file=file.path(processed_data_dir,paste0(name,'.csv')), row.names=FALSE)
	types_list[[name]] <- sapply(data, class)
	names(types_list[[name]]) <- names(data)

	

	# Inspection code:
	#	for ( nom in names(data)) { 
	#		print(paste0("::::::",name, "$",nom,":")); 
	#		uvals <- unique(data[[nom]]);
	#		print(sort(uvals)[1:min(200,length(uvals))]); 
	#		print(min(uvals));
	#		readline() 
	#	}
}





