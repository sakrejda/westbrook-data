
do <- list(
	import_stage = c(
		"import_to_sql.R",
		"form_table_deaths.R",
		"form_table_detections.R",
		"form_table_recaptures.R",
		
		"form_table_samples.R",
		"calculate_season_breaks.R",
		"form_table_season_map.R",
		"form_table_tags.R"
	),
	
	data_table_stage = c(
		"form_data_detections.R",
		"form_data_emigrations.R",
		"form_data_recaptures.R",
		
		"form_data_seasonal.R",
		"form_data_trap_captures.R",
		"form_data_smolts.R",
		"form_data_tag_history.R"
	),

	assert_stage = c(
		"assert_tag_properties.R",
		"assert_data_errors."
	),

	cleaning_stage = c(
		"fix_data_errors.R"
	)
)
	
source('shared_data.R')
for (stage in names(do)) {
	for (script in do[[stage]]) {
		s <- file.path(stage,script)
		cat(s,"\n")
		source(file=s)
	}
}



