
script_order <- c(
	"shared_data.R",
	"import_to_sql.R",
	"form_table_deaths.R",
	"form_table_detections.R",
	"form_table_recaptures.R",
	
	"form_table_samples.R",
	"form_table_tags.R",
	
	"form_data_detections.R",
	"form_data_emigrations.R",
	"form_data_recaptures.R",
	
	"form_data_seasonal.R",
	"form_data_salmon_survival.R",
	"form_data_smolts.R"
)

for (s in script_order) {
	cat(s,"\n")
	source(s)
}
