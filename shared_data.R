library(RPostgreSQL)
library(lubridate)

source('~/data_management/generic/data_cleaning_functions.R')
options(stringsAsFactors=FALSE)
options(check.names=FALSE)
options(mc.cores=6)

pgsql <- readRDS("~/credentials/pgsql-pass-salmonids-local-db.rds")

root_data_dir <- '~/data_store/westbrook'
original_data_dir <- file.path(root_data_dir,'original_data')
adjusted_data_dir <- file.path(root_data_dir,'adjusted_data')
processed_data_dir <- file.path(root_data_dir,'processed_data')

tag_data_names <- c(
	"tags_antenna", "tags_dead", 
	"tags_salmon_wb", "tags_trout_wb", "tags_tribs_wb"
)

csv_files <- paste(file.path(adjusted_data_dir, tag_data_names), '.csv', sep='')
names(csv_files) <- tag_data_names
standardize_files <- paste(file.path(adjusted_data_dir, tag_data_names), '_standardize.R', sep='')
names(standardize_files) <- tag_data_names

unknowns <- list(-9999, "-9999", '-9999\r\n-9999', '', -999, -99,
								 -99999,"9999",9999, "np")








