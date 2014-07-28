library(RPostgreSQL)
library(lubridate)
library(integrator)
library(parallel)
library(reshape2)
library(ggplot2)
library(cruftery)

options(stringsAsFactors=FALSE)
options(check.names=FALSE)
options(mc.cores=6)

shared_data <<- local(expr={
	source("~/data_management/westbrook/covariate_pipeline/pipeline_code.R", local=TRUE)
	link <- db_connector("~/credentials/pgsql-pass-salmonids-local-db.rds") 
	data_root <- '~/data_store/westbrook'
	## This is where the global env gets a 'shared_data' as constructed above.
	return(environment(NULL))   
})




