
## Load tables and do subsetting here.
## Load per tag data:
id_table <- dbGetQuery(link$conn, "SELECT * FROM data_per_tag WHERE species = 'ats';")

## Load sample history:
sampling <- dbGetQuery(link$conn, "SELECT * FROM data_sampling;")

## Occasion data:
occasions <- dbGetQuery(link$conn, "SELECT * FROM data_occasions;")

## Load state table and sort it---it must be sorted after loading.
state <- dbGetQuery(link$conn, "SELECT * FROM state_table WHERE species = 'ats';")
state <- state[order(state[['tag']], state[['detection_date']]),]

## Load environmental table:
edj <- dbGetQuery(link$conn, "SELECT date_ct, zst, zsd  FROM data_environmental_with_zst_zsd")

## Sometimes we use splits
split_state <- split(x=state, f=state[['tag']], drop=FALSE)
id_tables <- split(x=id_table, f=id_table[['tag']])


## A few modifications:

## Below this size it appears that tagging was not always done---
## the final decision to tag or not tag was a field judgment.
deleting_length_measurement <- state[['observed_length']] < 60
state[['observed_length']][ deleting_length_measurement ] <- NA

## Dropping rows should happen BEFORE covariate calculations!!

## Drop extra boundary_detection rows, keeping only the last (for censoring).
split_state <- mclapply(
	X = split_state,
	FUN = function(state, tag_data) {
		boundary_detections <- which(state[['status']] == 'boundary_detection')
		N <- length(boundary_detections)
		if (length(boundary_detections) > 1) {
			state <- state[-boundary_detections[1:(N-1)],]
		}
		return(state)
	},
	tag_data=id_tables,
	mc.cores=getOption('mc.cores',6L)
)

state <- do.call(what=rbind, args=split_state)


## Make data globally available in shared_data.
assign(x='id_table', value=id_table, envir=shared_data)
assign(x='id_tables', value=id_tables, envir=shared_data)
assign(x='sampling', value=sampling, envir=shared_data)
assign(x='occasions', value=occasions, envir=shared_data)
assign(x='state', value=state, envir=shared_data)
assign(x='split_state', value=split_state, envir=shared_data)
assign(x='edj', value=edj, envir=shared_data)


