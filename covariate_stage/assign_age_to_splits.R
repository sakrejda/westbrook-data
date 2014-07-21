state <- dbGetQuery(link$conn, "SELECT * FROM state_table;")
split_state <- split(x=state, f=state[['tag']])
id_table <- dbGetQuery(link$conn, "SELECT * FROM data_per_tag;")
id_tables <- split(x=id_table, f=id_table[['tag']])
occasions <- dbGetQuery(link$conn, "SELECT * FROM data_occasions;")

ager <- function(date, birthday, breaks=NULL) {
	if (date < birthday) return(0)
	if (is.null(breaks)) {
		if (year(date) == year(birthday)) return(0)
		breaks <- birthday + years(1:(year(date) - year(birthday)))
	}
	relevant <- (breaks > birthday) & (breaks <= date)
	number_of_breaks <- sum(relevant)
	return(number_of_breaks)
}

if (!all(names(split_state) == names(id_tables))) {
	stop("'split_state' data frame and 'id_tables' list must have identical sort order.")
}

split_state <- mcmapply(
	FUN=function(state, tag_data, breaks) {
		if (is.na(tag_data[['cohort']])) {
			state[['age_group']] <- as.numeric(NA)
			return(state)
		} else {
			birthday <- tag_data[['possible_capture_lower_bound']]
			state[['age_group']] <- sapply(
				X=state[['detection_date']], FUN=ager,
				birthday=birthday, breaks=breaks)
			return(state)
		}
		stop("Something is rotten in Denmark.")
	},
	state = split_state,
	tag_data = id_tables,
	MoreArgs = list(breaks = occasions[['detection_date']]),
	SIMPLIFY=FALSE
)




