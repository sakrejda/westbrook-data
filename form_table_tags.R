columns <- list(
	who = c('tag')
)

recaptures_stub <- paste(
		"SELECT", paste(unlist(columns), collapse=', '), "FROM"
	)

queries <- list()
for ( nom in tag_data_names ) {
	queries[[nom]] <- paste(recaptures_stub, nom, "WHERE tag IS NOT NULL")
}

if (getOption('verbose',FALSE)) print(queries)

tags <- data.frame(tag=NULL, species=NULL)
for (query in queries) {
	tags <- rbind(tags,dbGetQuery(conn, paste0(query,";")))
	tags <- unique(tags)
}
tags[['tag_number']] <- as.numeric(factor(x=tags[['tag']]))

dbSendQuery(conn,'DROP TABLE tags;')
dbWriteTable(conn_write, 'tags', tags, row.names=FALSE)

get_tags <- function(conn) {
	tags <- dbGetQuery(conn, "SELECT * FROM tags ORDER BY tag_number;")
	tags_f <- factor(x=tags$tag_number, labels=tags$tag)
	return(tags_f)
}

map_tags <- function(tags, conn) {
	tag_map <- get_tags(conn)	
	tags <- tag_map[as.character(tag_map) == tags]
	return(tags)
}


