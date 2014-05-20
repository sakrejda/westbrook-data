columns <- list(
	who = c('tag'),
	when = c('date_known_dead')
)

select_stmt <- paste(
		"SELECT", paste(unlist(columns), collapse=', '), 
		"FROM", "tags_dead"
	)

dbSendQuery(link_1$conn, "DROP TABLE tags_found")
create_query <- paste0(
	"CREATE TABLE tags_found AS ",
	"(", select_stmt, ");"
)
dbSendQuery(link_1$conn, create_query)


