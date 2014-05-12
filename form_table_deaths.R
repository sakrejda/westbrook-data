columns <- list(
	who = c('tag'),
	when = c('date_known_dead')
)

select_stmt <- paste(
		"SELECT", paste(unlist(columns), collapse=', '), 
		"FROM", "tags_dead"
	)

dbSendQuery(conn, "DROP TABLE tags_found")
create_query <- paste0(
	"CREATE TABLE tags_found AS ",
	"(", select_stmt, ");"
)
dbSendQuery(conn, create_query)


