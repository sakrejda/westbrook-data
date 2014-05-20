columns <- list(
	who = c('tag'),
	when = c('date','sample_name'),
	where = c('river','area','section'),
	how = c('sample_type','survey'),
	stable_trait = c('species','cohort'),
	mutable_trait = c('measured_length','measured_weight')
)

recaptures_stub <- paste(
		"SELECT", paste(unlist(columns), collapse=', '), "FROM"
	)

electrofishing_samples <- c(
	'tags_salmon_wb', 'tags_trout_wb', 'tags_tribs_wb')

queries <- list()
for ( nom in electrofishing_samples ) {
	queries[[nom]] <- paste(recaptures_stub, nom)
}

if (getOption('verbose',FALSE)) print(queries)

dbSendQuery(link_1$conn,"DROP TABLE tags_recaptures_raw;")
create_query <- paste0(
	"CREATE TABLE tags_recaptures_raw AS ",
	"(", queries[[1]], ");"
)
dbSendQuery(link_1$conn, create_query)

for (query in queries) {
	insert_query <- paste0(
		"INSERT INTO tags_recaptures_raw ",
		"(", query, ");"
	)
	dbSendQuery(link_1$conn, insert_query)
}

dbSendQuery(link_1$conn, "DROP TABLE tags_recaptures;")
create_tagged_fish_table <- paste0(
	"CREATE TABLE tags_recaptures AS (SELECT * FROM ",
	"tags_recaptures_raw WHERE tag IS NOT NULL);"
)
dbSendQuery(link_1$conn, create_tagged_fish_table)

