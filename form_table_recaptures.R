columns <- list(
	who = c('tag'),
	when = c('date','sample_name'),
	where = c('river','area','section'),
	how = c('sample_type','survey'),
	stable_trait = c('species'),
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

create_query <- paste0(
	"CREATE TABLE tags_electrofishing_raw AS ",
	"(", queries[[1]], ");"
)
dbSendQuery(conn, create_query)

for (query in queries) {
	insert_query <- paste0(
		"INSERT INTO tags_electrofishing_raw ",
		"(", query, ");"
	)
	dbSendQuery(conn, insert_query)
}

create_tagged_fish_table <- paste0(
	"CREATE TABLE tags_electrofishing AS (SELECT * FROM ",
	"tags_electrofishing_raw WHERE tag IS NOT NULL);"
)
dbSendQuery(conn, create_tagged_fish_table)

