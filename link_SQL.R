    
driver <- dbDriver("PostgreSQL")
conn <- dbConnect(drv=driver,
  user=pgsql[['user']],
  password=pgsql[['password']],
  dbname=pgsql[['dbname']],
  host=pgsql[['host']],
  port=pgsql[['port']]
)

conn_write <- dbConnect(drv=driver, 
	user=pgsql[['user']],
	password=pgsql[['password']],
	dbname=pgsql[['dbname']],
	host=pgsql[['host']],
	port=pgsql[['port']]
)



