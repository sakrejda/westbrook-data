recaptures <- dbGetQuery(conn, "SELECT * FROM data_seasonal_recaptures;")
smolts <- dbGetQuery(conn, "SELECT * FROM data_smolts;")
emigrants <- dbGetQuery(conn, "SELECT * FROM data_emigrations;")
sampling <- dbGetQuery(conn, "SELECT * FROM data_sampling;")






