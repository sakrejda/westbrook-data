## Load state bits:
occasion_table <- dbGetQUery(link$conn, "SELECT * FROM state_occasion_rows;")

sampling_table <- dbGetQUery(link$conn, "SELECT * FROM state_sample_rows;")
sampling_table <- sampling_table[,names(occasion_table)]

## Load tag histories:
history_table <- dbGetQuery(link$conn, "SELECT * FROM data_corrected_history_table;")
history_table <- unique(history_table)
history_table <- history_table[,names(occasion_table)]

## Merge with rbind into one table:
state <- do.call(
  what=rbind,
  args=list(occasion_table, sampling_table, history_table)
)

## Sort
state <- state[
  order(state[['tag']], state[['detection_date']]),]

## Write:

dbWriteTable(conn=link$conn, name='state_table', value=state, overwrite=TRUE, row.names=FALSE)




