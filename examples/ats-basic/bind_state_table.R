state <- do.call(what=rbind, args=split_state)
rownames(state) <- 1:nrow(state)


if (nrow(state) != nrow(unique(state))) {
	warning("Duplicate state rows exist. Dropping them.")
	state.bk <- state
	assign(x='state.bk', value=state.bk, envir=.GlobalEnv)
	state <- unique(state)
}

dbWriteTable(conn=link$conn, name='state',value=state,
						 overwrite=TRUE, row.names=FALSE)


assign(x='state', value=state, envir=shared_data)

