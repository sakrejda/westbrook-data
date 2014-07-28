
split_state <- mcmapply(
		FUN = function(state, tag_data, pipeline_code, age_breaks, env_table) {
			## Environment for pipeline.
			local_env <- new.env()
			local_env[['breaks']] <- age_breaks
			local_env[['verified_cohort']] <- tag_data[['cohort']]
			local_env[['birthday']] <- tag_data[['possible_capture_lower_bound']]
			local_env[['recruit_date']] <- tag_data[['first_capture']]
			local_env[['last_detection']] <- tag_data[['last_detection']]
			local_env[['covariate_table']] <- env_table

			state.bk <- state
			state <- pipeline_data_transformation(
				data=state, 
				pipeline=pipeline_code,
				envir=local_env,
				final_names='all'
			)
			if (!is.data.frame(state)) stop("HERE.")
			return(state)
		},
		state = split_state,
		tag_data = id_tables,
		MoreArgs = list(
			pipeline_code = covariate_pipeline,
			age_breaks = occasions[['detection_date']],
			env_table = edj
		),
		mc.cores = getOption("mc.cores", 6L),
		SIMPLIFY=FALSE
	)

assign(x='split_state', value=split_state, envir=shared_data)

