
#' @export
#' @importFrom lightr create_context get_package_function_names
create_sequencr_context <- function() {
    create_context(application_load_callback = application_load_callback,
                   application_unload_callback = application_unload_callback,
                   call_exit_callback = call_exit_callback,
                   functions = get_package_function_names("dplyr", public = TRUE, private = FALSE))
}

#' @importFrom lightr set_data
application_load_callback <- function(context, application) {
    set_data(context, list(df = NULL,
                           sequences = list(),
                           active = character(0)))
}

#' @importFrom lightr set_data get_data get_name
application_unload_callback <- function(context, application) {
    data <- get_data(context)

    sequences <- data$sequences

    if (length(data$active) > 0) {
        index <- length(sequences) + 1
        sequences[[index]] <- data$active
    }

    index <- 1

    df <- data.frame(program = character(0),
                     name = character(0),
                     id = integer(0),
                     parent_id = integer(0))

    for(sequence in sequences) {
        parent_index <- -1
        for(fun_name in sequence) {
            df[nrow(df) + 1, ] <- list(get_name(application), fun_name, index, parent_index)
            parent_index <- index
            index <- index + 1
        }
    }

    set_data(context, df)
}


#' @importFrom lightr set_data get_data get_call_stack get_size get_name
#' @importFrom lightr get_parameters get_arguments is_evaluated get_result
#' @importFrom lightr is_successful
call_exit_callback <- function(context, application, package, func, call) {

    call_stack <- get_call_stack(application)

    if(get_size(call_stack) != 0) return(NULL)

    fun_name <- get_name(func)

    parameters <- get_parameters(call)

    if(length(parameters) == 0) return(NULL)

    first_argument <- get_arguments(parameters[[1]])[[1]]

    if(!is_evaluated(first_argument)) return(NULL)

    argument_result <- get_result(first_argument)

    if(!is.data.frame(argument_result)) return(NULL)

    call_result <- get_result(call)

    if(!is_successful(call)) return(NULL)

    data <- get_data(context)

    df <- data$df

    if (identical(df, argument_result)) {
        data$active <- c(data$active, fun_name)
    }
    else {
        if (length(data$active) > 0) {
            sequences <- data$sequences
            index <- length(sequences) + 1
            sequences[[index]] <- data$active
            data$sequences <- sequences
        }
        data$active <- fun_name
    }

    data$df <- call_result

    set_data(context, data)
}
