
#' @export
#' @importFrom lightr trace_code is_error get_error get_source get_message get_call
#' @importFrom utils write.csv
trace_sequence <- function(code,
                           datadir = file.path(getwd(), ".sequence"),
                           envir = parent.frame(),
                           quote = TRUE) {
    if (quote) {
        code <- substitute(code)
    }

    context <- create_sequencr_context()

    result <- trace_code(code, context, envir, quote = FALSE)

    sequence_data <- get_data(context)

    ## create datadir
    dir.create(datadir, showWarnings = FALSE)

    ## write sequence data
    data_file_path <- file.path(datadir, "sequence.csv")
    write.csv(sequence_data, data_file_path, row.names = FALSE)

    ## handle error
    if (is_error(result)) {
        status_file <- file.path(datadir, "ERROR")
        error <- get_error(result)
        error_data <- data.frame(source = get_source(error),
                                 message = get_message(error),
                                 call = paste(deparse(get_call(error)), sep = "\n"))
        write.csv(error_data, status_file, row.names = FALSE)
    }

    result
}
