#' Read Fatality Analysis Reporting System data
#'
#' This function reads a .csv file providing the \emph{American public yearly
#' data regarding fatal injuries suffered in motor vehicle traffic crashes}, and
#' returns a tibble.
#'
#' @import readr
#' @import dplyr
#'
#' @param filename A string with the path of the .csv file to read.
#'
#' @return A tibble containing the data, or an error if the file does not exist
#'
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#' library(readr)
#' fars_read("accident_2015.csv.bz2")
#'
#' }
#'
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make data filename corresponding to a given yean
#'
#' This function makes a data filename related to the given \code{year}
#' Warning : The function does not check if the file is available.
#'
#' @param year A string or an integer with the input \code{year}
#'
#' @return A string with the .csv filename for a given
#'   year, and the file path within the package.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' }
#'
#' @seealso \link{fars_read}
#'
#' @export


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read fars datafiles for a given list of years
#'
#' This function is used by \code{fars_summarize_years}
#'
#'
#' @param years A list with a list of years (given as strings)
#'
#' @import dplyr
#' @import magrittr
#
#' @return A dataframe with entries in data by month, or NULL (and a warning) if the
#'  \code{year} is not a valid year in the dataset.
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#' @seealso \link{fars_summarize_years}
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' }
#'
#' @export


fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize data by years
#'
#' This function summarizes  accident data, by year and month.
#'
#' @param years A list of years (given as strings)
#'
#' @return A dataframe with number of accidents by years summarized by month
#'
#' @import dplyr
#' @import tidyr
#' @import magrittr
#'
#' @seealso \link{fars_read_years}
#'
#' @examples
#' \dontrun{
#' plot(fars_summarize_years(2015))
#' fars_summarize_years(c(2015, 2014))
#' }
#'
#' @export


fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Create a map to display accidents by state and year
#'
#' Displays a plot with a state map including the accidents location by year.
#'
#' If the \code{state.num} is invalid the function shows an error
#'
#' @param state.num An Integer with the US State Code (in alphabetical order)
#' @param year A string with the input \code{year}
#'
#' @import maps
#' @import dplyr
#' @import graphics
#'
#' @return None
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#'
#' @examples
#' \dontrun{
#' fars_map_state(49, 2015)
#' }
#'
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
