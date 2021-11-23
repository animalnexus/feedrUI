#' Download data from original animalnexus database
#'
#' This function uses RCurl to submit an HTML form and retrieve the csv file
#' from the animalnexus database.
#'
#' This function is flexible with respect to date formats. Using
#' [lubridate::parse_date_time()] from the lubridate package, the
#' format of the date/time supplied will estimated. This allows for partial
#' date/times to be supplied (e.g., "2015-01-01 09" or "2015-09" or
#' "2015-09-01"). For best results, specify the date/time format as YYYY-MM-DD
#' HH:MM:SS and truncate as needed. Note that truncated times are interpreted as
#' 00 and trucated dates as the first of the month and the first month of the
#' year. Thus "2015" will be sumbitted as "2015-01-01 00:00:00".
#'
#' `species` options are:
#' \itemize{
#'   \item House Finch
#'   \item Mountain Chickadee
#'   \item Dark-eyed Junco
#'   \item Green Hermit
#'   \item Violet Sabrewing
#'   \item Rufous-tailed Hummingbird
#'   \item Stripe-throated Hermit
#'   }
#'
#' `site_id` options are either 'kl' for Kamloops, BC, or 'cr' for Costa
#' Rica. Note that Costa Rican data is protected while the scientist work on
#' publishing. Only users with valid credentials will be able to download this
#' data.
#'
#' @param start Character. This is the start date (inclusive, with or without
#'   time) for the data to download. There is some flexibility in the format
#'   (see details). If NULL, get records from start.
#' @param end  Character. This is the end date (inclusive, with or without time)
#'   for the data to download. There is some flexibility in the format (see
#'   details). If NULL, get records to end.
#' @param url Character. This is the url for the database service. The default
#'   should not need to be changed.
#' @param tz_disp Character vector. Timezone data should be displayed in (should match one of
#'   the zones produced by `OlsonNames()`)
#' @param dst Logical. Whether or not to use Daylight Savings. When set to FALSE
#'   timezones are converted to the Etc/GMT+X timezones which do not include
#'   DST. (Note this overrides the timezone specification such that a timezone
#'   of America/Vancouver, which would normally include DST in the summer, will
#'   be transformed to a timezone with the same GMT offset, but not including
#'   DST).
#' @param species Character. Vector of species to include (defaults to all). See
#'   details for valid entries.
#' @param site_id Character. Vector of sites to include (defaults to all
#'   permissible). See details for valid entries.
#' @param logger_details Deprecated.
#' @param feeder_details Deprecated.
#' @param animal_details Deprecated.
#' @param bird_details Deprecated.
#'
#' @examples
#' \dontrun{
#'
#' # Get all data (may take a couple minutes)
#' r <- dl_data()
#'
#' # Get all 2016 data
#' r <- dl_date(start = "2016")
#'
#' # Get specific data
#' r <- dl_data(start = "2016-01-01 09:34:12",
#'               end = "2016-02-01")
#' }
#'
#' @export
dl_data <- function(start = NULL,
                    end = NULL,
                    url = "http://gaia.tru.ca/birdMOVES/rscripts/anquery.csv",
                    tz_disp = "Etc/GMT+8",
                    dst = FALSE,
                    species = NULL,
                    site_id = NULL,
                    feeder_details, bird_details,
                    logger_details, animal_details) {

  if (!missing(feeder_details)) {
    warning("Argument feeder_details is deprecated (all logger details will be returned)",
            call. = FALSE)
  }

  if (!missing(bird_details)) {
    warning("Argument bird_details is deprecated (all animal details will be returned)",
            call. = FALSE)
  }

  if (!missing(logger_details)) {
    warning("Argument logger_details is deprecated (all logger details will be returned)",
            call. = FALSE)
  }

  if (!missing(animal_details)) {
    warning("Argument animal_details is deprecated (all animal details will be returned)",
            call. = FALSE)
  }

  # Timezone checks
  tz_disp <- feedr:::check_tz(tz_disp)
  if(!dst) tz_disp <- feedr::tz_offset(tz_disp, tz_name = TRUE)

  # Stop if time is not in the correct format
  t_start <- NULL
  t_end <- NULL
  if(!is.null(start)) {
    suppressWarnings(t_start <- lubridate::parse_date_time(start, orders = "ymd HMS", truncated = 5, tz = tz_disp))
    if(is.na(t_start)) stop("Your start time is ambiguous. Format should be YYYY-MM-DD (HH:MM:SS is optional)")
    if(format(t_start, "%H:%M:%S") == "00:00:00") t_start <- as.Date(t_start)
    t_start <- lubridate::with_tz(t_start, tz = "UTC")
  } else t_start <- as.POSIXct("2015-09-02")
  if(!is.null(end)) {
    suppressWarnings(t_end <- lubridate::parse_date_time(end, orders = "ymd HMS", truncated = 5, tz = tz_disp))
    if(is.na(t_end)) stop("Your end time is ambiguous. Format should be YYYY-MM-DD (HH:MM:SS is optional)")
    if(format(t_end, "%H:%M:%S") == "00:00:00") t_end <- as.Date(t_end) + lubridate::days(1)
    t_end <- lubridate::with_tz(t_end, tz = "UTC")
  } else t_end <- Sys.time()


  # Stop if url doesn't exist
  if(!curl::has_internet()) stop("No internet connection")

  # Get form options
  qry <- paste0("time::timestamp >= '", t_start, "' AND ",
                "time::timestamp <= '", t_end, "'")

  if(!is.null(species)) {
    species <- feedr:::species_list[tolower(feedr:::species_list) %in% tolower(species)]
    qry <- paste0(qry,
                  " AND engl_name IN ( '",
                  paste0(species, collapse = ","), "' )")
  }

  if(!is.null(site_id)) qry <- paste0(qry,
                                      " AND fieldsites.site_id IN ( '",
                                      paste0(site_id, collapse = ", "), "')")

  g <- RCurl::getForm(url, where = qry, key = check_db())

  if(nchar(g) < 80) stop("There are no online data matching these parameters. Try different url or a different date range.")

  l <- RCurl::getForm(url_loggers, key = check_db()) %>%
    utils::read.csv(text = ., strip.white = TRUE, colClasses = "character") %>%
    load_format(verbose = FALSE) %>%
    dplyr::mutate(logger_id = as.character(logger_id),
                  site_name = as.character(site_name))

  r <- load_format(utils::read.csv(text = g, strip.white = TRUE, colClasses = "character"),
                   tz = "UTC", tz_disp = tz_disp, verbose = FALSE) %>%
    dplyr::rename(species = engl_name) %>%
    dplyr::select(-site_id) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(logger_id = as.character(logger_id),
                  site_name = as.character(site_name)) %>%
    dplyr::left_join(l, by = c("logger_id", "site_name")) %>%
    load_format(tz = tz_disp, verbose = FALSE)

  return(r)
}
