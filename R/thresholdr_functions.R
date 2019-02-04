##' Calculate object volume
##'
##' This functions calculates the volume of an object given width and length
##' using the formula for an ellipsoid.
##' @param width numeric; width of object
##' @param length numeric; length of object
##' @return The rounded object volume
##' @author Benjamin Ostendorf
##' @export
calculate_volume <- function(width, length) {
    volume <- pi/6 * (width ^ 2) * length
    volume <- round(volume, 0)
    }


##' Calculate latency and status for meeting threshold
##'
##' This function takes a matrix with longitudinal measurements (e.g., volumes) and
##' timepoints as input and returns a matrix with status (given threshold reached vs.
##' censored) and latency.
##' @param dataset matrix
##' @param object (unquoted) variable in 'dataset' to calculate threshold time for
##' @param identifier (unquoted) variable in 'dataset' defining samples/replicates;
##'    one sample can have several pertaining objects
##' @param time (unquoted) variable in 'dataset' indicating time at measurement
##' @param group (unquoted) variable in 'dataset' indicating groups samples pertain to
##' @param size_threshold numeric; set threshold size
##' @return matrix containing time and status (threshold reached/censored)
##' @import dplyr
##' @author Benjamin Ostendorf
##' @export
time_to_one_threshold <- function(dataset,
                                  object,
                                  identifier = UID,
                                  time = age,
                                  group = group,
                                  size_threshold) {

  # create enquosure to use dplyr inside function
  object_enquo <- enquo(object)
  identifier_enquo <- enquo(identifier)
  time_enquo <- enquo(time)
  group_enquo <- enquo(group)

  dataset %>%

    # create variable to annotate each row whether threshold reached or not
    mutate(status = ifelse((!!object_enquo) > size_threshold, 1, 0)) %>%

    # group by both identifier and threshold status
    group_by(!!identifier_enquo, status) %>%

    # summarize results; in cases where threshold not reached capture maximum time
    #at which data censored; in cases where threshold was met capture
    # minimal time at exceeding it
    summarize(latency = ifelse(max(status) == 0,
                               max(!!time_enquo),
                               min(!!time_enquo)),
              group = (!!group_enquo)[1]) %>%

    # reverse order in order to then remove duplicates for status == 0
    arrange(-row_number() ) %>%
    distinct(!!identifier_enquo, .keep_all = TRUE)
  }


##' Calculate latency and status between thresholds
##'
##' This function takes a matrix with longitudinal measurements (e.g., volume) and
##' timepoints as input and returns a matrix holding the status (second threshold
##' reached vs. censored) and latency between two given thresholds.
##' @param dataset matrix
##' @param object (unquoted) variable in 'dataset' to calculate latency for
##' @param identifier (unquoted) variable in 'dataset' defining samples/replicates;
##'    one sample can have several pertaining objects
##' @param time (unquoted) variable in 'dataset' indicating time at measurement
##' @param group (unquoted) variable in 'dataset' indicating groups samples pertain to
##' @param lower_threshold numeric; lower threshold
##' @param upper_threshold numeric; upper threshold
##' @import dplyr
##' @author Benjamin Ostendorf
##' @export
latency_two_thresholds <- function(dataset,
                                   object,
                                   identifier = UID,
                                   time = age,
                                   group = group,
                                   lower_threshold,
                                   upper_threshold) {

  # Enquosures to use dplyr inside function
  object_enquo <- enquo(object)
  identifier_enquo <- enquo(identifier)
  time_enquo <- enquo(time)
  group_enquo <- enquo(group)

  dataset %>%

    # Filter dataset to include only rows where object palpable
    dplyr::filter((!!object_enquo) > (!!lower_threshold)) %>%

    # Create variable to annotate each row whether upper threshold was reached or not
    mutate(status_upper_threshold = ifelse((!!object_enquo) > upper_threshold, 1, 0)) %>%

    # Group for both identifier and whether upper threshold was met or not
    group_by(!!identifier_enquo, status_upper_threshold) %>%

    # Calculate time at which given object exceeds lower and upper threshold;
    # in cases where threshold not reached capture maximum time at which data censored;
    # in cases where threshold was met capture minimal time at exceeding it
    summarize(age_initiation = min(!!time_enquo),
              age_threshold = ifelse(max(status_upper_threshold) == 0,
                                     max(!!time_enquo),
                                     min(!!time_enquo)),
              status = max(status_upper_threshold),
              group = (!!group_enquo)[1]) %>%

    # at this stage, for objects that reached the threshold there are two entries; first, for timepoints
    # in which only lower threshold reached, and second for timepoints in which upper threshold reached;
    # now extract lowest age for initiation and highest age for upper threshold
    group_by(!!identifier_enquo) %>%
    summarize(status = max(status),
              age_initiation = min(age_initiation),
              age_threshold = max(age_threshold),
              latency = age_threshold - age_initiation,
              group = (!!group_enquo)[1])
  }
