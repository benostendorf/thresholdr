##' Calculate latency and status between thresholds
##'
##' This function takes as input a matrix with longitudinal measurements (e.g., volume)
##' and timepoints and returns a matrix holding the status (second threshold
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
##' @export
latency_two_thresholds <- function(dataset,
                                   object,
                                   identifier = UID,
                                   time = age,
                                   group = group,
                                   lower_threshold,
                                   upper_threshold) {

  object_enquo <- enquo(object)
  identifier_enquo <- enquo(identifier)
  time_enquo <- enquo(time)
  group_enquo <- enquo(group)

  dataset %>%

    # Filter rows in which object not present/palpable
    dplyr::filter((!!object_enquo) > (!!lower_threshold)) %>%

    # Annotate status (upper threshold reached/censored)
    mutate(status_upper_threshold = ifelse((!!object_enquo) > upper_threshold, 1, 0)) %>%

    # Group for both identifier and status
    group_by(!!identifier_enquo, status_upper_threshold) %>%

    # Calculate time at which given object exceeds lower and upper threshold;
    # in cases where threshold not reached capture maximum time at which data censored;
    # in cases where threshold was met capture minimal time at exceeding it
    summarize(
      age_initiation = min(!!time_enquo),
      age_threshold = ifelse(
        max(status_upper_threshold) == 0,
        max(!!time_enquo),
        min(!!time_enquo)
      ),
      status = max(status_upper_threshold),
      group = (!!group_enquo)[1]
    ) %>%

    # at this stage, for objects that reached the threshold there are two entries; first, for timepoints
    # in which only lower threshold reached, and second for timepoints in which upper threshold reached;
    # now extract lowest age for initiation and highest age for upper threshold
    group_by(!!identifier_enquo) %>%
    summarize(
      status = max(status),
      age_initiation = min(age_initiation),
      age_threshold = max(age_threshold),
      latency = age_threshold - age_initiation,
      group = (!!group_enquo)[1]
    )
}
