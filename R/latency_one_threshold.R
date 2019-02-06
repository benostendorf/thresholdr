##' Calculate latency and status for meeting threshold
##'
##' This function takes as input a matrix with longitudinal measurements (e.g., volumes)
##' and timepoints and returns a matrix with status (threshold reached vs.
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
##' @export
latency_one_threshold <- function(dataset,
                                  object,
                                  identifier = UID,
                                  time = age,
                                  group = group,
                                  size_threshold) {
  object_enquo <- enquo(object)
  identifier_enquo <- enquo(identifier)
  time_enquo <- enquo(time)
  group_enquo <- enquo(group)

  dataset %>%

    # annotate outcome status (threshold met/censored)
    mutate(status = ifelse((!!object_enquo) > size_threshold, 1, 0)) %>%

    # group by both identifier and threshold status
    group_by(!!identifier_enquo, status) %>%

    # summarize results; in cases where threshold not reached capture maximum time
    # at which data censored; in cases where threshold was met capture
    # minimal time at exceeding it
    summarize(latency = ifelse(max(status) == 0,
                               max(!!time_enquo),
                               min(!!time_enquo)),
              group = (!!group_enquo)[1]) %>%

    # reverse order in order to then remove duplicates for status == 0
    arrange(-row_number()) %>%
    distinct(!!identifier_enquo, .keep_all = TRUE)
}
