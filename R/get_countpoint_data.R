#' Gets the data for the selected countpoint
#'
#' @param pool pool object
#' @param count_id integer or character
#' @param count_type text

get_countpoint_data <- function (pool, count_id, count_type) {
  q1 <-
    paste0("SELECT * FROM traffic_gb.aadf_by_direction where count_point_id = ",
           count_id)
  q2 <-
    paste0("SELECT * FROM traffic_gb.mv_aadf_anydir where count_point_id = ",
           count_id)
  q3 <-
    paste0("SELECT * FROM traffic_gb.mv_raw_counts_anydir where count_point_id = ",
           count_id)
  q4 <-
    paste0("SELECT * FROM traffic_gb.raw_counts where count_point_id = ",
           count_id)

  aadf_bydirection <- DBI::dbGetQuery(pool, q1)
  aadf <- DBI::dbGetQuery(pool, q2)
  raw_bydirection <- DBI::dbGetQuery(pool, q4)
  raw <- DBI::dbGetQuery(pool, q3)

  return(
    list(
      countpoint_aadf_bydirection = aadf_bydirection,
      countpoint_aadf = aadf,
      countpoint_raw_bydirection = raw_bydirection,
      countpoint_raw = raw
    )
  )

}
