#' Gets information about the selected countpoint
#'
#' @param df dataframe of countpoints
#' @param count_id integer or character
#' @param count_type text


get_countpoint_info <- function (df, count_id, count_type) {

  df <- sf::st_drop_geometry(df)

  df <- df[df$count_point_id == count_id ,]

    table_data <- df %>% dplyr::select(
      "Id" = count_point_id,
      "Region" = region_name,
      "Authority" = local_authority_name,
      "Road Type" = road_type,
      "Road Name" = road_name,
      "Start Junction" = start_junction_road_name,
      "End Junction" = end_junction_road_name,
      "Length (km)" = length_km
    )

    count_lat <- as.numeric(df %>% dplyr::select(latitude))
    count_lng <- as.numeric(df %>% dplyr::select(longitude))

  return(list(table = table_data, coords = c(count_lat, count_lng)))
}
