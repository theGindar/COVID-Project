# Deprecated
# x: input data as list
# n: moving average window size
moving_average <- function(x, n) {
  return(stats::filter(x, rep(1 / n, n), sides = 2))
}

#' Gibt zurück, in welchem Bundesland sich ein Landkreis befindet.
#'
#' @param district_id IdLandkreis (Amtlicher Gemeindeschlüssel, AGS) des Landkreises
#' @return Bundeslandname
get_federal_state_by_district_id <- function(district_id) {
  federal_state_numbers <- list(
    "Schleswig-Holstein" = 1,
    "Hamburg" = 2,
    "Niedersachsen" = 3,
    "Bremen" = 4,
    "Nordrhein-Westfalen" = 5,
    "Hessen" = 6,
    "Rheinland-Pfalz" = 7,
    "Baden-Württemberg" = 8,
    "Bayern" = 9,
    "Saarland" = 10,
    "Berlin" = 11,
    "Brandenburg" = 12,
    "Mecklenburg-Vorpommern" = 13,
    "Sachsen" = 14,
    "Sachsen-Anhalt" = 15,
    "Thüringen" = 16
  )
  names(federal_state_numbers[federal_state_numbers ==  as.integer(district_id/1000)])
}
