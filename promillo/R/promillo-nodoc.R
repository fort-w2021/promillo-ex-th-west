#' Bloodlevel alcohol of a person in permille
#'
#' This function calculates the blood level alcohol in permille using height,
#' weight, sex, the time of drinking, age an number of specific drinks.
#' @param age the age of the person. Single number.
#' @param sex single character. Possible choices are "male" and "female".
#' @param height the height in cm of that person. Single number.
#' @param weight the weight in kg of that person. Single number.
#' @param drinking_time a POSIXct vector of length 2. First argument is the
#'   time the person started to drink alcohol, second argument is the end of
#'   drinking.
#' @param drinks a named numeric vector of drinks. Possible choices
#'   "massn", "hoibe", "wein" and "schnaps"
#' @return The function returns the permille value after ending the drinking
#'   process
#' @export tell_me_how_drunk
#'
#' @examples
#' tell_me_how_drunk(
#'   age = 39, sex = "male", height = 190, weight = 87,
#'   drinking_time = as.POSIXct(c("2016-10-03 17:15:00", "2016-10-03 22:55:00")),
#'   drinks = c("massn" = 3, "schnaps" = 4)
#' )
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight, drinks)
  get_permille(alcohol_drunk, bodywater, drinking_time, units = "hours")
}

# utilities --------------------------------------------------------------------
#' Alcohol mass
#'
#' The function calculates how much alcohol a person consumed.
#' @inheritParams tell_me_how_drunk
#' @return The function returns a numeric value indicating the alcohol mass.
get_alcohol <- function(drinks) {
  # homogenize inputs:
  drinks <- unlist(drinks)
  checkmate::assert_subset(names(drinks),
    choices = c(
      "massn", "hoibe",
      "wein", "schnaps"
    ),
    empty.ok = FALSE
  )
  checkmate::assert_numeric(drinks, lower = 0)

  volume <- c(
    "massn"    = 1000,
    "hoibe"   = 500,
    "wein"    = 200,
    "schnaps" = 40
  )
  alcohol_concentration <- c(
    "massn"    = 0.06,
    "hoibe"   = 0.06,
    "wein"    = 0.11,
    "schnaps" = 0.4
  )
  alcohol_density <- 0.8

  sum(drinks * volume[names(drinks)] *
    alcohol_concentration[names(drinks)] * alcohol_density)
}

#' Body Water of a person
#'
#' The function calculates the body water of a specific person using age,
#' sex, height, weight. In addition the function performs an age check using
#' the age of a person and the drinks said person consumed.
#'
#' @inheritParams tell_me_how_drunk
#' @details `get_bodywater` uses the Whatson formula. see references.
#' @references [Erlaeuterung der Promille-Berechnung](https://web.archive.org/web/20150123143123/http://promille-rechner.org/erlaeuterung-der-promille-berechnung/)
#' @return Function returns the calculated Body Water.
get_bodywater <- function(sex = c("male", "female"),
                          age, height, weight, drinks) {
  sex <- tolower(sex)
  sex <- match.arg(sex)
  drinks <- unlist(drinks)

  checkmate::assert_number(age, lower = 10, upper = 110)
  age_class <- ifelse("schnaps" %in% names(drinks), "adult", "minor")
  if (age_class == "adult" & age < 18) {
    warning(" Consumption of hard liquor for minors is illegal.")
  }

  if (age_class == "minor" & age < 16) {
    warning("Consumption of alcohol for minors younger than 16 is illegal.")
  }
  checkmate::assert_number(height, lower = 100, upper = 230)
  checkmate::assert_number(weight, lower = 40, upper = 300)

  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}

#' Permille
#'
#' Internal function that calculates permille using alcohol mass, bodywater
#' and drinking time.
#' @param alcohol_drunk a single numeric value of the alcohol mass in gram.
#' @param bodywater a single numeric value of body water.
#' @param drinking_time a POSIXct vector of length 2. First argument is the
#'   time the person started to drink alcohol, second argument is the end of
#'   drinking.
#' @param units a single value of type "character". Possible choices are "hours",
#'   "mins".
#' @return The function returns the permille value of a person after drinking for
#'   more than one hour.
get_permille <- function(alcohol_drunk, bodywater, drinking_time, units = "hours") {
  checkmate::assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)

  partylength <- difftime(drinking_time[2], drinking_time[1], units = units)
  if (units == "hours") {
    sober_per_hour <- 0.15
    # sobering up starts only after one hour & you can't be more sober than 0:
    max(0, permille - (max(0, partylength - 1) * sober_per_hour))
  } else {
    sober_per_minute <- 0.0025
    # sobering up starts only after one hour & you can't be more sober than 0:
    max(0, permille - (max(0, partylength - 60) * sober_per_minute))
  }
}
