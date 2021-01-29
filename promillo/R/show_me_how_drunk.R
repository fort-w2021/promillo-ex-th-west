#' Plotfunction for permille
#'
#' The function plots the decline of blood-level alcohol over time in 5-minute steps.
#' @inheritParams tell_me_how_drunk
#' @return qplot
#' @export show_me_how_drunk
#' @importFrom ggplot2 qplot

show_me_how_drunk <- function(age, sex, height, weight, drinking_time, drinks) {
  permille_vector <- numeric(0)
  partytime <- difftime(drinking_time[2], drinking_time[1], units = "mins")
  partytime <- as.numeric(partytime)
  drinking <- c(drinking_time[1], drinking_time[1])
  time_minutes <- numeric(0)
  for (i in seq(from = 0, to = partytime, by = 5)) {
    drinking <- c(drinking[1], drinking[1] + 60 * i)
    permille_vector[i] <- tell_me_how_drunk(age, sex, height, weight,
      drinking_time = drinking, drinks
    )
    time_minutes[i] <- i
  }
  permille_vector <- permille_vector[!is.na(permille_vector)]
  time_minutes <- time_minutes[!is.na(time_minutes)]
  ggplot2::qplot(
    x = time_minutes, y = permille_vector, geom = "line",
    ylab = "Promille", xlab = "Zeit in Minuten"
  )
}
