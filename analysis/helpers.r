#' initiales a data.frame with the data for the loto analysis
#'
#' @return a data.frame with rows containing the numbers and winnings for
#' various draws
read.data <- function(numberSource = "numbers.csv", payoutSource = "winnings.csv") {
    numbers <- read.csv(numberSource)
    numbers$Date <- as.POSIXct(numbers$Date, format = "%Y-%m-%d")

    winnings <- read.csv(payoutSource)
    winnings$Date <- as.POSIXct(winnings$Date, format = "%Y-%m-%d")

    return(merge(numbers, winnings, by = "Date"))
}

#' calculates and returns the probabilities for every win category in lotto
#'
#' @return a data.frame containing the probabilities to win various combinations
#' in lotto
get.Prob <- function() {
    ret <- data.frame(
        Win6s = choose(43, 0) * choose(6, 6) / choose(49, 6) * 1 / 10,
        Win6 = choose(43, 0) * choose(6, 6) / choose(49, 6) * 9 / 10,
        Win5s = choose(43, 1) * choose(6, 5) / choose(49, 6) * 1 / 10,
        Win5 = choose(43, 1) * choose(6, 5) / choose(49, 6) * 9 / 10,
        Win4s = choose(43, 2) * choose(6, 4) / choose(49, 6) * 1 / 10,
        Win4 = choose(43, 2) * choose(6, 4) / choose(49, 6) * 9 / 10,
        Win3s = choose(43, 3) * choose(6, 3) / choose(49, 6) * 1 / 10,
        Win3 = choose(43, 3) * choose(6, 3) / choose(49, 6) * 9 / 10,
        Win2s = choose(43, 4) * choose(6, 2) / choose(49, 6) * 1 / 10
    )
    return(ret)
}

#' calculates the expected prices for each win class
#'
#' @return a data.frame with the first row containing the probabilites of
#' winning and the second row containing the expected winnings
get.ExpWin <- function() {
    prob <- get.Prob()

    # these are the fractions of income that is distributed to the win classes
    payout.rate <- c(
        0.128,
        0.074,
        0.037,
        0.111,
        0.037,
        0.074,
        0.074,
        0.332,
        0.132
    )

    # for every 1 € played 50 cents are payed out
    expect <- 0.5 * payout.rate / prob

    # the draw2s class has a fixed payout rate
    expect[1, 9] <- 5
    return(rbind(prob, expect))
}

#' calculates the ratios between the expected winnings and the actual payed
#' out amounts
#'
#' @return ratios of realized and expected winnings
get.WinRatios <- function(data, exp.win = get.ExpWin()[2, ]) {
    ratio.win2S <- data$Win2S / exp.win$Win2s

    ratio.win3 <- exp.win$Win3 / data$Win3
    ratio.win3S <- exp.win$Win3s / data$Win3S
    ratio.win3zz <- ratio.win3S / ratio.win3

    ratio.win4 <- exp.win$Win4 / data$Win4
    ratio.win4S <- exp.win$Win4s / data$Win4S
    ratio.win4zz <- ratio.win4S / ratio.win4

    return(data.frame(
        Date = data$Date,
        data$ZZ,
        ratio.win2S,
        ratio.win3,
        ratio.win3S,
        ratio.win3zz,
        ratio.win4,
        ratio.win4S,
        ratio.win4zz
    ))
}

#' given a vector representing a row of the data.frame returned by
#' @link read.data return a vector of exponents for the model.
#' correctDrawn modifies the exponents for the data set (e.g. 3 if
#' used for win3, 4 for win4, etc.)
#'
#' @return exponents for every lotto number used in the model for the
#' general analysis
getExponents <- function(row, correctDrawn) {
    exponent <- rep((6 - correctDrawn) / 43, 49)
    exponent[as.integer(row[2])] <- correctDrawn / 6
    exponent[as.integer(row[3])] <- correctDrawn / 6
    exponent[as.integer(row[4])] <- correctDrawn / 6
    exponent[as.integer(row[5])] <- correctDrawn / 6
    exponent[as.integer(row[6])] <- correctDrawn / 6
    exponent[as.integer(row[7])] <- correctDrawn / 6
    return(exponent)
}