% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_word.R
\name{plot_word}
\alias{plot_word}
\title{Plot the 2D fractal containing the chosen word}
\usage{
plot_word(
  word = "copula",
  R = 20,
  phi = 0,
  copula = FALSE,
  portion = 0.2,
  shift = 1.2,
  orbit = 3000,
  letter_type = 1
)
}
\arguments{
\item{word}{Word which the fractal should contain infinitely often.}

\item{R}{Number of runs of the chaos game.}

\item{phi}{Angle of the rotation.}

\item{copula}{logical. If TRUE the sample is (approximately) probability-integral-transformed.}

\item{portion}{Portion based on which the empirical distribution functions are calculated, if \code{copula = TRUE}.}

\item{shift}{Distance between letters.}

\item{orbit}{Number of steps in each run of the chaos game.}

\item{letter_type}{integer, which indicates the type of the letters. Options are 1 (default) or 2 (round letters).}
}
\description{
The function allows to enter a word of at least two letters based on which an Iterated Function System with Probabilities (IFSP) is constructed. This IFSP is then used to generate a two-dimensional fractal containing the chosen word infinitely often,
which is then plotted (and optionally probability-integral-transformed).
}
\examples{
#Function with word as input, constructs the IFSP and runs the chaos game:

# for nice results use, for example, R = 20 and orbit = 3000
A <- plot_word(word = "copula", R = 50, orbit = 100)
#plot without histograms of the marginal distributions
plot(A, pch =19, col = 4, cex = 0.1)

# further examples:
# with round letters
# A <- plot_word(word = "copula", R = 100, orbit = 300, letter_type = 2)
# with rotation
# A <- plot_word(word = "copula", R = 100, orbit = 300, phi = pi/8)
# A <- plot_word(word = "fractal", R = 100, orbit = 300, phi = pi/6)

# (approximately) probability-integral-transformed
# A <- plot_word(word = "copula", R = 100, orbit = 300, phi = pi/8, copula = TRUE)
# A <- plot_word(word = "fractal", R = 100, orbit = 300, phi = pi/6, copula = TRUE)
}
