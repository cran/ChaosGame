\name{plot_word}
\alias{plot_word}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the 2D fractal containing the chosen word 
}
\description{
The function allows to enter a word of at least two letters based on which an Iterated Function System with Probabilities (IFSP) is constructed. This IFSP is then used to generate a two-dimensional fractal containing the chosen word infinitely often, which is then plotted (and optionally probability-integral-transformed).
}
\usage{
plot_word(word = "copula", R = 20, phi = 0, copula = FALSE, 
          portion = 0.2, shift = 1.2, orbit = 3000)
}

\arguments{
  \item{word}{
Word which the fractal should contain infinitely often.
}
  \item{R}{
Number of runs of the chaos game.  
}
  \item{phi}{
Angle of the rotation. 
}
  \item{copula}{
If \code{copula = TRUE} the sample is (approximately) probability-integral-transformed. 
}
  \item{portion}{
Portion based on which the empirical distribution functions are calculated, if \code{copula = TRUE}.
}
  \item{shift}{
Distance between letters.  
}
  \item{orbit}{
Number of steps in each run of the chaos game.  
}
}

\author{
Manuela Schreyer \email{manuelalarissa.schreyer@sbg.ac.at},\cr 
Wolfgang Trutschnig \email{Wolfgang.Trutschnig@sbg.ac.at}
}
   
\examples{
# function with word as input, constructs the IFSP and runs the chaos game:
# for nice results use, for example, R = 20 and orbit = 3000
A <- plot_word(word = "copula", R = 75, orbit = 300)

# plot without histograms of the marginal distributions
plot(A, pch =19, col = 4, cex = 0.1)


# further examples:

# with rotation
# A <- plot_word(word = "copula", R = 100, orbit = 300, phi = pi/8)
# A <- plot_word(word = "fractel", R = 100, orbit = 300, phi = pi/6)

# (approximately) probability-integral-transformed
# A <- plot_word(word = "copula", R = 100, orbit = 300, phi = pi/8, copula = TRUE)
# A <- plot_word(word = "fractel", R = 100, orbit = 300, phi = pi/6, copula = TRUE)
}