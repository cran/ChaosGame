\name{ChaosGame-package}
\alias{ChaosGame-package}
\alias{ChaosGame}
\docType{package}
\title{
Chaos game generation of fractal words
}
\description{
The main objective of the package is to enter a word of at least two letters based on which an Iterated Function System with Probabilities (IFSP) is constructed, and a two-dimensional fractal containing the chosen word infinitely often is generated via the Chaos Game. Additionally, the package allows to project the two-dimensional fractal on several three-dimensional surfaces and to transform the fractal into another fractal with uniform marginals.
}
\details{
\tabular{ll}{
Package: \tab ChaosGame\cr
Type: \tab Package\cr
Version: \tab 0.3\cr
Date: \tab 2017-07-18\cr
}

}
\author{
Manuela Schreyer \email{manuelalarissa.schreyer@sbg.ac.at},\cr 
Wolfgang Trutschnig \email{Wolfgang.Trutschnig@sbg.ac.at}
}
\keyword{ package }

\examples{
# function with word as input, runs the chaos game and
# projects the result on the Enneper Minimal Surface:

# for nice results use, for example, R = 20 and orbit = 3000
A <- plot_word3D(word = "copula", R = 50, orbit = 100, copula = FALSE, 
                 plot.surface = "EnneperMinimalSurface", histogram = FALSE)
            
# further examples:

# same example as before, now with histogram = TRUE
# A <- plot_word3D(word = "copula", R = 100, orbit = 300, copula = FALSE,
#                  plot.surface = "EnneperMinimalSurface")
# same example as before, now (approximately) probability-integral-transformed 
# (i.e. copula = TRUE)
# A <- plot_word3D(word = "copula", R = 100, orbit = 300, copula = TRUE,
#                  plot.surface = "EnneperMinimalSurface")


# projection of the fractal on a Catalan Surface
# A <- plot_word3D(word = "copula", R = 100, orbit = 300, copula = FALSE, 
#                  color.rgl.plot = "blue2green", plot.surface = "CatalanSurface")
# Catalan Surface (approximately) probability-integral-transformed (i.e. copula = TRUE)
# A <- plot_word3D(word = "copula", R = 100, orbit = 300, copula = TRUE,
#                  color.rgl.plot = "blue2green", plot.surface = "CatalanSurface")


# projection of the fractal on a Helix
# A <- plot_word3D(word = "copula", R = 100, orbit = 300, copula = FALSE, 
#                  color.rgl.plot = "green2red", plot.surface = "Helix")
# Helix (approximately) probability-integral-transformed (i.e. copula = TRUE)
# A <- plot_word3D(word = "copula", R = 100, orbit = 300, copula = TRUE, 
#                  color.rgl.plot = "green2red", plot.surface = "Helix")


# projection of the fractal on a Torus
# A <- plot_word3D(word = "copula", R = 100, orbit = 300, copula = FALSE, 
#                  color.rgl.plot = "blue2yellow", plot.surface = "Torus")
# Torus (approximately) probability-integral-transformed (i.e. copula = TRUE)
# A <- plot_word3D(word = "copula", R = 100, orbit = 300, copula = TRUE, 
#                  color.rgl.plot = "blue2yellow", plot.surface = "Torus")


# projection of the fractal on a Sphere
# A <- plot_word3D(word = "copula", R = 100, orbit = 300, copula = FALSE, 
#                  color.rgl.plot = "ygobb", plot.surface = "Sphere")
# Sphere (approximately) probability-integral-transformed (i.e. copula = TRUE)
# A <- plot_word3D(word = "copula", R = 100, orbit = 300, copula = TRUE, 
#                  color.rgl.plot = "ygobb", plot.surface = "Sphere")
           
            
}
