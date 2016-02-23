\name{plot_hist3D}
\alias{plot_hist3D}
\title{
Plot the 3D fractal containing the chosen word 
}
\description{
The function allows to enter a word of at least two letters based on which an Iterated Function System with Probabilities (IFSP) is constructed. This IFSP is then used to generate a two-dimensional fractal containing the chosen word infinitely often, which is then projected onto several three-dimensional surfaces. Optionally, the projection is transformed into another fractal with uniform marginals.
}
\usage{
plot_hist3D(word = "copula", R = 20, plot.rgl = TRUE, 
           copula = TRUE, portion = 0.2, color.rgl.plot = "magenta2green",
           plot.surface = "Sphere", histogram = TRUE, shift = 1.2, 
           orbit = 3000, cex.label = 0.7, size.lines = 0.1)
}
\arguments{
  \item{word}{
Word which the fractal should contain infinitely often.
}
  \item{R}{
Number of runs of the chaos game. 
}
  \item{plot.rgl}{
If \code{plot.rgl = TRUE} a rgl-plot is generated. Otherwise a scatter-plot with plot3D is produced.
}
  \item{copula}{
If \code{copula = TRUE} the sample is (approximately) probability-integral-transformed. 
}
  \item{portion}{
Portion based on which the empirical distribution functions are calculated, if \code{copula = TRUE}.
}
  \item{color.rgl.plot}{
Plotting color/color-range for the rgl-plot. One can choose between \code{"gray"}, \code{"blue2green"}, \code{"green2red"}, \code{"blue2yellow"}, \code{"ygobb"} and \code{"magenta2green"}. 
}
  \item{plot.surface}{
Three-dimensional surface on which the two-dimensional fractal is projected. Options are \code{"Sphere"}, \code{"Helix"}, \code{"Torus"}, \code{"EnneperMinimalSurface"} and \code{"CatalanSurface"}. 
}
  \item{histogram}{
If \code{TRUE}, two-dimensional and one-dimensional marginal histograms are plotted.
}
  \item{shift}{
Distance between letters. 
}
  \item{orbit}{
Number of steps in each run of the chaos game.  
}
  \item{cex.label}{
Font size, for exporting as a pdf or png file (see examples).
}
  \item{size.lines}{
Line width, for exporting as a pdf or png file (see examples).  
}
}
\author{
Manuela Schreyer \email{manuelalarissa.schreyer@sbg.ac.at},\cr 
Wolfgang Trutschnig \email{Wolfgang.Trutschnig@sbg.ac.at} 
}
\examples{
# function with a word as input, runs the chaos game, calculates the
# copula transformation and projects the result on a sphere:

# for nice results use, for example, R = 20 and orbit = 3000
A <- plot_hist3D(word = "copula", copula = FALSE, R = 75, orbit = 300)


# further examples:

# projection of the fractal on the Enneper Minimal Surface:
# A <- plot_hist3D(word = "copula", R = 75, orbit = 300, copula = FALSE, 
#                  plot.surface = "EnneperMinimalSurface", histogram = FALSE)
# same example as before, now with histogram = TRUE
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = FALSE, 
#                  plot.surface = "EnneperMinimalSurface")
# same example as before (approximately) probability-integral-transformed 
# (i.e. copula = TRUE)
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = TRUE, 
#                  plot.surface = "EnneperMinimalSurface")


# projection of the fractal on a Catalan Surface
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = FALSE, 
#                  color.rgl.plot = "blue2green", plot.surface = "CatalanSurface")
# Catalan Surface (approximately) probability-integral-transformed 
# (i.e. copula = TRUE)
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = TRUE, 
#                  color.rgl.plot = "blue2green", plot.surface = "CatalanSurface")


# projection of the fractal on a Helix
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = FALSE, 
#                  color.rgl.plot = "green2red", plot.surface = "Helix")
# Helix (approximately) probability-integral-transformed (i.e. copula = TRUE)
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = TRUE, 
#                  color.rgl.plot = "green2red", plot.surface = "Helix")


# projection of the fractal on a Torus
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = FALSE, 
#                  color.rgl.plot = "blue2yellow", plot.surface = "Torus")
# Torus (approximately) probability-integral-transformed (i.e. copula = TRUE)
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = TRUE, 
#                  color.rgl.plot = "blue2yellow", plot.surface = "Torus")


# projection of the fractal on a Sphere
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = FALSE, 
#                  color.rgl.plot = "ygobb", plot.surface = "Sphere")
# Sphere (approximately) probability-integral-transformed (i.e. copula = TRUE)
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = TRUE, 
#                  color.rgl.plot = "ygobb", plot.surface = "Sphere")



# scatter-plot with plot3D (i.e. plot.rgl = FALSE) 
# A <- plot_hist3D(word = "copula", R = 10, orbit = 300, copula = FALSE, 
#                  plot.surface = "Sphere", plot.rgl = FALSE)


# scatter-plot with plot3D (i.e. plot.rgl = FALSE) for exporting as a pdf file
# setwd("working_directory")
# pdf(file = "Sphere.pdf", width = 30, height = 25)
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = FALSE, plot.rgl = FALSE, 
#                  plot.surface = "Sphere", cex.label = 1.8, size.lines = 0.001)
# dev.off()


# scatter-plot with plot3D (i.e. plot.rgl = FALSE) for exporting as a png file
# setwd("working_directory")
# png(file = "Sphere.png", width = 5000, height = 4000)
# A <- plot_hist3D(word = "copula", R = 100, orbit = 300, copula = FALSE, plot.rgl = FALSE, 
#                  plot.surface = "Sphere", cex.label = 5, size.lines = 2)
# dev.off()
}