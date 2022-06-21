#' Plot the 2D fractal containing the chosen word
#'
#' @description The function allows to enter a word of at least two letters based on which an Iterated Function System with Probabilities (IFSP) is constructed. This IFSP is then used to generate a two-dimensional fractal containing the chosen word infinitely often,
#' which is then plotted (and optionally probability-integral-transformed).
#'
#'
#' @param word Word which the fractal should contain infinitely often.
#' @param R Number of runs of the chaos game.
#' @param phi Angle of the rotation.
#' @param copula logical. If TRUE the sample is (approximately) probability-integral-transformed.
#' @param portion Portion based on which the empirical distribution functions are calculated, if \code{copula = TRUE}.
#' @param shift Distance between letters.
#' @param orbit Number of steps in each run of the chaos game.
#' @param letter_type integer, which indicates the type of the letters. Options are 1 (default) or 2 (round letters).
#'
#' @examples
#' #Function with word as input, constructs the IFSP and runs the chaos game:
#'
#' # for nice results use, for example, R = 20 and orbit = 3000
#' A <- plot_word(word = "copula", R = 50, orbit = 100)
#' #plot without histograms of the marginal distributions
#' plot(A, pch =19, col = 4, cex = 0.1)
#'
#' # further examples:
#' # with round letters
#' # A <- plot_word(word = "copula", R = 100, orbit = 300, letter_type = 2)
#' # with rotation
#' # A <- plot_word(word = "copula", R = 100, orbit = 300, phi = pi/8)
#' # A <- plot_word(word = "fractal", R = 100, orbit = 300, phi = pi/6)
#'
#' # (approximately) probability-integral-transformed
#' # A <- plot_word(word = "copula", R = 100, orbit = 300, phi = pi/8, copula = TRUE)
#' # A <- plot_word(word = "fractal", R = 100, orbit = 300, phi = pi/6, copula = TRUE)

plot_word <-
function(word = "copula", R = 20, phi = 0, copula = FALSE, portion = 0.2, shift = 1.2, orbit = 3000, letter_type = 1){


  A <- .chaos_game_word(word = word, R = R, shift = shift, orbit = orbit, letter_type = letter_type)

  #A dataframe with columns x and y, output of chaos_game_word
  farbe<-rainbow(100,start=.40,end=.17)

  x<-(cos(phi)*A$x-sin(phi)*A$y)
  y<-(sin(phi)*A$x+cos(phi)*A$y)
  B1<-data.frame(x=(x-min(x))/(max(x)-min(x)), y=(y-min(y))/(max(y)-min(y)))

  if(copula==TRUE){
    perc<-round(nrow(B1)*portion)
    trafox<-ecdf(B1$x[1:perc])
    trafoy<-ecdf(B1$y[1:perc])
    B1<-data.frame(x=trafox(B1$x),y=trafoy(B1$y))
  }

  hx <- hist(B1$x, plot=FALSE)
  hy <- hist(B1$y, plot=FALSE)

  Data.x <- data.frame(x = hx$breaks[1:(length(hx$breaks)-1)], frequency = hx$density )
  p1 <- ggplot(Data.x, aes(x = x, y = frequency)) + geom_bar(stat = "identity", fill = "lightblue",colour="gray20")
  p1 <- p1 + theme_bw()
  p1 <- p1 + scale_y_continuous("frequency")

  p2 <- ggplot(data=B1,aes(x=x,y=y))
  p2 <- p2 + stat_bin2d(binwidth = c(0.0025,0.0025),colour=NA,size=0.01)
  p2 <- p2 + theme_bw()
  p2 <- p2 + scale_fill_gradientn(colours=farbe,name="count", guide = "none")

  p3 <- ggplot(B1, aes(x=x,y=y))
  p3 <- p3 + geom_point(colour="gray30",size=0.05)
  p3 <- p3 + theme_bw()

  Data.y <- data.frame(y = hy$breaks[1:(length(hy$breaks)-1)], frequency = hy$density )
  p4 <- ggplot(Data.y, aes(x = y, y = frequency)) + geom_bar(stat = "identity", fill = "lightblue",colour="gray20")
  p4 <- p4 + theme_bw()
  p4 <- p4 + coord_flip()
  p4 <- p4 + scale_y_continuous("frequency")

  grid.arrange(p1, p2 , p3, p4, ncol=2, nrow=2, widths=c(4, 2), heights=c(2, 4))
  plots<-list(p1,p2,p3,p4)

  res<-list(A=B1,plots=plots)

  return(B1)
}
