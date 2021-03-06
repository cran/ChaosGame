.chaos_game_word <-
function(word="fractal",shift=1.2,R=10,orbit=3000, letter_type = letter_type){
  #Iconstructs IFS and runs Chaos Game

  II<-.construct_IFS(word=word, shift=shift, letter_type = letter_type)
  IFS<-II$IFS
  trans<-II$trans
  end<-max(II$trans)+1

  #choose start point IN attractor
  start<-c(0,0)
  for(k in 1:100){
    start<-IFS[[1]](x=start[1],y=start[2])
  }

  x<-start[1]; y<-start[2]
  nr<-1:length(IFS)
  steps<-orbit
  datafr <- rbind(matrix(NA,nrow=(steps),ncol=R))

  chaos_game_function <- function(vec){

    indices<-sample(nr,size=steps,replace=TRUE,prob=II$vol)
    vec1<-c()
    vec2<-c()
    for(i in 1:steps){
      e<-c(1/end,1)*(IFS[[indices[i]]](x,y)+c(trans[indices[i]],0))
      vec1<-c(vec1,e[1])
      vec2<-c(vec2,e[2])
      x<-e[1];y<-e[2]
    }
    r <- c(vec1,vec2)
    return(r)
  }

  gg<-apply(datafr, 2, chaos_game_function)
  A <- data.frame(x=c(gg[1:steps,]),y=c(gg[((steps+1):(2*steps)),]))

  return(A)
}
.green2magenta <- function(n){
  ramp.col(col=c("chartreuse","green","darkolivegreen","magenta","deeppink4","darkmagenta"), n = n)
}
.colorfunction <-
function(N, col="magenta2green"){
  if(col=="gray"){r <- gray(seq(0,(1.2*N))/(1.2*N))[1:N]}
  if(col=="blue2green"){r <- sapply("blue2green", do.call, list(N)) }
  if(col=="green2red"){r <- sapply("green2red", do.call, list(N)) }
  if(col=="blue2yellow"){r <- sapply("blue2yellow", do.call, list(N)) }
  if(col=="magenta2green"){r <- sapply("magenta2green", do.call, list(N)) }
  if(col=="ygobb"){r <- sapply("ygobb", do.call, list(N)) }
  if(col=="green2magenta"){r <- sapply("ygobb", do.call, list(N))
    r[,1] <- .green2magenta(N) }
  return(r)
}


.m <- function(type = "m"){
  m <- matrix(c(cos(24*pi/180),sin(24*pi/180),-sin(24*pi/180),cos(24*pi/180)),byrow=TRUE,nrow=2)
  ma <- matrix(c(cos(pi/5),-sin(pi/5),sin(pi/5),cos(pi/5)),byrow=TRUE,nrow=2)
  maa <- matrix(c(cos(pi/5),sin(pi/5),-sin(pi/5),cos(pi/5)),byrow=TRUE,nrow=2)
  m1<- matrix(c(-1,0,0,1),byrow=TRUE,nrow=2)
  m2<- matrix(c(1,0,0,-1),byrow=TRUE,nrow=2)
  m4<- matrix(c(0,1,-1,0),byrow=TRUE,nrow=2)
  m5<- matrix(c(0,-1,1,0),byrow=TRUE,nrow=2)
  m7 <- matrix(c(cos(51*pi/180),-sin(51*pi/180),sin(51*pi/180),cos(51*pi/180)),byrow=TRUE,nrow=2)
  m7a <- matrix(c(cos(51*pi/180),sin(51*pi/180),-sin(51*pi/180),cos(51*pi/180)),byrow=TRUE,nrow=2)
  m7b <- matrix(c(cos(48*pi/180),-sin(48*pi/180),sin(48*pi/180),cos(48*pi/180)),byrow=TRUE,nrow=2)
  m8 <- matrix(c(cos(pi/4),sin(pi/4),-sin(pi/4),cos(pi/4)),byrow=TRUE,nrow=2)
  m8a <- matrix(c(cos(pi/4),-sin(pi/4),sin(pi/4),cos(pi/4)),byrow=TRUE,nrow=2)
  m9 <- matrix(c(cos(38.6*pi/180),-sin(38.6*pi/180),sin(38.6*pi/180),cos(38.6*pi/180)),byrow=TRUE,nrow=2)
  m9a <- matrix(c(cos(38.6*pi/180),sin(38.6*pi/180),-sin(38.6*pi/180),cos(38.6*pi/180)),byrow=TRUE,nrow=2)
  m10 <- matrix(c(cos(80.5*pi/180),sin(80.5*pi/180),-sin(80.5*pi/180),cos(80.5*pi/180)),byrow=TRUE,nrow=2)
  m10a <- matrix(c(cos(80.5*pi/180),-sin(80.5*pi/180),sin(80.5*pi/180),cos(80.5*pi/180)),byrow=TRUE,nrow=2)
  m11 <- matrix(c(cos(21.8*pi/180),sin(21.8*pi/180),-sin(21.8*pi/180),cos(21.8*pi/180)),byrow=TRUE,nrow=2)
  m11a <- matrix(c(cos(21.8*pi/180),-sin(21.8*pi/180),sin(21.8*pi/180),cos(21.8*pi/180)),byrow=TRUE,nrow=2)
  matrices <- list(m = m, ma = ma, maa = maa, m1 = m1, m2 = m2, m4 = m4, m5 = m5,
                   m7 = m7, m7a = m7a, m7b = m7b, m8 = m8, m8a = m8a, m9 = m9,
                   m9a = m9a, m10 = m10, m10a = m10a, m11 = m11, m11a = m11a)
  return(matrices[[type]])
}

.construct_IFS <-
function(word="fractal", shift=1.25, letter_type = 1){
  #word...word being the attractor
  #shift-1...distance between letters
  w<-toupper(word)
  ws<-strsplit(w,split="")
  ws<-ws[[1]]
  w<-ws


  if(letter_type == 2)
    IFSg<- .roundLetterIFS[w[1]][[1]]
  else
    IFSg<-.LetterIFS[w[1]][[1]]


  trans<-rep(0,length(IFSg))

  for(i in 2:length(w)){
    if(letter_type == 2)
      IFS<- .roundLetterIFS[w[i]][[1]]
    else
      IFS<-.LetterIFS[w[i]][[1]]

    trans<-c(trans,rep((i-1)*shift,length(IFS)))
    IFSg<-c(IFSg,IFS)
  }
  span<-max(trans)+1

  vol<-c()
  for(i in 1:length(IFSg)){
    v1<-(IFSg[[i]](x=1,y=0)-IFSg[[i]](x=0,y=0))
    v2<-(IFSg[[i]](x=0,y=1)-IFSg[[i]](x=0,y=0))
    vol<-c(vol, abs(det(cbind(v1,v2))))
  }

  Res<-list(IFS=IFSg,trans=trans,span=span,vol=vol)
  return(Res)
}






.LetterIFS <-
structure(
  list(point = structure(list(f1 = function(x, y){
  c(1/3 * x + 1/3, 1/8 * y)
}), .Names = c("f1")), hyphen = structure(list(f1 = function(x, y){
  c(x, 1/5 * y + 2/5)
}), .Names = c("f1")), A = structure(list(f1 = function(x, y){
  c(x, 1/4 * y + 3/4)
}, f2 = function(x, y){
  c(1/2 * x + 1/4, 1/4 * y + 1/4)
}, f3 = function(x, y){
  c(-1/4 * y + 1/4, 3/4 * x)
}, f4 = function(x, y){
  (c(-1/4 * y + 1/4, 3/4 * x) + c(3/4, 0))
}), .Names = c("f1", "f2", "f3", "f4")), B = structure(list(f1 = function(x, y){
  c(3/4 * x, 1/4 * y)
}, f2 = function(x, y){
  c(-1/4 * y + 1/4, 1/2 * x + 1/4)
}, f3 = function(x, y){
  c(3/4 * x, 1/4 * y) + c(0, 3/4)
}, f4 = function(x, y){
  c(-1/4 * y + 1, 5/16 * x + 1/8)
}, f5 = function(x, y){
  c(-1/4 * y + 1, 5/16 * x + 1/2 + 1/16)
}, f6 = function(x, y){
  c(1/2 * x + 1/4, 1/5 * y + 2/5)
}), .Names = c("f1", "f2", "f3", "f4", "f5", "f6")), C = structure(list(
    f1 = function(x, y){
      c(x, 1/4 * y)
    }, f2 = function(x, y){
      c(-1/4 * y + 1/4, 1/2 * x + 1/4)
    }, f3 = function(x, y){
      c(x, 1/4 * y) + c(0, 3/4)
    }), .Names = c("f1", "f2", "f3")), D = structure(list(f1 = function (x, y){
  c(3/4 * x, 1/4 * y)
}, f2 = function (x, y){
  c(-1/4 * y + 1/4, 1/2 * x + 1/4)
}, f3 = function(x, y){
  c(3/4 * x, 1/4 * y) + c(0, 3/4)
}, f4 = function(x, y){
  c(-1/4 * y + 1, 3/4 * x + 1/8)
}), .Names = c("f1", "f2", "f3", "f4")), E = structure(list(f1 = function(x, y){
  c(-1/4 * y + 1/4, 1/2 * x + 1/4)
}, f2 = function(x, y){
  c(x, 1/4 * y)
}, f3 = function(x, y){
  c(x, 1/4 * y + 3/4)
}, f4 = function(x, y){
  c(3/4 * x + 1/4, 1/5 * y + 2/5)
}), .Names = c("f1", "f2", "f3", "f4")), F = structure(list(f1 = function(x, y){
  c(x, 1/4 * y + 3/4)
}, f2 = function(x, y){
  c(1/2 * x + 1/4, 1/4 * y + 1/4)
}, f3 = function(x, y){
  c(-1/4 * y + 1/4, 3/4 * x)
}), .Names = c("f1", "f2", "f3")), G = structure(list(f1 = function(x, y){
  c(3/4 * x, 1/4 * y)
}, f2 = function(x, y){
  c(-1/4 * y + 1/4, 1/2 * x + 1/4)
}, f3 = function(x, y)
{
  c(x, 1/4 * y) + c(0, 3/4)
}, f4 = function(x, y){
  c(-1/4 * y + 1, 1/4 * x + 1/8)
}, f5 = function(x, y){
  c(1/2 * x + 1/2, 1/4 * y + 3/8)
}), .Names = c("f1", "f2", "f3", "f4", "f5")), H = structure(list(
    f1 = function(x, y){
      c(-1/4 * y + 1/4, x)
    }, f2 = function(x, y){
      c(1/2 * x + 1/4, 1/4 * y + 3/8)
    }, f3 = function(x, y){
      c(-1/4 * y + 1/4, x) + c(3/4, 0)
    }), .Names = c("f1", "f2", "f3")), I = structure(list(f1 = function(x, y){
  c(x, 1/4 * y)
}, f2 = function(x, y){
  c(-1/4 * y + 1/4, 1/2 * x + 1/4) + c(3/8, 0)
}, f3 = function(x, y){
  c(x, 1/4 * y) + c(0, 3/4)
}), .Names = c("f1", "f2", "f3")), J = structure(list(f1 = function(x, y){
  c(x, 1/4 * y + 3/4)
}, f2 = function(x, y){
  c(-1/4 * y + 1/4, 1/2 * x + 1/4) + c(3/4, 0)
}, f3 = function(x, y){
  c(3/4 * x, 1/4 * y) + c(1/4, 0)
}, f4 = function(x, y){
  c(-1/4 * y + 1, 1/4 * x + 1/8) + c(-3/4, 0)
}), .Names = c("f1", "f2", "f3", "f4")), K = structure(list(f1 = function(x, y){
  c(-(1 - 1/2 - 1/(4 * sqrt(2))) * y + (1 - 1/2 - 1/(4 * sqrt(2))), x)
}, f2 = function(x, y){
  c(sqrt(2)/2 * (sqrt(2) * (1/2) * x - 1/4 * y) + 1/2 + sqrt(2)/8, sqrt(2)/2 * (sqrt(2) * (1/2) * x + 1/4 * y) + 1/2) - c(1/(4 * sqrt(2)), 1/(4 * sqrt(2)))
}, f3 = function(x, y){
  c(sqrt(2)/2 * (sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1/2 + sqrt(2)/8, sqrt(2)/2 * (-sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1/2) - c(1/(4 * sqrt(2)), 1/(4 * sqrt(2)))
}), .Names = c("f1", "f2", "f3")), L = structure(list(f1 = function(x, y){
  c(-1/4 * y + 1/4, x)
}, f2 = function(x, y){
  c(3/4 * x + 1/4, 1/4 * y)
}), .Names = c("f1", "f2")), M = structure(list(f1 = function(x, y){
  c(-1/4 * y + 1/4, x)
}, f2 = function(x, y){
  c(-(sqrt(2)/2 * (sqrt(2) * (1/3) * x + 1/6 * y) + 1/2 - 1/(6 * sqrt(2))), sqrt(2)/2 * (sqrt(2) * (1/3) * x - 1/6 * y) + 1/2 + sqrt(2)/8 - 1/(6 * sqrt(2))) + c(1, 0)
}, f3 = function(x, y){
  c(-(sqrt(2)/2 * (-sqrt(2) * (1/3 - 1/(6 * sqrt(2))) *  x + 1/6 * y) + 1/2 - 1/(6 * sqrt(2))), sqrt(2)/2 * (sqrt(2) * (1/3 - 1/(6 * sqrt(2))) * x + 1/6 * y) + 1/2 + sqrt(2)/8 - 1/(6 * sqrt(2))) + c(1, 0)
}, f4 = function(x, y){
  c(-1/4 * y + 1/4, x) + c(3/4, 0)
}), .Names = c("f1", "f2", "f3", "f4")), N = structure(list(f1 = function(x, y){
  c(-1/4 * y + 1/4, 0.57 * x)
}, f2 = function(x, y){
  c(sqrt(2)/2 * (sqrt(2) * (1 - 1/(4 * sqrt(2))) * x + 1/4 * y), sqrt(2)/2 * (-sqrt(2) * (1 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1 - 1/(4 * sqrt(2)))
}, f3 = function(x, y){
  c(-1/4 * y + 1/4, 0.57 * x) + c(3/4, 1 - 0.57)
}), .Names = c("f1", "f2", "f3")), O = structure(list(f1 = function(x, y){
  c(x, 1/4 * y)
}, f2 = function(x, y){
  c(-1/4 * y + 1/4, 1/2 * x + 1/4)
}, f3 = function(x, y){
  c(x, 1/4 * y) + c(0, 3/4)
}, f4 = function(x, y){
  c(-1/4 * y + 1/4, 1/2 * x + 1/4) + c(3/4, 0)
}), .Names = c("f1", "f2", "f3", "f4")), P = structure(list(f1 = function(x, y){
  c(-1/4 * y + 1/4, 3/4 * x)
}, f2 = function(x, y){
  c(1/2 * x + 1/4, 1/5 * y + 2/5)
}, f3 = function(x, y){
  c(-1/4 * y + 1, 5/16 * x + 1/2 + 1/16)
}, f4 = function(x, y){
  c(3/4 * x, 1/4 * y) + c(0, 3/4)
}), .Names = c("f1", "f2", "f3", "f4")), Q = structure(list(f1 = function(x, y){
  c(sqrt(2)/2 * (sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1/2 + sqrt(2)/8, sqrt(2)/2 * (-sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1/2) - c(1/(4 * sqrt(2)), 1/(4 * sqrt(2)))
}, f2 = function(x, y){
  c(-1/4 * y + 1/4, 1/2 * x + 1/4)
}, f3 = function(x, y){
  c(x, 1/4 * y + 3/4)
}, f4 = function(x, y){
  c((0.43 + 1/8) * x, 1/4 * y)
}, f5 = function(x, y){
  c(-1/4 * y + 1, (0.305 + 0.05) * x + 0.445 - 0.05)
}), .Names = c("f1", "f2", "f3", "f4", "f5")), R = structure(list(
    f1 = function(x, y){
      c(sqrt(2)/2 * (sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1/2 + sqrt(2)/8, sqrt(2)/2 * (-sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1/2) - c(1/(4 * sqrt(2)),  1/(4 * sqrt(2)))
    }, f2 = function(x, y){
      c(-1/4 * y + 1/4, 3/4 * x)
    }, f3 = function(x, y){
      c((3/4 + 1/8) * x, 1/4 * y + 3/4)
    }, f4 = function(x, y){
      c(-1/4 * y + 1, 0.305 * x + 0.445)
    }, f5 = function(x, y){
      c(1/4 * x + 1/4, 0.2 * y + 0.3)
    }), .Names = c("f1", "f2", "f3", "f4", "f5")), S = structure(list(
    f1 = function(x, y){
      c(3/4 * x, 1/4 * y)
    }, f2 = function(x, y){
      c(-1/4 * y + 1, (1/2 - 1/8) * x)
    }, f3 = function(x, y){
      c(-1/4 * y + 1/4, (1/2 - 1/8) * x + 1/2 - 1/8)
    }, f4 = function(x, y){
      c(3/4 * x + 1/4, 1/4 * y + 1/2 - 1/8)
    }, f5 = function(x, y){
      c(x, 1/4 * y + 3/4)
    }), .Names = c("f1", "f2", "f3", "f4", "f5")), T = structure(list(
    f1 = function(x, y){
      c(x, 1/4 * y + 3/4)
    }, f2 = function(x, y){
      c(-1/3 * y + 1/2 + 1/8, 3/4 * x)
    }), .Names = c("f1", "f2")), U = structure(list(f1 = function(x, y){
  c(-1/4 * y + 1/4, 3/4 * x + 1/4)
}, f2 = function(x, y)
{
  c(-1/4 * y + 1/4, 3/4 * x + 1/4) + c(3/4, 0)
}, f3 = function(x, y){
  c(x, 1/4 * y)
}), .Names = c("f1", "f2", "f3")), V = structure(list(f1 = function(x, y){
  c(-1/2 * 1.064 * x - sqrt(3)/2 * 0.2 * y + 0.61, sqrt(3)/2 * 1.064 * x - 1/2 * 0.2 * y + 0.08)
}, f2 = function(x, y){
  c(1/2 * 1.042 * x - sqrt(3)/2 * 0.2 * y + 0.57, sqrt(3)/2 * 1.042 * x + 1/2 * 0.2 * y)
}), .Names = c("f1", "f2")), W = structure(list(f1 = function(x, y){
  (c(2/3 * 0.98, 0.95) * c(-1/2 * 1.1 * x - sqrt(3)/2 * 0.2 * y + 0.61, sqrt(3)/2 * 1.1 * x - 1/2 * 0.2 * y + 0.08)+c(0.079,0.02))*c(0.389/(0.389+0.07),1)
}, f2 = function(x, y){
  c(2/3, 1) * c(1/2 * 1.1 * 1/2 * x - sqrt(3)/2 * 0.2 * y + 0.57, sqrt(3)/2 * 1.1 * 1/2 * x + 1/2 * 0.2 * y)
}, f3 = function(x, y){
  c(-1, 1) * c(2/3, 0.95) * c(-1/2 * 1.1 * x - sqrt(3)/2 * 0.2 * y + 0.61, sqrt(3)/2 * 1.1 * x - 1/2 * 0.2 * y + 0.08) + c(1, 0.02)-c(0.079,0)
}, f4 = function(x, y){
  c(-1, 1) * c(2/3, 1) * c(1/2 * 1.1 * 1/2 * x - sqrt(3)/2 * 0.2 * y + 0.57, sqrt(3)/2 * 1.1 * 1/2 * x + 1/2 * 0.2 * y) + c(1, 0)
}), .Names = c("f1", "f2", "f3", "f4")), X = structure(list(f1 = function(x, y){
  c(sqrt(2)/2 * (sqrt(2) * (1 - 1/(4 * sqrt(2))) * x + 1/4 * y), sqrt(2)/2 * (-sqrt(2) * (1 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1 - 1/(4 * sqrt(2)))
}, f2 = function(x, y){
  c(-(sqrt(2)/2 * (-sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1/2 - 1/(4 * sqrt(2))), sqrt(2)/2 * (sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1/2 + sqrt(2)/8 - 1/(4 * sqrt(2))) + c(1, 0)
}, f3 = function(x, y){
  c(-(sqrt(2)/2 * (-sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1/2 - 1/(4 * sqrt(2))), sqrt(2)/2 * (sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1/2 + sqrt(2)/8 - 1/(4 * sqrt(2))) + c(1, 0) - c(1/2, 1/2)
}), .Names = c("f1", "f2", "f3")), Y = structure(list(f1 = function(x, y){
  c(-1/4 * y + 1/2 - 1/8 + 1/4, 0.375 * x)
}, f2 = function(x, y){
  c(-(sqrt(2)/2 * (sqrt(2) * (1/2) * x + 1/4 * y) + 1/2 - 1/(4 * sqrt(2))), sqrt(2)/2 * (sqrt(2) * (1/2) * x - 1/4 * y) + 1/2 + sqrt(2)/8 - 1/(4 * sqrt(2))) + c(1, 0)
}, f3 = function(x, y){
  c(-(sqrt(2)/2 * (-sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x +  1/4 * y) + 1/2 - 1/(4 * sqrt(2))), sqrt(2)/2 * (sqrt(2) * (1/2 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1/2 + sqrt(2)/8 - 1/(4 * sqrt(2))) + c(1, 0)
}), .Names = c("f1", "f2", "f3")), Z = structure(list(f1 = function(x, y){
  c(-0.57 * x + 1, -1/4 * y + 1/4)
}, f2 = function(x, y){
  c(-(sqrt(2)/2 * (-sqrt(2) * (1 - 1/(4 * sqrt(2))) * x + 1/4 * y) + 1 - 1/(4 * sqrt(2))) + 1, sqrt(2)/2 * (sqrt(2) * (1 - 1/(4 * sqrt(2))) * x + 1/4 * y))
}, f3 = function(x, y){
  c(-(0.57 * x + 1 - 0.57) + 1, -1/4 * y + 1/4 + 3/4)
}), .Names = c("f1", "f2", "f3"))), .Names = c(".","-","A", "B", "C",
"D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
"Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"))







.roundLetterIFS <-
  structure(list(point = structure(list(f1 = function(x, y){
      c(1/3 * x + 1/3, 1/8 * y)
    }), .Names = c("f1")), hyphen = structure(list(f1 = function(x, y){
      c(x, 1/5 * y + 2/5)
    }), .Names = c("f1")), A = structure(list(f1 = function(x,y){
      c(2/5,1) + .m("m11a") %*% c(y/6,((1-x)*1.07-y/15)-1.07)
    },f2 = function(x,y){
      c(2/5,1) + .m("m11") %*% c((1-y)/6,((x*1.02)/(4/3-y/3)-y/15+1/15)-1.07)
    },f3 = function(x,y){
      c(y/4*(1-((1-x)/2))+3/8+(1-x)/16,3/10+(1-x)/5)
    }), .Names = c("f1", "f2", "f3")), B = structure(list(  f1 = function(x,y){
      c(y/5,1-x)
    },f2 = function(x,y){
      c(1/5+x*2/5,y*3/20+17/20)
    },f3 = function(x,y){
      c(1/5+x*2/5,y*3/10+7/20)
    },f4 = function(x,y){
      c(1/5+x*2/5,y*3/20)
    },f5 = function(x,y){
      c((sqrt(1-((sqrt(1-(x)^2))^2))*1/5+x/5)+3/5,((sqrt(1-(x)^2)*1/8+y/8)+3/4)*5/4-1/4)
    },f6 = function(x,y){
      c(0,3/2)+ .m("m2") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))*1/5+x/5)+3/5,((sqrt(1-(x)^2)*1/8+(1-y)/8)+3/4)*5/4-1/4)
    },f7 = function(x,y){
      c(0,-1/2)+c((sqrt(1-((sqrt(1-(x)^2))^2))*1/5+x/5)+3/5,((sqrt(1-(x)^2)*1/8+y/8)+3/4)*5/4-1/4)
    },f8 = function(x,y){
      c(0,1)+ .m("m2") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))*1/5+x/5)+3/5,((sqrt(1-(x)^2)*1/8+(1-y)/8)+3/4)*5/4-1/4)
    }), .Names = c("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8")), C = structure(list(f1 = function(x,y){
      c(1,0)+ .m("m5") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))*3/10+x/5)+1/2,(sqrt(1-(x)^2)*3/10+y/5+4/10)*5/4-1/8)
    },f2 = function(x,y){
      c(0,1)+ .m("m2") %*% (c(1,0)+.m("m5") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))*3/10+x/5)+1/2,(sqrt(1-(x)^2)*3/10+(1-y)/5+4/10)*5/4-1/8))
    },f3 = function(x,y){
      c(1/2+x/2*(1+(y/5)*2)-(y/5),y/5)
    },f4 = function(x,y){
      c(1/2+x/2*(3-(y/5+4/5)*2)-(1-(y/5+4/5)),y/5+4/5)
    }), .Names = c("f1", "f2", "f3","f4")), D = structure(list(f1 =function(x,y){
      c((sqrt(1-((sqrt(1-(x)^2))^2))*3/10+x/5)*8/5+1/5,(sqrt(1-(x)^2)*3/10+y/5+4/10)+1/12)
    },f2 =function(x,y){
      c(0,1)+.m("m2") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))*3/10+x/5)*8/5+1/5,(sqrt(1-(x)^2)*3/10+(1-y)/5+4/10)+1/12)
    },f3 =function(x,y){
      c(y/5,1-x)
    },f4 =function(x,y){
      c(x/10+9/10,3/8+y/4*((1-(x[1]/10)*5))+(x/16))
    }), .Names = c("f1", "f2", "f3", "f4")), E = structure(list(f1 =function(x,y){
      c(y/5,1-x)
    },f2 =function(x,y){
      c(4/5*x+1/5,4/5+y/5)
    },f3 =function(x,y){
      c(2/5*x+1/5,2/5+y/5)
    },f4 =function(x,y){
      c(4/5*x+1/5,y/5)
    }), .Names = c("f1", "f2", "f3", "f4")), F = structure(list(f1 =function(x,y){
        c(y/5,1-x)
      },f2 =function(x,y){
        c(x*3/5+1/5,4/5+y/5)
      },f3 =function(x,y){
        c(x*2/5+1/5,2/5+y/5)
      },f4 =function(x,y){
        c(x/5+4/5,4/5+y/5)
      },f5 =function(x,y){
        c(x/5+3/5,y/5+2/5)
      }), .Names = c("f1", "f2", "f3", "f4", "f5")), G = structure(list(f1 =function(x,y){
        c(1,0)+.m("m5") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))*3/10+x/5)+1/2,(sqrt(1-(x)^2)*3/10+y/5+4/10)*5/4-1/8)
      },f2 =function(x,y){
        c(0,1)+.m("m2") %*% (c(1,0)+.m("m5") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))*3/10+x/5)+1/2,(sqrt(1-(x)^2)*3/10+(1-y)/5+4/10)*5/4-1/8))
      },f3 =function(x,y){
        c(1/2+x/2*(1+(y/5)*2)-(y/5),y/5)
      },f4 =function(x,y){
        c(1/2+x/2*(3-(y/5+4/5)*2)-(1-(y/5+4/5)),y/5+4/5)
      },f5 =function(x,y){
        c(y/5+4/5,31/80-x*7/40)
      },f6 =function(x,y){
        c(1/2+x/2,2/5+y/5)
      }), .Names = c("f1", "f2", "f3", "f4", "f5")), H = structure(list(f1 =function(x,y){
        c(y/5,1-x/2)
      },f2 =function(x,y){
        c(y/5+4/5,1-x/2)
      },f3 =function(x,y){
        c(x*3/5+1/5,y/5+2/5)
      },f4 =function(x,y){
        c(y/5,1/2-x/2)
      },f5 =function(x,y){
        c(y/5+4/5,1/2-x/2)
      }), .Names = c("f1", "f2", "f3", "f4", "f5")), I = structure(list(f1 =function(x,y){
          c(x*3/10+7/20,y/5+4/5)
        },f2 =function(x,y){
          c(x*3/10+7/20,y/5)
        },f3 =function(x,y){
          c(y/5+2/5,3/4-x/2)
        }), .Names = c("f1", "f2", "f3")), J = structure(list(f1 =function(x,y){
          c(x*4/5,4/5+y/5)
        },f2 =function(x,y){
          c(4/5+y/5,1-x*3/5)
        },f3 =function(x,y){
          c(0,9/10)+.m("m4") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))*1/5+x/5)+1/2,(sqrt(1-(x)^2)*3/10+y/5+4/10)*5/4-1/8)
        },f4 =function(x,y){
          c(0,1)+.m("m4") %*% (c(0,1)+.m("m4") %*% c((sqrt(1-((sqrt(1-(1-x)^2))^2))*3/10+(1-x)/5)*(1.2-(abs(4/5-((sqrt(1-(1-x)^2)*3/10+(1-y)/5+4/10)*5/4-1/8))))^1.1+1/2-1/10+abs(4/5-((sqrt(1-(1-x)^2)*3/10+(1-y)/5+4/10)*5/4-1/8))/2.2,(sqrt(1-(1-x)^2)/5+(1-y)/5+1/2)*5/4-1/8))
        }), .Names = c("f1", "f2", "f3", "f4")), K = structure(list(f1 =function(x,y){
          c(y/5,1-x)
        },f2 =function(x,y){
          c(1,0)+.m("m7") %*% c(y*3/20-3/20,(1-x)*3/4-(y*1/5-1/5))
        },f3 =function(x,y){
          c(0.17,0.5)+.m("m7a") %*% c((1-y)*3/20,x*3/4+(1-y)/5)
        }), .Names = c("f1", "f2", "f3")), L = structure(list(f1 =function(x,y){c(y/5,1-x)
            },f2 =function(x,y){c(x*4/5+1/5,y/5)
          }), .Names = c("f1", "f2")), M = structure(list(f1 =function(x,y){
            c(y/5+4/5,(1-x)*0.73*(1+y/3))
            },f2 =function(x,y){
              c(y/5,(1-x)*0.73*(4/3-y/3))
              },f3 =function(x,y){
              c(1/2,1/2) +.m("m8") %*% c((1-y)/6-1/6,(x*sqrt(2)/2.1)/(1+y)+y/6+1/40)
                }, f4 =function(x,y){
              c(1/2,1/2) +.m("m8a") %*% c(y/6,((1-x)*sqrt(2)/2.1)/(1+y)+y/6+1/40)
            }), .Names = c("f1", "f2", "f3", "f4")), N = structure(list(f1 =function(x,y){
              c(y/5+4/5,1/20+(1-x)*7/10*(1+y*5/14)+(1-y)/4)
            },f2 =function(x,y){
              c(y/5,(1-x)*7/10*(4/3-y/3))
            },f3 =function(x,y){
              c(1,0) +.m("m9") %*% c(y/6-1/6,(1-x)*5/4+(1-y)/8)
            }), .Names = c("f1", "f2", "f3")), O = structure(list(f1 =function(x,y){ #rundes o breit, Schrift vertikal(o6a)
              c((sqrt(1-((sqrt(1-(x)^2))^2))*3/10+x/5)+1/2,(sqrt(1-(x)^2)*3/10+y/5+4/10)*5/4-1/8)
            },f2 =function(x,y){
              c(0,1)+.m("m2") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))*3/10+x/5)+1/2,(sqrt(1-(x)^2)*3/10+(1-y)/5+4/10)*5/4-1/8)
            },f3 =function(x,y){
              c(1,0)+.m("m1") %*% c((sqrt(1-((sqrt(1-(1-x)^2))^2))*3/10+(1-x)/5)+1/2,(sqrt(1-(1-x)^2)*3/10+y/5+4/10)*5/4-1/8)
            },f4 =function(x,y){
              c(1,0)+.m("m1") %*% (c(0,1)+.m("m2") %*% c((sqrt(1-((sqrt(1-(1-x)^2))^2))*3/10+(1-x)/5)+1/2,(sqrt(1-(1-x)^2)*3/10+(1-y)/5+4/10)*5/4-1/8))
            }), .Names = c("f1", "f2", "f3","f4")), P = structure(list(f1 =function(x,y){
              c(y/5,1-x)
            },f2 =function(x,y){
              c(1/5+x*2/5,y/5+4/5)
            },f3 =function(x,y){
              c(1/5+x*2/5,2/5+y/5)
            },f4 =function(x,y){
              c((sqrt(1-((sqrt(1-(x)^2))^2))*1/5+x/5)+3/5,((sqrt(1-(x)^2)*1/8+y/8)+3/4)*5/3-2/3)
            },f5 =function(x,y){
              c(0,3/2)+.m("m2") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))*1/5+x/5)+3/5,((sqrt(1-(x)^2)*1/8+(1-y)/8)+3/4)*5/3-17/30)
            }), .Names = c("f1", "f2", "f3", "f4","f5")), Q = structure(list(f1 =function(x,y){
              c((sqrt(1-((sqrt(1-(x)^2))^2))*3/10+x/5)+1/2,(sqrt(1-(x)^2)*3/10+y/5+4/10)*5/4-1/8)
            },f2 =function(x,y){
              c(0,1)+.m("m2") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))/12+x/12)+5/6,(sqrt(1-(x)^2)/6+(1-y)/6+5/12)*5/4-1/8)
            },f3 =function(x,y){
              c(1,0)+.m("m1") %*% c((sqrt(1-((sqrt(1-(1-x)^2))^2))*3/10+(1-x)/5)+1/2,(sqrt(1-(1-x)^2)*3/10+y/5+4/10)*5/4-1/8)
            },f4 =function(x,y){
              c(1,0)+.m("m1") %*% (c(0,1)+.m("m2") %*% c((sqrt(1-((sqrt(1-(1-x)^2))^2))*0.47+(1-x)/5)+0.33,(sqrt(1-(1-x)^2)*3/10+(1-y)/5+4/10)*5/4-1/8))
            },f5 =function(x,y){
              c(0.79,0)+.m("ma") %*% c(y/6,1/2-x*1/2)
            }), .Names = c("f1", "f2", "f3", "f4", "f5")), R = structure(list(f1 =function(x,y){
              c(y*2/9,1-x)
            },f2 =function(x,y){
              c(1/4+x*7/20,y/5+4/5)
            },f3 =function(x,y){
              c(1/4+x*7/20,2/5+y/5)
            },f4 =function(x,y){
              c((sqrt(1-((sqrt(1-(x)^2))^2))*1/5+x/5)+3/5,((sqrt(1-(x)^2)*1/8+y/8)+3/4)*5/3-2/3)
            },f5 =function(x,y){
              c(0,3/2)+.m("m2") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))*1/5+x/5)+3/5,((sqrt(1-(x)^2)*1/8+(1-y)/8)+3/4)*5/3-17/30)
            },f6 =function(x,y){
              c(1,0)+.m("m7b") %*% c(y/5-1/5,(1-x)*4/7+(2/7-y*2/7))
            }), .Names = c("f1", "f2", "f3", "f4", "f5", "f6")), S = structure(list(f1 =function(x,y){
                c((2/5+(1/5-y/5)*2)*x+1/5+y/5,4/5+y/5)
              },f2 =function(x,y){
                c(1,0)+.m("m5") %*% c((sqrt(1-((sqrt(1-(x)^2))^2))/10+x/5)+7/10,(sqrt(1-(x)^2)/5+y/5+6/10))
              },f3 =function(x,y){
                c(0,1.4)+.m("m2") %*% (c(1,0)+.m("m5") %*% c((sqrt(1-((sqrt(1-(1-x)^2))^2))/10+(1-x)/5)+7/10,(sqrt(1-(1-x)^2)/5+y/5+6/10)))
              },f4 =function(x,y){
                c(2/5+x*2/5-(y/5),y/5+2/5)
              },f5 =function(x,y){
                c((sqrt(1-(1-x)^2)/5+y/5+6/10),(sqrt(1-((sqrt(1-(1-x)^2))^2))/10+(1-x)/5)+3/10)
              },f6 =function(x,y){
                c(0,0.6)+.m("m2") %*% c((sqrt(1-(1-x)^2)/5+(1-y)/5+6/10),(sqrt(1-((sqrt(1-(1-x)^2))^2))/10+(1-x)/5)+3/10)
              },f7 =function(x,y){
                c((2/5+y*2/5)*x+1/5-(y/5),y/5)
              }), .Names = c("f1", "f2", "f3", "f4", "f5","f6")), T = structure(list(f1 =function(x,y){
                  c(x/2,4/5+y/5)
                },f2 =function(x,y){
                  c(1/2+x/2,4/5+y/5)
                },f3 =function(x,y){
                  c(3/8+y/4,4/5-x*4/5)
                }), .Names = c("f1", "f2", "f3")), U = structure(list(f1 =function(x,y){
                    c(0,1)+.m("m4") %*% c((sqrt(1-(y)^2)*3/10+x/5+4/10)*5/4-1/8,(sqrt(1-((sqrt(1-(y)^2))^2))*3/10+y/5)+1/2)
                  },f2 =function(x,y){
                    c(1,0)+.m("m1") %*% (c(0,1)+.m("m4") %*% c((sqrt(1-(y)^2)*3/10+(1-x)/5+4/10)*5/4-1/8,(sqrt(1-((sqrt(1-(y)^2))^2))*3/10+y/5)+1/2))
                  },f3 =function(x,y){
                    c(y/5,1-(x*4/9*(1+(y/5)*2)))
                  },f4 =function(x,y){
                    c(y/5+4/5,1-(x*4/9*(3-(y/5+4/5)*2)))
                  }), .Names = c("f1", "f2", "f3","f4")), V = structure(list(f1 =function(x,y){
                      c(2/5,0) +.m("m11") %*% c((1-y)/6,x*1.07+(1-y)/15)
                    },f2 =function(x,y){
                      c(2/5,0) +.m("m11a") %*% c(y/6,0.02+((1-x)*1.07)/(1+y/3)+y/6)
                    }), .Names = c("f1", "f2")), W = structure(list(f1 = function(x, y){
                      (c(2/3 * 0.98, 0.95) * c(-1/2 * 1.1 * x - sqrt(3)/2 * 0.2 * y + 0.61, sqrt(3)/2 * 1.1 * x - 1/2 * 0.2 * y + 0.08)+c(0.079,0.02))*c(0.389/(0.389+0.07),1)
                    }, f2 = function(x, y){
                      c(2/3, 1) * c(1/2 * 1.1 * 1/2 * x - sqrt(3)/2 * 0.2 * y + 0.57, sqrt(3)/2 * 1.1 * 1/2 * x + 1/2 * 0.2 * y)
                    }, f3 = function(x, y){
                      c(-1, 1) * c(2/3, 0.95) * c(-1/2 * 1.1 * x - sqrt(3)/2 * 0.2 * y + 0.61, sqrt(3)/2 * 1.1 * x - 1/2 * 0.2 * y + 0.08) + c(1, 0.02)-c(0.079,0)
                    }, f4 = function(x, y){
                      c(-1, 1) * c(2/3, 1) * c(1/2 * 1.1 * 1/2 * x - sqrt(3)/2 * 0.2 * y + 0.57, sqrt(3)/2 * 1.1 * 1/2 * x + 1/2 * 0.2 * y) + c(1, 0)
                    }), .Names = c("f1", "f2", "f3", "f4")), X = structure(list(f1 =function(x,y){
                      c(3/5,11/24)+.m("m9a") %*% c((1-y)/6-1/6,x*0.64/(1.6-(1-y)*3/5)+y*1/9)
                    },f2 =function(x,y){
                      c(0,1/24)+.m("m9a") %*% c((1-y)/6,x*0.64/(1.6-y*3/5)+(1-y)*2/15)
                    },f3 =function(x,y){
                      c(1,0)+.m("m9") %*% c(y/6-1/6,(1-x)*1.275+(1-y)/9)
                    }), .Names = c("f1", "f2", "f3")), Y = structure(list(f1 =function(x,y){
                      c(2/5+y/5,3/5-x*3/5)
                    },f2 =function(x,y){
                      c(y/5+x*2/5,1-x*2/5)
                    },f3 =function(x,y){
                      c(1/2,7/10)+.m("m8") %*% c(sqrt(2)/10-y/10*sqrt(2),sqrt(2)*x*3/10*(4/3-y/3))
                    }), .Names = c("f1", "f2", "f3")), Z = structure(list(f1 =function(x,y){
                      c(x*13/20*(1+y*3/13),y/5+4/5)
                    },f2 =function(x,y){
                      c(1/5+x*13/20*(1+(3/13-y*3/13))+y*3/20,y/5)
                    },f3 =function(x,y){
                      .m("m9a") %*% c((1-y)/6,x*5/4+(1-y)/9)
                    }), .Names = c("f1", "f2", "f3"))), .Names = c(".","-","A", "B", "C",
                                                                   "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
                                                                   "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"))






.plot_ball <-
function(A, width = 0.75){  # width in (0,1) is the compression of theta

  r<-1 #Radius


  # Function for the Mercator projection and calculation of the Cartesian coordinates for (x,y) in (0,1):
  Cartesian_coordinates <- function(x,y,s){ # s correspondas to the compression of the Latitude

    x<-c(0.48*x,0.48*x+0.5)
    y<-c(0.98*y,0.98*y)

    # Transformation to (-pi,pi):
    x<-2*pi*x-pi
    y<-2*pi*y-pi

    # Mercator projection:
    phi <- asin(tanh(y)) # Latitude
    lambda <- x # Longitude

    theta <- pi/2 - phi # Spherical coordinate system
    theta <- width*theta/pi+s # compression of theta
    r<-1 #Radius

    # Cartesian coordinates:
    x<-r*sin(theta)*cos(lambda)
    y<-r*sin(theta)*sin(lambda)
    z<-r*cos(theta)

    list_of_coord <- data.frame(x=x,y=y,z=z)
    return(list_of_coord)
  }

  # Mercator projection and calculation of the Cartesian coordinates:
  list_of_coord_1 <- Cartesian_coordinates(A$x,A$y,0.4)
  list_of_coord_2 <- Cartesian_coordinates(A$x,A$y,1.2)
  list_of_coord_3 <- Cartesian_coordinates(A$x,A$y,2)

  surface_total <- rbind(list_of_coord_1, list_of_coord_2, list_of_coord_3)


  # rotate z-axis
  al <- pi/4
  n_vec <- c(0,0,1)
  rot_mat <- matrix(nrow=3, ncol=3, c(n_vec[1]^2*(1-cos(al))+cos(al), n_vec[2]*n_vec[1]*(1-cos(al))+n_vec[3]*sin(al), n_vec[3]*n_vec[1]*(1-cos(al))-n_vec[2]*sin(al),n_vec[1]*n_vec[2]*(1-cos(al))-n_vec[3]*sin(al), n_vec[2]^2*(1-cos(al))+cos(al), n_vec[3]*n_vec[2]*(1-cos(al))+n_vec[1]*sin(al),n_vec[1]*n_vec[3]*(1-cos(al))+n_vec[2]*sin(al), n_vec[2]*n_vec[3]*(1-cos(al))-n_vec[1]*sin(al), n_vec[3]^2*(1-cos(al))+cos(al)))

  dreh_func <- function(vec){
    r<-rot_mat%*%matrix(nrow=3, ncol=1, c(vec[1],vec[2],vec[3]))
    return(r)
  }
  surface_total <- as.data.frame(t(apply(surface_total, 1, dreh_func)))
  names(surface_total) <- c("x","y","z")


  # rotate with angle alpha
  alpha <- pi/4
  df <- rotate3d(cbind(surface_total$x,surface_total$y,surface_total$z), x = 1, y = -0.4, z = 0, angle = alpha)*r
  surface_total <- data.frame(x=df[,1], y=df[,2], z=df[,3])

  return(surface_total)
}
.plot_CatalanSurface <-
function(A, al = 1.5*pi, pointsize = 0.00001, ball.color="green2red", theta.pl2D = 40, phi.pl2D = 10){ ## rotate the ball with angle alpha

  y <- -A$y+1
  x <- -A$x+1
  B1<-data.frame(x=c(x,x,x,x),y=(c(y+3.3,y+2.2,y+1.1,y))/4.4)

  u <- B1$x*10-5
  v <- B1$y*10-5
  x <- u-sin(u)*cos(v)
  y <- 1-cos(u)*cosh(v)
  z <- 4*sin(u/2)*sinh(v/2)

  surface <- data.frame(x=x,y=y,z=z)

  n_vec <- c(0,0,1)/1
  rot_mat <- matrix(nrow=3, ncol=3, c(n_vec[1]^2*(1-cos(al))+cos(al), n_vec[2]*n_vec[1]*(1-cos(al))+n_vec[3]*sin(al), n_vec[3]*n_vec[1]*(1-cos(al))-n_vec[2]*sin(al),n_vec[1]*n_vec[2]*(1-cos(al))-n_vec[3]*sin(al), n_vec[2]^2*(1-cos(al))+cos(al), n_vec[3]*n_vec[2]*(1-cos(al))+n_vec[1]*sin(al),n_vec[1]*n_vec[3]*(1-cos(al))+n_vec[2]*sin(al), n_vec[2]*n_vec[3]*(1-cos(al))-n_vec[1]*sin(al), n_vec[3]^2*(1-cos(al))+cos(al)))

  dreh_func <- function(vec){
    r<-rot_mat%*%matrix(nrow=3, ncol=1, c(vec[1],vec[2],vec[3]))
    return(r)
  }

  surface2 <- as.data.frame(t(apply(surface, 1, dreh_func)))
  names(surface2) <- c("x","y","z")

  surface2$z <- -surface2$z+1

  return(surface2)

}
.plot_EnneperMinimalSurface <-
function(A, al = pi/4, pointsize = 0.00001, ball.color="green2red", theta.pl2D = 10, phi.pl2D = 40){ ## rotate the ball with angle alpha

  B1<-data.frame(x=c(A$x,A$x,A$x,A$x),y=(c(A$y+3.3,A$y+2.2,A$y+1.1,A$y))/4.4)

  u <- B1$x*10-5
  v <- B1$y*10-5
  x <- u-1/3*u^2+u*v^2
  y <- -v-u^2*v+1/3*v^3
  z <- u^2-v^2

  surface <- data.frame(x=x,y=y,z=z)

  n_vec <- c(1,1,1)/3
  rot_mat <- matrix(nrow=3, ncol=3, c(n_vec[1]^2*(1-cos(al))+cos(al), n_vec[2]*n_vec[1]*(1-cos(al))+n_vec[3]*sin(al), n_vec[3]*n_vec[1]*(1-cos(al))-n_vec[2]*sin(al),n_vec[1]*n_vec[2]*(1-cos(al))-n_vec[3]*sin(al), n_vec[2]^2*(1-cos(al))+cos(al), n_vec[3]*n_vec[2]*(1-cos(al))+n_vec[1]*sin(al),n_vec[1]*n_vec[3]*(1-cos(al))+n_vec[2]*sin(al), n_vec[2]*n_vec[3]*(1-cos(al))-n_vec[1]*sin(al), n_vec[3]^2*(1-cos(al))+cos(al)))

  dreh_func <- function(vec){
    r<-rot_mat%*%matrix(nrow=3, ncol=1, c(vec[1],vec[2],vec[3]))
    return(r)
  }

  surface2 <- as.data.frame(t(apply(surface, 1, dreh_func)))
  names(surface2) <- c("x","y","z")

  return(surface2)

}
.plot_Helix <-
function(A, len = 8, radius = 2.5, pointsize = 0.00001, ball.color="green2red", theta.pl2D = 50, phi.pl2D = 40){ ## rotate the ball with angle alpha

  x <- -A$x+1
  x <- c(0.48*x,0.48*x+0.5)
  y <- c(0.98*A$y,0.98*A$y)

  r <- y*radius + radius/5    # radius
  t <- x*len    # with length len
  x <- cos(t)*r
  y <- sin(t)*r
  z <- t
  surface <- data.frame(x=x,y=y,z=z)

  return(surface)

}
.plot_Torus <-
function(A, R = 2, r = 1, al = pi/4, pointsize = 0.00001, ball.color="green2red", theta.pl2D = 10, phi.pl2D = 40){ ## rotate the ball with angle alpha

  B1<-data.frame(x=c(A$x,A$x,A$x,A$x),y=(c(A$y+3.3,A$y+2.2,A$y+1.1,A$y))/4.4)

  t1 <- B1$x*2*pi
  t2 <- B1$y*2*pi
  x <- cos(t1)*(R+r*cos(t2))
  y <- sin(t1)*(R+r*cos(t2))
  z <- sin(t2)*r

  surface <- data.frame(x=x,y=y,z=z)

  n_vec <- c(1,1,1)/3
  rot_mat <- matrix(nrow=3, ncol=3, c(n_vec[1]^2*(1-cos(al))+cos(al), n_vec[2]*n_vec[1]*(1-cos(al))+n_vec[3]*sin(al), n_vec[3]*n_vec[1]*(1-cos(al))-n_vec[2]*sin(al),n_vec[1]*n_vec[2]*(1-cos(al))-n_vec[3]*sin(al), n_vec[2]^2*(1-cos(al))+cos(al), n_vec[3]*n_vec[2]*(1-cos(al))+n_vec[1]*sin(al),n_vec[1]*n_vec[3]*(1-cos(al))+n_vec[2]*sin(al), n_vec[2]*n_vec[3]*(1-cos(al))-n_vec[1]*sin(al), n_vec[3]^2*(1-cos(al))+cos(al)))

  dreh_func <- function(vec){
    r<-rot_mat%*%matrix(nrow=3, ncol=1, c(vec[1],vec[2],vec[3]))
    return(r)
  }

  surface2 <- as.data.frame(t(apply(surface, 1, dreh_func)))
  names(surface2) <- c("x","y","z")

  return(surface2)

}
