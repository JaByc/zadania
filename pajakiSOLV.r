rm(list=ls())

t = 0.1 #time
v <- 1

Xpos1 <- function(x1, x2, y1 , y2, t, v){
	ans <- v*t* cos(atan( (y2-y1)/(x2-x1)  ))
	return(ans)
}
Ypos1 <- function(x1, x2, y1 , y2, t, v){
	ans <- v*t* sin(atan( (y2-y1)/(x2-x1)  ))
	return(ans)
}

Xpos2 <- function(x2, x3, y2 , y3, t, v){
	ans <- v*t* sin(atan( (x3-x2)/(y3-y2)  ) + pi)
	return(ans)
}
Ypos2 <- function(x2, x3, y2 , y3, t, v){
	ans <- v*t* cos(atan( (x3-x2)/(y3-y2)  ) + pi)
	return(ans)
}

Xpos3 <- function(x3, x4, y3 , y4, t, v){
	ans <- v*t* cos(atan( (y4-y3)/(x4-x3)  ) - pi)
	return(ans)
}
Ypos3 <- function(x3, x4, y3 , y4, t, v){
	ans <- v*t* sin(atan( (y4-y3)/(x4-x3)  ) + pi)
	return(ans)
}

Xpos4 <- function(x4, x1, y4 , y1, t, v){
	ans <- v*t* sin(atan( (x4-x1)/(y4-y1)  ) )
	return(ans)
}
Ypos4 <- function(x4, x1, y4 , y1, t, v){
	ans <- v*t* cos(atan( (x4-x1)/(y4-y1)  ) )
	return(ans)
}

xALLpos = data.frame(x1 = c(-5), x2 = c(5), x3 = c(5), x4 = c(-5))
yALLpos = data.frame(y1 = c(5),  y2 = c(5), y3 = c(-5), y4 = c(-5))
end=500

for( i in 1:end){

 if( yALLpos$y1[i] >= yALLpos$y4[i] && yALLpos$y1[i] >= yALLpos$y2[i]  ){
  tmpX1 <- Xpos1( xALLpos$x1[i], xALLpos$x2[i], yALLpos$y1[i] , yALLpos$y2[i], t, v )
  tmpX2 <- Xpos2( xALLpos$x2[i], xALLpos$x3[i], yALLpos$y2[i] , yALLpos$y3[i], t, v )
  tmpX3 <- Xpos3( xALLpos$x3[i], xALLpos$x4[i], yALLpos$y3[i] , yALLpos$y4[i], t, v )
  tmpX4 <- Xpos4( xALLpos$x4[i], xALLpos$x1[i], yALLpos$y4[i] , yALLpos$y1[i], t, v )

  tmpY1 <- Ypos1( xALLpos$x1[i], xALLpos$x2[i], yALLpos$y1[i] , yALLpos$y2[i], t, v )
  tmpY2 <- Ypos2( xALLpos$x2[i], xALLpos$x3[i], yALLpos$y2[i] , yALLpos$y3[i], t, v )
  tmpY3 <- Ypos3( xALLpos$x3[i], xALLpos$x4[i], yALLpos$y3[i] , yALLpos$y4[i], t, v )
  tmpY4 <- Ypos4( xALLpos$x4[i], xALLpos$x1[i], yALLpos$y4[i] , yALLpos$y1[i], t, v )
   
    as.data.frame(XtmpVec <- c( tmpX1, tmpX2, tmpX3, tmpX4) + xALLpos[i,])
    as.data.frame(YtmpVec <- c( tmpY1, tmpY2, tmpY3, tmpY4) + yALLpos[i,])

    xALLpos <- rbind(xALLpos, XtmpVec)
    yALLpos <- rbind(yALLpos, YtmpVec)
 }else{
  if(yALLpos$y1[i] < yALLpos$y4[i] && yALLpos$y1[i] >= yALLpos$y2[i] ){
	tmpX4 <- Xpos1( xALLpos$x4[i], xALLpos$x1[i], yALLpos$y4[i] , yALLpos$y1[i], t, v )
	tmpX1 <- Xpos2( xALLpos$x1[i], xALLpos$x2[i], yALLpos$y1[i] , yALLpos$y2[i], t, v )
	tmpX2 <- Xpos3( xALLpos$x2[i], xALLpos$x3[i], yALLpos$y2[i] , yALLpos$y3[i], t, v )
	tmpX3 <- Xpos4( xALLpos$x3[i], xALLpos$x4[i], yALLpos$y3[i] , yALLpos$y4[i], t, v )

	tmpY4 <- Ypos1( xALLpos$x4[i], xALLpos$x1[i], yALLpos$y4[i] , yALLpos$y1[i], t, v )
	tmpY1 <- Ypos2( xALLpos$x1[i], xALLpos$x2[i], yALLpos$y1[i] , yALLpos$y2[i], t, v )
	tmpY2 <- Ypos3( xALLpos$x2[i], xALLpos$x3[i], yALLpos$y2[i] , yALLpos$y3[i], t, v )
	tmpY3 <- Ypos4( xALLpos$x3[i], xALLpos$x4[i], yALLpos$y3[i] , yALLpos$y4[i], t, v )
   
    as.data.frame(XtmpVec <- c( tmpX1, tmpX2, tmpX3, tmpX4) + xALLpos[i,])
    as.data.frame(YtmpVec <- c( tmpY1, tmpY2, tmpY3, tmpY4) + yALLpos[i,])

    xALLpos <- rbind(xALLpos, XtmpVec)
    yALLpos <- rbind(yALLpos, YtmpVec)
  }else{
	if(yALLpos$y1[i] < yALLpos$y4[i] && yALLpos$y1[i] < yALLpos$y2[i] ){
	tmpX3 <- Xpos1( xALLpos$x3[i], xALLpos$x4[i], yALLpos$y3[i] , yALLpos$y4[i], t, v )
	tmpX4 <- Xpos2( xALLpos$x4[i], xALLpos$x1[i], yALLpos$y4[i] , yALLpos$y1[i], t, v )
	tmpX1 <- Xpos3( xALLpos$x1[i], xALLpos$x2[i], yALLpos$y1[i] , yALLpos$y2[i], t, v )
	tmpX2 <- Xpos4( xALLpos$x2[i], xALLpos$x3[i], yALLpos$y2[i] , yALLpos$y3[i], t, v )

	tmpY3 <- Ypos1( xALLpos$x3[i], xALLpos$x4[i], yALLpos$y3[i] , yALLpos$y4[i], t, v )
	tmpY4 <- Ypos2( xALLpos$x4[i], xALLpos$x1[i], yALLpos$y4[i] , yALLpos$y1[i], t, v )
	tmpY1 <- Ypos3( xALLpos$x1[i], xALLpos$x2[i], yALLpos$y1[i] , yALLpos$y2[i], t, v )
	tmpY2 <- Ypos4( xALLpos$x2[i], xALLpos$x3[i], yALLpos$y2[i] , yALLpos$y3[i], t, v )
   
    as.data.frame(XtmpVec <- c( tmpX1, tmpX2, tmpX3, tmpX4) + xALLpos[i,])
    as.data.frame(YtmpVec <- c( tmpY1, tmpY2, tmpY3, tmpY4) + yALLpos[i,])

    xALLpos <- rbind(xALLpos, XtmpVec)
    yALLpos <- rbind(yALLpos, YtmpVec)
	}else{

		tmpX2 <- Xpos1( xALLpos$x2[i], xALLpos$x3[i], yALLpos$y2[i] , yALLpos$y3[i], t, v )
		tmpX3 <- Xpos2( xALLpos$x3[i], xALLpos$x4[i], yALLpos$y3[i] , yALLpos$y4[i], t, v )
		tmpX4 <- Xpos3( xALLpos$x4[i], xALLpos$x1[i], yALLpos$y4[i] , yALLpos$y1[i], t, v )
		tmpX1 <- Xpos4( xALLpos$x1[i], xALLpos$x2[i], yALLpos$y1[i] , yALLpos$y2[i], t, v )

		tmpY2 <- Ypos1( xALLpos$x2[i], xALLpos$x3[i], yALLpos$y2[i] , yALLpos$y3[i], t, v )
		tmpY3 <- Ypos2( xALLpos$x3[i], xALLpos$x4[i], yALLpos$y3[i] , yALLpos$y4[i], t, v )
		tmpY4 <- Ypos3( xALLpos$x4[i], xALLpos$x1[i], yALLpos$y4[i] , yALLpos$y1[i], t, v )
		tmpY1 <- Ypos4( xALLpos$x1[i], xALLpos$x2[i], yALLpos$y1[i] , yALLpos$y2[i], t, v )
   
    		as.data.frame(XtmpVec <- c( tmpX1, tmpX2, tmpX3, tmpX4) + xALLpos[i,])
    		as.data.frame(YtmpVec <- c( tmpY1, tmpY2, tmpY3, tmpY4) + yALLpos[i,])

    		xALLpos <- rbind(xALLpos, XtmpVec)
    		yALLpos <- rbind(yALLpos, YtmpVec)

	}
  }
 }
}

plot(xALLpos$x1,yALLpos$y1, type="l", xlim=c(-5,5), ylim=c(-5,5), xlab="",ylab="", xaxt="n",yaxt="n")
matplot(xALLpos$x2, yALLpos$y2, type="l", xlim=c(-5,5), ylim=c(-5,5),add=T)
matplot(xALLpos$x3, yALLpos$y3, type="l", xlim=c(-5,5), ylim=c(-5,5),add=T)
matplot(xALLpos$x4, yALLpos$y4, type="l", xlim=c(-5,5), ylim=c(-5,5),add=T)



