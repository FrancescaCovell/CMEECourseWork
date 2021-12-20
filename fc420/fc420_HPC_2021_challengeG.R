# CMEE 2021 HPC excercises R code challenge G pro forma

rm(list=ls()) # nothing written elsewhere should be needed to make this work

# please edit these data to show your information.
name <- "Francesca Covell"
preferred_name <- "Fran"
email <- "f.covell20@imperial.ac.uk"
username <- "fc420"



fern2<-function(x,y,z,d){if(z>0.01)x<-turtle(c(x[1],x[2]),y,z)|fern2(c(x[1],x[2]),y,(z*0.87),d)|if(d*-1==-1)fern2(c(x[1],x[2]),y+(pi/4),(z*0.38),d*-1)|if(d*-1==1)fern2(c(x[1],x[2]),y-(pi/4),(z*0.38),d)}


draw_fern2 <- function()  {
  graphics.off()# clear any existing graphs and plot your graph within the R window
  tempplot <-plot(1, type = "n",                         # Remove all elements of plot
                  xlab = "", ylab = "",
                  xlim = c(-15, 15), ylim = c(-15, 15)) 
  fern2(c(0,-15),pi/2,4,1)
}
draw_fern2()