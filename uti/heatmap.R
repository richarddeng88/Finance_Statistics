myheatmap <- function(myMAT,low,high,uselabel){
  colorFun <- colorRampPalette(c("blue","white","red")) 
  
  b <- boxplot(myMAT, plot = FALSE)
  thr <- c(low,high)
  colbins <- 100
  step <- abs(thr[2] - thr[1])/50
  
  myAT <- seq(thr[1], thr[2], step)
  
  myCOLregions <- colorFun(length(myAT))
  
  levelplot(myMAT, at = myAT, col.regions = myCOLregions, 
            scales=list(x=list(rot=90)), labels=uselabel)
}