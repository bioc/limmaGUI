HistogramPlot <- function()
{
  SlideNamesVec <- get("SlideNamesVec",limmaGUIenvironment)
  RG            <- get("RG",limmaGUIenvironment)
  MA.Available  <- get("MA.Available",limmaGUIenvironment)
  ArraysLoaded  <- get("ArraysLoaded",limmaGUIenvironment)
  
  if (ArraysLoaded==FALSE)
  {
    tkmessageBox(message="Error: No arrays have been loaded",icon="error")
    return()
  }  

  slide <- GetSlideNum()
  if (slide>0) # i.e. User didn't press cancel
  {
    
    if (MA.Available$Raw==FALSE)
      MAraw <- MA.RG(RG)
    else
      MAraw <- get("MAraw",limmaGUIenvironment)
      
    tkmessageBox(message="The histogram will be plotted in the standard R graphics device, NOT a Tk window.")
      
    hist(MAraw$M[,slide],col="blue",main=paste("Histogram of M for Slide",SlideNamesVec[slide]),
      xlab="M")
  }
}
