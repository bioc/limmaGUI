PlotOptions <- function()
{
  Try(ttGetPlotOptions<-tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.title(ttGetPlotOptions,"Plot Options"))
  Try(tkwm.deiconify(ttGetPlotOptions))
  Try(tkgrab.set(ttGetPlotOptions))
  Try(tkframe1 <- tkframe(ttGetPlotOptions,borderwidth=2))
  Try(tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2))
  Try(tkframe4<-tkframe(tkframe1,borderwidth=2))

  Try(tkgrid(tklabel(tkframe1,text="    ")))

  Try(tkgrid(tklabel(tkframe2,text="Choose a graphics device for plotting.",font=.limmaGUIglobals$limmaGUIfont2),column=2,rowspan=1,columnspan=2,sticky="w"))

  Try(limmaGUIglobals <- get(".limmaGUIglobals",envir=.GlobalEnv))

  Try(if (!("graphicsDevice" %in% names(limmaGUIglobals)))
  {
    Try(limmaGUIglobals$graphicsDevice <- "tkrplot")
    Try(assign(".limmaGUIglobals",limmaGUIglobals,.GlobalEnv))
  })
  Try(graphicsDevice <- .limmaGUIglobals$graphicsDevice)

  Try(graphicsDeviceTcl <- tclVar(graphicsDevice))

  Try(tkrplot.but <- tkradiobutton(tkframe2,text="Tk R Plot",variable=graphicsDeviceTcl,value="tkrplot",font=.limmaGUIglobals$limmaGUIfont2))  
  Try(R.but       <- tkradiobutton(tkframe2,text="R",variable=graphicsDeviceTcl,value="R",font=.limmaGUIglobals$limmaGUIfont2))  
    
  Try(tkgrid(tkrplot.but,column=2))
  Try(tkgrid(R.but,column=2))

  Try(tkgrid.configure(tkrplot.but,R.but,sticky="w"))
  Try(tkgrid(tkframe2))
  Try(newGraphicsDevice <- "")
  onOK <- function()
  {
    Try(newGraphicsDevice<<-tclvalue(graphicsDeviceTcl))
    Try(tkgrab.release(ttGetPlotOptions))
    Try(tkdestroy(ttGetPlotOptions))
    Try(if (newGraphicsDevice==graphicsDevice)
      return())    
    Try(graphicsDevice <- newGraphicsDevice)
    
    Try(if (graphicsDevice=="R" && .Platform$OS.type=="windows")
      Try(tkmessageBox(title="R graphics device",
      message="It is strongly recommended that you run R in SDI (Single Document Interface) mode when using the R graphics device",icon="warning")))
    
    Try(limmaGUIglobals <- get(".limmaGUIglobals",envir=.GlobalEnv))
    Try(limmaGUIglobals$graphicsDevice <- graphicsDevice)
    Try(assign(".limmaGUIglobals",limmaGUIglobals,.GlobalEnv))
    Try(tkfocus(.limmaGUIglobals$ttMain))  
  }
  Try(OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=function(){Try(tkgrab.release(ttGetPlotOptions));Try(tkdestroy(ttGetPlotOptions));newGraphicsDevice<-"";Try(tkfocus(.limmaGUIglobals$ttMain))},font=.limmaGUIglobals$limmaGUIfont2))
  Try(onHelp <- function() tkmessageBox(title="Plotting Device",
    message="The R graphics is device can be resized more easily, but it may be inconvenient for Windows users running R in MDI mode.",
    icon="info"))
  Try(Help.but <- tkbutton(tkframe4,text=" Help ",command=function()Try(onHelp()),font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(tkframe4,text="                    ")))
  Try(tkgrid(OK.but,Cancel.but,Help.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="e"))
  Try(tkgrid.configure(Help.but,sticky="e"))  
  Try(tkgrid(tklabel(tkframe4,text="       ")))
  Try(tkgrid(tkframe4))
  Try(tkgrid(tkframe1))
  Try(tkfocus(OK.but))
  Try(tkbind(ttGetPlotOptions, "<Return>",onOK))
  Try(tkbind(ttGetPlotOptions, "<Destroy>", function() {Try(tkgrab.release(ttGetPlotOptions));Try(tkfocus(.limmaGUIglobals$ttMain))}))
  Try(tkwait.window(ttGetPlotOptions))

  Try(tkdestroy(ttGetPlotOptions))  

# This return value below is not used.  The function above is used for its effect
# on PlotOptions in limmaGUIenvironment
  return(newGraphicsDevice)
}
