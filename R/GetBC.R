BChelp <- function()
{
  Try(help("backgroundCorrect",htmlhelp=TRUE))
}

GetBackgroundCorrectionMethod <- function()
{
  Try(ttGetBCMethod<-tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.title(ttGetBCMethod,"Background Correction Method"))
  Try(tkwm.deiconify(ttGetBCMethod))
  Try(tkgrab.set(ttGetBCMethod))
  Try(tkframe1 <- tkframe(ttGetBCMethod,borderwidth=2))
  Try(tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2))
  Try(tkframe4<-tkframe(tkframe1,borderwidth=2))

  Try(tkgrid(tklabel(tkframe1,text="    ")))

  Try(tkgrid(tklabel(tkframe2,text="Choose a method for background correction.",font=.limmaGUIglobals$limmaGUIfont2),column=2,rowspan=1,columnspan=3,sticky="w"))

  Try(if (!exists("BCMethod",envir=limmaGUIenvironment))
  {
    Try(BCMethod <- "subtract")
    Try(assign("BCMethod",BCMethod,limmaGUIenvironment))
  })
  Try(BCMethod <- get("BCMethod",envir=limmaGUIenvironment))

  Try(methBC <- tclVar(BCMethod))
  Try(none.but <- tkradiobutton(tkframe2,text="None",variable=methBC,value="none",font=.limmaGUIglobals$limmaGUIfont2))
  Try(subtract.but <- tkradiobutton(tkframe2,text="Subtract",variable=methBC,value="subtract",font=.limmaGUIglobals$limmaGUIfont2))
  Try(half.but <- tkradiobutton(tkframe2,text="Half",variable=methBC,value="half",font=.limmaGUIglobals$limmaGUIfont2))
  Try(minimum.but <- tkradiobutton(tkframe2,text="Minimum",variable=methBC,value="minimum",font=.limmaGUIglobals$limmaGUIfont2))
  Try(edwards.but <- tkradiobutton(tkframe2,text="Edwards",variable=methBC,value="edwards",font=.limmaGUIglobals$limmaGUIfont2))
    
  Try(tkgrid(none.but,column=2))
  Try(tkgrid(subtract.but,column=2))
  Try(tkgrid(half.but,column=2))
  Try(tkgrid(minimum.but,column=2))
  Try(tkgrid(edwards.but,column=2))
  Try(tkgrid.configure(none.but,subtract.but,half.but,minimum.but,edwards.but,sticky="w"))
  Try(tkgrid(tkframe2))
  Try(NewBCMethod <- "")
  onOK <- function()
  {
    Try(NewBCMethod<<-tclvalue(methBC));
    Try(tkgrab.release(ttGetBCMethod));
    Try(tkdestroy(ttGetBCMethod))
    Try(if (NewBCMethod==BCMethod)
      return())    
    Try(BCMethod <- NewBCMethod)
    Try(assign("BCMethod",BCMethod,limmaGUIenvironment))
    # Maybe update the status window!
    Try(tkfocus(.limmaGUIglobals$ttMain))  
  }
  Try(OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=function(){Try(tkgrab.release(ttGetBCMethod));Try(tkdestroy(ttGetBCMethod));NewBCMethod<-"";Try(tkfocus(.limmaGUIglobals$ttMain))},font=.limmaGUIglobals$limmaGUIfont2))
  Try(Help.but <- tkbutton(tkframe4,text=" Help ",command=BChelp,font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(tkframe4,text="                    ")))
  Try(tkgrid(OK.but,Cancel.but,Help.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="e"))
  Try(tkgrid.configure(Help.but,sticky="e"))  
  Try(tkgrid(tklabel(tkframe4,text="       ")))
  Try(tkgrid(tkframe4))
  Try(tkgrid(tkframe1))
  Try(tkfocus(OK.but))
  Try(tkbind(ttGetBCMethod, "<Return>",onOK))
  Try(tkbind(ttGetBCMethod, "<Destroy>", function() {Try(tkgrab.release(ttGetBCMethod));Try(tkfocus(.limmaGUIglobals$ttMain))}))
  Try(tkwait.window(ttGetBCMethod))

  Try(tkdestroy(ttGetBCMethod))  

  return(NewBCMethod)
}
