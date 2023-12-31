GetImageAnalysisColumnHeadings <- function()
{
  Try(ttImageAnalysisColumnHeadings <- tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.deiconify(ttImageAnalysisColumnHeadings))
  Try(tkgrab.set(ttImageAnalysisColumnHeadings))
  Try(tkfocus(ttImageAnalysisColumnHeadings))
  Try(tkwm.title(ttImageAnalysisColumnHeadings,"Image Analysis File Column Headings"))
  Try(tkgrid(tklabel(ttImageAnalysisColumnHeadings,text="    ")))
  Try(tkgrid(tklabel(ttImageAnalysisColumnHeadings,text="Image Analysis File Column Headings",font=.limmaGUIglobals$limmaGUIfont2)))
  Try(tkgrid(tklabel(ttImageAnalysisColumnHeadings,text="    ")))
  Try(RedForegroundTcl <- tclVar("Rf"))
  Try(entry.RedForeground<-tkentry(ttImageAnalysisColumnHeadings,width="20",font=.limmaGUIglobals$limmaGUIfont2,textvariable=RedForegroundTcl,bg="white"))
  Try(tkgrid(tklabel(ttImageAnalysisColumnHeadings,text="Red (Cy5 / 635nm) Foreground     ",font=.limmaGUIglobals$limmaGUIfont2),entry.RedForeground,sticky="w"))
  Try(RedBackgroundTcl <- tclVar("Rb"))
  Try(entry.RedBackground<-tkentry(ttImageAnalysisColumnHeadings,width="20",font=.limmaGUIglobals$limmaGUIfont2,textvariable=RedBackgroundTcl,bg="white"))
  Try(tkgrid(tklabel(ttImageAnalysisColumnHeadings,text="Red (Cy5 / 635nm) Background     ",font=.limmaGUIglobals$limmaGUIfont2),entry.RedBackground,sticky="w"))
  Try(GreenForegroundTcl <- tclVar("Gf"))
  Try(entry.GreenForeground<-tkentry(ttImageAnalysisColumnHeadings,width="20",font=.limmaGUIglobals$limmaGUIfont2,textvariable=GreenForegroundTcl,bg="white"))
  Try(tkgrid(tklabel(ttImageAnalysisColumnHeadings,text="Green (Cy3 / 532nm) Foreground   ",font=.limmaGUIglobals$limmaGUIfont2),entry.GreenForeground,sticky="w"))
  Try(GreenBackgroundTcl <- tclVar("Gb"))
  Try(entry.GreenBackground<-tkentry(ttImageAnalysisColumnHeadings,width="20",font=.limmaGUIglobals$limmaGUIfont2,textvariable=GreenBackgroundTcl,bg="white"))
  Try(tkgrid(tklabel(ttImageAnalysisColumnHeadings,text="Green (Cy3 / 532nm) Background   ",font=.limmaGUIglobals$limmaGUIfont2),entry.GreenBackground,sticky="w"))

  ReturnVal <- list()
  
  onOK <- function()
  {
    Try(Rf      <- tclvalue(RedForegroundTcl))
    Try(Rb      <- tclvalue(RedBackgroundTcl))
    Try(Gf      <- tclvalue(GreenForegroundTcl))
    Try(Gb      <- tclvalue(GreenBackgroundTcl))
    Try(tkgrab.release(ttImageAnalysisColumnHeadings))
    Try(tkdestroy(ttImageAnalysisColumnHeadings))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    Try(ReturnVal <<- list(Rf=Rf,Rb=Rb,Gf=Gf,Gb=Gb))
  }
  onCancel <- function() {Try(tkgrab.release(ttImageAnalysisColumnHeadings));Try(tkdestroy(ttImageAnalysisColumnHeadings));Try(tkfocus(.limmaGUIglobals$ttMain));Try(ReturnVal <<- list())}
  OK.but <- tkbutton(ttImageAnalysisColumnHeadings,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2)
  Cancel.but <- tkbutton(ttImageAnalysisColumnHeadings,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2)
  Try(tkgrid(tklabel(ttImageAnalysisColumnHeadings,text="    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid(tklabel(ttImageAnalysisColumnHeadings,text="    ")))
  Try(tkfocus(ttImageAnalysisColumnHeadings))
  Try(tkbind(ttImageAnalysisColumnHeadings, "<Destroy>", function() {Try(tkgrab.release(ttImageAnalysisColumnHeadings));Try(tkfocus(.limmaGUIglobals$ttMain));}))
  Try(tkwait.window(ttImageAnalysisColumnHeadings))

  return (ReturnVal)
}
