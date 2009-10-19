lmFitMethodHelp <- function()
{
  Try(help("lmFit",help_type="html"))
}

GetlmFitMethod <- function()
{
  Try(ttGetlmFitMethod<-tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.title(ttGetlmFitMethod,"Linear Model Fit Method"))
  Try(tkwm.deiconify(ttGetlmFitMethod))
  Try(tkgrab.set(ttGetlmFitMethod))
  Try(tkframe1 <- tkframe(ttGetlmFitMethod,borderwidth=2))
  Try(tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2))
  Try(tkframe4<-tkframe(tkframe1,borderwidth=2))

  Try(tkgrid(tklabel(tkframe1,text="    ")))

  Try(tkgrid(tklabel(tkframe2,text="Choose a method for the linear model fit.",font=.limmaGUIglobals$limmaGUIfont2),column=2,rowspan=1,columnspan=3,sticky="w"))

  Try(if (!exists("lmFitMethod",envir=limmaGUIenvironment))
  {
    Try(lmFitMethod <- "ls")
    Try(assign("lmFitMethod",lmFitMethod,limmaGUIenvironment))
  })
  Try(lmFitMethod <- get("lmFitMethod",envir=limmaGUIenvironment))

  Try(methlmFit <- tclVar(lmFitMethod))
  Try(ls.but <- tkradiobutton(tkframe2,text="Least Squares",variable=methlmFit,value="ls",font=.limmaGUIglobals$limmaGUIfont2))
  Try(robust.but <- tkradiobutton(tkframe2,text="Robust",variable=methlmFit,value="robust",font=.limmaGUIglobals$limmaGUIfont2))
    
  Try(tkgrid(ls.but,column=2))
  Try(tkgrid(robust.but,column=2))
  Try(tkgrid.configure(ls.but,robust.but,sticky="w"))
  Try(tkgrid(tkframe2))
  Try(NewlmFitMethod <- "")
  onOK <- function()
  {
    Try(NewlmFitMethod<<-tclvalue(methlmFit));
    Try(tkgrab.release(ttGetlmFitMethod));
    Try(tkdestroy(ttGetlmFitMethod))
    Try(if (NewlmFitMethod==lmFitMethod)
      return())    
    Try(lmFitMethod <- NewlmFitMethod)
    Try(assign("lmFitMethod",lmFitMethod,limmaGUIenvironment))
    # Maybe update the status window!
    Try(tkfocus(.limmaGUIglobals$ttMain))  
  }
  Try(OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=function(){Try(tkgrab.release(ttGetlmFitMethod));Try(tkdestroy(ttGetlmFitMethod));NewlmFitMethod<-"";Try(tkfocus(.limmaGUIglobals$ttMain))},font=.limmaGUIglobals$limmaGUIfont2))
  Try(Help.but <- tkbutton(tkframe4,text=" Help ",command=lmFitMethodHelp,font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(tkframe4,text="                    ")))
  Try(tkgrid(OK.but,Cancel.but,Help.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="e"))
  Try(tkgrid.configure(Help.but,sticky="e"))  
  Try(tkgrid(tklabel(tkframe4,text="       ")))
  Try(tkgrid(tkframe4))
  Try(tkgrid(tkframe1))
  Try(tkfocus(OK.but))
  Try(tkbind(ttGetlmFitMethod, "<Return>",onOK))
  Try(tkbind(ttGetlmFitMethod, "<Destroy>", function() {Try(tkgrab.release(ttGetlmFitMethod));Try(tkfocus(.limmaGUIglobals$ttMain))}))
  Try(tkwait.window(ttGetlmFitMethod))

  Try(tkdestroy(ttGetlmFitMethod))  

  return(NewlmFitMethod)
}
