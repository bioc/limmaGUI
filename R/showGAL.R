showGAL <- function()
{
  Try(gal <- get("gal",envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))  
  Try(ArraysLoaded <- get("ArraysLoaded",envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported <- get("NormalizedMADataWasImported",envir=limmaGUIenvironment))
  
  Try(if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      Try(tkmessageBox(title="View Gene List",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  })
  

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))

  Try(nrows <- nrow(gal))
  Try(ncols <- ncol(gal))  
  
	Try(tempfile1 <- tempfile())
	Try(write.table(gal,file=tempfile1,quote=FALSE,col.names=NA,sep="\t"))
	Try(ttGALTable <- tktoplevel(.limmaGUIglobals$ttMain))
	Try(tkwm.title(ttGALTable,"Gene List"))
	Try(xscr <-tkscrollbar(ttGALTable, repeatinterval=5,orient="horizontal",command=function(...)tkxview(txt,...)))
	Try(scr <- tkscrollbar(ttGALTable, repeatinterval=5,command=function(...)tkyview(txt,...)))
	Try(txt <- tktext(ttGALTable, bg="white", font="courier",xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(scr,...),wrap="none",width=60))

	Try(copyText2 <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Copy>>"))))

	Try(editPopupMenu2 <- tkmenu(txt, tearoff=FALSE))
	Try(tkadd(editPopupMenu2, "command", label="Copy <Ctrl-C>",command=copyText2))

	RightClick2 <- function(x,y) # x and y are the mouse coordinates
	{
	 Try(rootx <- as.integer(tkwinfo("rootx",txt)))
	 Try(rooty <- as.integer(tkwinfo("rooty",txt)))
	 Try(xTxt <- as.integer(x)+rootx)
	 Try(yTxt <- as.integer(y)+rooty)
	 Try(.Tcl(paste("tk_popup",.Tcl.args(editPopupMenu2,xTxt,yTxt))))
	}
	Try(tkbind(txt, "<Button-3>",RightClick2))

	Try(tkpack(scr, side="right", fill="y"))
	Try(tkpack(xscr, side="bottom", fill="x"))
	Try(tkpack(txt, side="left", fill="both", expand="yes"))

	Try(chn <- tclvalue(tkcmd("open", tempfile1)))
	Try(tkinsert(txt, "end", tclvalue(tkcmd("read", chn))))
	Try(tkcmd("close", chn))
	Try(tkconfigure(txt, state="disabled"))
	Try(tkmark.set(txt,"insert","0.0"))
	Try(tkfocus(txt))

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
  
  SaveGAL <- function()
  {
    Try(galFile <- tclvalue(tkgetSaveFile(initialfile=paste(limmaDataSetNameText,".gal",sep=""))))
    Try(if (!nchar(galFile))
      return())
    Try(write.table(gal,file=galFile,quote=FALSE,row.names=FALSE,sep="\t"))
  } 
  
	Try(copyFcn <-      function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Copy>>"))))

	Try(topMenu2 <- tkmenu(ttGALTable))
	Try(tkconfigure(ttGALTable, menu=topMenu2))
	Try(fileMenu2 <- tkmenu(topMenu2, tearoff=FALSE))
	Try(tkadd(fileMenu2, "command", label="Save As",command=SaveGAL))
	Try(tkadd(fileMenu2, "command", label="Close",command=function() tkdestroy(ttGALTable)))
	Try(tkadd(topMenu2, "cascade", label="File",menu=fileMenu2))
	Try(editMenu2 <- tkmenu(topMenu2, tearoff=FALSE))
	Try(tkadd(editMenu2, "command", label="Copy <Ctrl-C>",command=copyFcn))
	Try(tkadd(topMenu2, "cascade", label="Edit",menu=editMenu2))

	Try(tkfocus(ttGALTable))
  
}
