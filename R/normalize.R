GetNormalizationMethod <- function()
{
  Try(ttGetNormalizationMethod<-tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.title(ttGetNormalizationMethod,"Normalization Method"))
  Try(tkwm.deiconify(ttGetNormalizationMethod))
  Try(tkgrab.set(ttGetNormalizationMethod))
  Try(tkframe1 <- tkframe(ttGetNormalizationMethod,borderwidth=2))
  Try(tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2))
  Try(tkframe4<-tkframe(tkframe1,borderwidth=2))

  Try(tkgrid(tklabel(tkframe1,text="    ")))

  Try(tkgrid(tklabel(tkframe2,text="Choose a method for within-array normalization.",font=.limmaGUIglobals$limmaGUIfont2),column=2,rowspan=1,columnspan=2,sticky="w"))

  Try(if (!exists("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
  {
    Try(WithinArrayNormalizationMethod <- "printtiploess")
    Try(assign("WithinArrayNormalizationMethod",WithinArrayNormalizationMethod,limmaGUIenvironment))
  })
  Try(WithinArrayNormalizationMethod <- get("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))

  Try(methnorm <- tclVar(WithinArrayNormalizationMethod))

#  Try(none.but <- tkradiobutton(tkframe2,text="None",variable=methnorm,value="none",font=.limmaGUIglobals$limmaGUIfont2))
  Try(median.but <- tkradiobutton(tkframe2,text="Median",variable=methnorm,value="median",font=.limmaGUIglobals$limmaGUIfont2))
  Try(global.but <- tkradiobutton(tkframe2,text="Global loess",variable=methnorm,value="loess",font=.limmaGUIglobals$limmaGUIfont2))
  Try(printtip.but <- tkradiobutton(tkframe2,text="Print-tip group loess",variable=methnorm,value="printtiploess",font=.limmaGUIglobals$limmaGUIfont2))
  Try(composite.but <- tkradiobutton(tkframe2,text="Composite",variable=methnorm,value="composite",font=.limmaGUIglobals$limmaGUIfont2))
  Try(robustspline.but <- tkradiobutton(tkframe2,text="Robust Spline",variable=methnorm,value="robustspline",font=.limmaGUIglobals$limmaGUIfont2))
    
#  Try(tkgrid(none.but,column=2))
  Try(tkgrid(median.but,column=2))
  Try(tkgrid(global.but,column=2))
  Try(tkgrid(printtip.but,column=2))
  Try(tkgrid(composite.but,column=2))
  Try(tkgrid(robustspline.but,column=2))
#  Try(tkgrid.configure(none.but,median.but,global.but,printtip.but,composite.but,robustspline.but,sticky="w"))
  Try(tkgrid.configure(median.but,global.but,printtip.but,composite.but,robustspline.but,sticky="w"))
  Try(tkgrid(tkframe2))
  Try(NewNormalizationMethod <- "")
  onOK <- function()
  {
    Try(NewNormalizationMethod<-tclvalue(methnorm));
    Try(tkgrab.release(ttGetNormalizationMethod));
    Try(tkdestroy(ttGetNormalizationMethod))
    Try(if (NewNormalizationMethod==WithinArrayNormalizationMethod)
      return(NewNormalizationMethod))    
    Try(WithinArrayNormalizationMethod <- NewNormalizationMethod)
    Try(assign("WithinArrayNormalizationMethod",WithinArrayNormalizationMethod,limmaGUIenvironment))
    Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
    Try(MA.Available$WithinArrays <- FALSE)
    Try(MA.Available$Both <- FALSE)
    Try(MA.Available$BetweenArrays <- FALSE)
    Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
    
    Try(tkdelete(.limmaGUIglobals$mainTree,"Raw.Status"))
    Try(tkinsert(.limmaGUIglobals$mainTree,"end","Raw","Raw.Status" ,text="Not Available",font=.limmaGUIglobals$limmaGUIfontTree))
    Try(tkdelete(.limmaGUIglobals$mainTree,"WithinOnly.Status"))
    Try(tkinsert(.limmaGUIglobals$mainTree,"end","WithinOnly","WithinOnly.Status" ,text="Not Available",font=.limmaGUIglobals$limmaGUIfontTree))
    Try(tkdelete(.limmaGUIglobals$mainTree,"WithinAndBetween.Status"))
    Try(tkinsert(.limmaGUIglobals$mainTree,"end","WithinAndBetween","WithinAndBetween.Status" ,text="Not Available",font=.limmaGUIglobals$limmaGUIfontTree))                   
    Try(tkdelete(.limmaGUIglobals$mainTree,"BetweenOnly.Status"))
    Try(tkinsert(.limmaGUIglobals$mainTree,"end","BetweenOnly","BetweenOnly.Status" ,text="Not Available",font=.limmaGUIglobals$limmaGUIfontTree))        

    Try(tkfocus(.limmaGUIglobals$ttMain))  
  }
  Try(OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=function(){Try(tkgrab.release(ttGetNormalizationMethod));Try(tkdestroy(ttGetNormalizationMethod));NewNormalizationMethod<-"";Try(tkfocus(.limmaGUIglobals$ttMain))},font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(tkframe4,text="                    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="e"))
  Try(tkgrid(tklabel(tkframe4,text="       ")))
  Try(tkgrid(tkframe4))
  Try(tkgrid(tkframe1))
  Try(tkfocus(OK.but))
  Try(tkbind(ttGetNormalizationMethod, "<Return>",onOK))
  Try(tkbind(ttGetNormalizationMethod, "<Destroy>", function() {Try(tkgrab.release(ttGetNormalizationMethod));Try(tkfocus(.limmaGUIglobals$ttMain))}))
  Try(tkwait.window(ttGetNormalizationMethod))

  Try(tkdestroy(ttGetNormalizationMethod))  

  return(NewNormalizationMethod)
}

NormalizeNow <- function()
{
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
  
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))   

  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment)) 
  Try(if (!exists("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
  {
    Try(WithinArrayNormalizationMethod <- "printtiploess")
    Try(assign("WithinArrayNormalizationMethod",WithinArrayNormalizationMethod,limmaGUIenvironment))
  })
  Try(WithinArrayNormalizationMethod <- get("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))

  
  if (ArraysLoaded==FALSE)
  {
      Try(tkmessageBox(title="Normalization",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))      
      return()
  }
  
  if (NormalizedMADataWasImported==TRUE)
  {
      Try(tkmessageBox(title="Normalization",message="Normalized data was imported.  Impossible to recover raw data to renormalize.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))      
      return()
  }

  Try(RG <- get("RG",envir=limmaGUIenvironment))
  Try(SetLayoutParamReturnVal <- 1)
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
  Try(if (length(maLayout)==0) SetLayoutParamReturnVal <-Try(SetLayoutParameters()))
  Try(if (SetLayoutParamReturnVal==0) return())
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))

  Try(NormalizeWithinArraysMB <-tkmessageBox(title="Normalization Within Arrays",message="Normalize Within Arrays?",type="yesnocancel",icon="question",default="yes"))
  Try(WhetherToNormalizeWithinArrays <- tclvalue(NormalizeWithinArraysMB))
  if (WhetherToNormalizeWithinArrays=="cancel")
      return()
  Try(NormalizeBetweenArraysMB <-tkmessageBox(title="Normalization Between Arrays",message="Normalize Between Arrays?",type="yesnocancel",icon="question",default="no"))
  Try(WhetherToNormalizeBetweenArrays <- tclvalue(NormalizeBetweenArraysMB))
  if (WhetherToNormalizeBetweenArrays=="cancel")
    return()

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

  Try(WeightingType <- get("WeightingType",envir=limmaGUIenvironment))

  Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))

	if (WhetherToNormalizeWithinArrays=="yes")
	{
			if (MA.Available$WithinArrays)
			{
				Try(MA <- get("MAwithinArrays",envir=limmaGUIenvironment))
				Try(assign("MA",MA,limmaGUIenvironment))
			}
			else          
			{
				if (WeightingType == "none")
					Try (MA <- normalizeWithinArrays(RG,maLayout,method=WithinArrayNormalizationMethod))
				else
					Try(MA <- normalizeWithinArrays(RG,weights=RG$weights,maLayout,method=WithinArrayNormalizationMethod))
				Try(assign("MA",MA,limmaGUIenvironment))        
				Try(assign("MAwithinArrays",MA,limmaGUIenvironment))
				Try(MA.Available$WithinArrays <- TRUE)
				Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
				Try(tkdelete(.limmaGUIglobals$mainTree,"WithinOnly.Status"))
				Try(tkinsert(.limmaGUIglobals$mainTree,"end","WithinOnly","WithinOnly.Status" ,text=paste("Available (using ",WithinArrayNormalizationMethod,")",sep=""),font=.limmaGUIglobals$limmaGUIfontTree))
			}
	}
	else
	{
			if (MA.Available$Raw)
			{
				Try(MA <- get("MAraw",envir=limmaGUIenvironment))
				Try(assign("MA",MA,limmaGUIenvironment))
			}
			else          
			{
				Try (MA <- MA.RG(RG))
				Try(assign("MA",MA,limmaGUIenvironment))        
				Try(assign("MAraw",MA,limmaGUIenvironment))
				Try(MA.Available$Raw <- TRUE)
				Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
				Try(tkdelete(.limmaGUIglobals$mainTree,"Raw.Status"))
				Try(tkinsert(.limmaGUIglobals$mainTree,"end","Raw","Raw.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
			}        
	}
	Try(MA <- get("MA",envir=limmaGUIenvironment))

	if (WhetherToNormalizeBetweenArrays=="yes") 
	{
		if (WhetherToNormalizeWithinArrays=="yes")
		{
			if (MA.Available$Both)
			{
				Try(MA <- get("MAboth",envir=limmaGUIenvironment))
				Try(assign("MA",MA,limmaGUIenvironment))                    
			}
			else
			{
				Try (MA <- normalizeBetweenArrays(MA))
				Try(assign("MA",MA,limmaGUIenvironment))        
				Try(assign("MAboth",MA,limmaGUIenvironment))
				Try(MA.Available$Both <- TRUE)
				Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
				Try(tkdelete(.limmaGUIglobals$mainTree,"WithinAndBetween.Status"))
				Try(tkinsert(.limmaGUIglobals$mainTree,"end","WithinAndBetween","WithinAndBetween.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))                   
			}

		}
		else
		{
			if (MA.Available$BetweenArrays)
			{
				Try(MA <- get("MAbetweenArrays",envir=limmaGUIenvironment))
				Try(assign("MA",MA,limmaGUIenvironment))                    
			}
			else
			{
				Try (MA <- normalizeBetweenArrays(MA))
				Try(assign("MA",MA,limmaGUIenvironment))        
				Try(assign("MAbetweenArrays",MA,limmaGUIenvironment))
				Try(MA.Available$BetweenArrays <- TRUE)
				Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
				Try(tkdelete(.limmaGUIglobals$mainTree,"BetweenOnly.Status"))
				Try(tkinsert(.limmaGUIglobals$mainTree,"end","BetweenOnly","BetweenOnly.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))        
			}        
		}
	}

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

}

AboutNormalization <- function()
{
  Try(winTitle<-"About Normalization")
  Try(message<-paste("Almost all cDNA data should be normalized within arrays.\n",
    "The default method of print-tip-group loess should be suitable for most data.\n",
    "If there is a significant difference in scale between the arrays (as seen by an M Box Plot by slide),\n",
    "then the data should be normalized between arrays as well.\n\n",
    
    "Use the M Box Plots and the M A Plots (with lowess curves) to see the effects of the normalization.\n\n",
    
    "The M A plot (with lowess curves) normalizes only for a single array with print-tip-group lowess\n",
    "normalization using the sma package, and the results are discarded after displaying the plot.\n",
    "This is designed to give a quick (but not as accurate) impression of the effects of normalization on\n",
    "one slide.\n\n",
    
    "The M Box Plot and the Color-Coded M A Plot both use the same normalization methods (from the\n",
    "limma package) which are used in the linear model. Here, you have to normalize within all arrays\n",
    "at once (which can be slow the first time), but the results will be recorded in the left status\n",
    "window, and as long as you don't change the within-array normalization method, your data will not\n",
    "need to be normalized again.  If you ask for a plot or a linear model requiring normalized M and A\n",
    "values, it should be much quicker the second time.\n",sep=""))
    
  Try(ttAboutNormalization  <- tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.title(ttAboutNormalization,winTitle))
  Try(tkwm.deiconify(ttAboutNormalization))
  Try(tkgrab.set(ttAboutNormalization))
  Try(scr <- tkscrollbar(ttAboutNormalization, repeatinterval=5,
                         command=function(...)tkyview(txt,...)))
  Try(txt <- tktext(ttAboutNormalization,bg="white",yscrollcommand=function(...)tkset(scr,...)))
  Try(tkgrid(txt,scr))
  Try(tkgrid.configure(scr,sticky="ns"))
  Try(tkgrid.configure(txt,sticky="nsew"))
  Try(tkinsert(txt,"end",message))
  Try(tkconfigure(txt, state="disabled"))
  Try(tkfocus(txt))
  Try(onOK <- function() {Try(tkgrab.release(ttAboutNormalization));tkdestroy(ttAboutNormalization)})
  Try(OK.but <- tkbutton(ttAboutNormalization,text="  Close  ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2))  
  Try(tkgrid(tklabel(ttAboutNormalization,text="    ")))
  Try(tkgrid(OK.but))
  Try(tkgrid(tklabel(ttAboutNormalization,text="    ")))    
  Try(tkfocus(OK.but))
  Try(tkwait.window(ttAboutNormalization))
}

ExportMvalues <- function()
{
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))   
  Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
  
  Try(if (NormalizedMADataWasImported==FALSE)
  {
    Try(Which <- WithinBetweenOrBoth("M"))
    Try(if (Which=="") return())
		Try(if (MA.Available[[Which]]==FALSE) 
		{
			tkmessageBox(title="MA Object Unavailable",message="The MA object you requested is unavailable.  Click on \"Normalize / Update M and A\" from the \"Normalization\" menu.")
			return()
		})
		Try(if (Which=="Raw")           MA <- get("MAraw",envir=limmaGUIenvironment))
		Try(if (Which=="WithinArrays")  MA <- get("MAwithinArrays",envir=limmaGUIenvironment))  
		Try(if (Which=="BetweenArrays") MA <- get("MAbetweenArrays",envir=limmaGUIenvironment))    
		Try(if (Which=="Both")          MA <- get("MAboth",envir=limmaGUIenvironment))      
  }
  else
    Try(MA <- get("MAimported",envir=limmaGUIenvironment)))

	Try(FileName <- tclvalue(tkgetSaveFile(initialfile=paste(limmaDataSetNameText,"_M.txt",sep=""),filetypes="{{Tab-Delimited Text Files} {.txt}} {{All files} *}")))
	Try(if (!nchar(FileName)) return())
	Try(len <- nchar(FileName))
	if (len<=4)
		Try(FileName <- paste(FileName,".txt",sep=""))
	else if (substring(FileName,len-3,len)!=".txt")
				Try(FileName <- paste(FileName,".txt",sep=""))

	Try(write.table(MA$M,file=FileName,sep="\t",quote=FALSE,col.names=TRUE,row.names=FALSE))
}


ExportAvalues <- function()
{
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))     
  Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))  

  Try(if (NormalizedMADataWasImported==FALSE)
  {
    Try(Which <- WithinBetweenOrBoth("A"))
    Try(if (Which=="") return())
		Try(if (MA.Available[[Which]]==FALSE) 
		{
			tkmessageBox(title="MA Object Unavailable",message="The MA object you requested is unavailable.  Click on \"Normalize / Update M and A\" from the \"Normalization\" menu.")
			return()
		})
		Try(if (Which=="Raw")           MA <- get("MAraw",envir=limmaGUIenvironment))
		Try(if (Which=="WithinArrays")  MA <- get("MAwithinArrays",envir=limmaGUIenvironment))  
		Try(if (Which=="BetweenArrays") MA <- get("MAbetweenArrays",envir=limmaGUIenvironment))    
		Try(if (Which=="Both")          MA <- get("MAboth",envir=limmaGUIenvironment))      
  }
  else
    Try(MA <- get("MAimported",envir=limmaGUIenvironment)))

  	Try(FileName <- tclvalue(tkgetSaveFile(initialfile=paste(limmaDataSetNameText,"_A.txt"),filetypes="{{Tab-Delimited Text Files} {.txt}} {{All files} *}")))
	Try(if (!nchar(FileName)) return())
	Try(len <- nchar(FileName))
	if (len<=4)
		Try(FileName <- paste(FileName,".txt",sep=""))
	else if (substring(FileName,len-3,len)!=".txt")
				Try(FileName <- paste(FileName,".txt",sep=""))

	Try(write.table(MA$A,file=FileName,sep="\t",quote=FALSE,col.names=TRUE,row.names=FALSE))
}


WithinBetweenOrBoth <- function(MorA="M")
{
  Try(ttWithinBetweenOrBoth <- tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.title(ttWithinBetweenOrBoth,paste("Export",MorA,"values.")))
  Try(tkwm.deiconify(ttWithinBetweenOrBoth))
  Try(tkgrab.set(ttWithinBetweenOrBoth))
  Try(tkfocus(ttWithinBetweenOrBoth))
  Try(tkgrid(tklabel(ttWithinBetweenOrBoth,text="    ")))
  Try(tkgrid(tklabel(ttWithinBetweenOrBoth,text="    "),tklabel(ttWithinBetweenOrBoth,text=paste("Which",MorA,"values should be exported"),font=.limmaGUIglobals$limmaGUIfont2),tklabel(ttWithinBetweenOrBoth,text="    ")))
  Try(tkgrid(tklabel(ttWithinBetweenOrBoth,text="    "),tklabel(ttWithinBetweenOrBoth,text="as tab-delimited text?",font=.limmaGUIglobals$limmaGUIfont2),tklabel(ttWithinBetweenOrBoth,text="    ")))
  Try(tkgrid(tklabel(ttWithinBetweenOrBoth,text="    ")))
  Try(WithinBetweenOrBothTcl <- tclVar("WithinArrays"))
  Try(frame1 <- tkframe(ttWithinBetweenOrBoth,relief="groove",borderwidth="2"))
  Try(tkgrid(tkradiobutton(frame1,text="Raw (Unnormalized)",variable=WithinBetweenOrBothTcl,value="Raw",font=.limmaGUIglobals$limmaGUIfont2),sticky="w"))
  Try(tkgrid(tkradiobutton(frame1,text="Within-Array Normalized",variable=WithinBetweenOrBothTcl,value="WithinArrays",font=.limmaGUIglobals$limmaGUIfont2),sticky="w"))
  Try(tkgrid(tkradiobutton(frame1,text="Between-Array Normalized",variable=WithinBetweenOrBothTcl,value="BetweenArrays",font=.limmaGUIglobals$limmaGUIfont2),sticky="w"))  
  Try(tkgrid(tkradiobutton(frame1,text="Within-Array and Between-Array Normalized",variable=WithinBetweenOrBothTcl,value="Both",font=.limmaGUIglobals$limmaGUIfont2),sticky="w"))
  Try(tkgrid(tklabel(ttWithinBetweenOrBoth,text="    "),frame1))
  Try(tkgrid(tklabel(ttWithinBetweenOrBoth,text="    ")))
  Try(tkframeOKCancel <- tkframe(ttWithinBetweenOrBoth))
  Try(ReturnVal <- "")
  Try(onOK <- function() { Try(ReturnVal <<- tclvalue(WithinBetweenOrBothTcl)); Try(tkdestroy(ttWithinBetweenOrBoth));Try(tkfocus(.limmaGUIglobals$ttMain))})
  Try(onCancel <- function() { Try(tkdestroy(ttWithinBetweenOrBoth));Try(ReturnVal <- "")})
  Try(OK.but     <- tkbutton(tkframeOKCancel,text="   OK   ",command=onOK,    font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <- tkbutton(tkframeOKCancel,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(tkframeOKCancel,text="    "),columnspan=2))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))  
  Try(tkgrid(tklabel(tkframeOKCancel,text="    "),columnspan=2))
  Try(tkgrid(tklabel(ttWithinBetweenOrBoth,text="    "),tkframeOKCancel))
  
  Try(tkbind(ttWithinBetweenOrBoth, "<Destroy>", function() {Try(tkgrab.release(ttWithinBetweenOrBoth));Try(tkfocus(.limmaGUIglobals$ttMain))}))
  Try(tkwait.window(ttWithinBetweenOrBoth))  
  
  return (ReturnVal)
}

