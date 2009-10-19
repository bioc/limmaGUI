BChelp <- function()
{
	Try(help("backgroundCorrect",help_type="html"))
}

GetBackgroundCorrectionMethod <- function(){
	#define the window ttGetBCMethod, with parent .limmaGUIglobals$ttMain
	Try(ttGetBCMethod<-tktoplevel(.limmaGUIglobals$ttMain))
	Try(tkwm.title(ttGetBCMethod,"Background Correction Method"))
	Try(tkwm.deiconify(ttGetBCMethod))
	Try(tkgrab.set(ttGetBCMethod))
	#define 3 frames
	Try(tkframe1 <- tkframe(ttGetBCMethod,borderwidth=2))
	Try(tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2))
	Try(tkframe4 <- tkframe(tkframe1,borderwidth=2))

	Try(tkgrid(tklabel(tkframe1,text="    ")))
	Try(tkgrid(tklabel(tkframe2,text="Choose a method for background correction.",font=.limmaGUIglobals$limmaGUIfont2),column=2,rowspan=1,columnspan=3,sticky="w"))

	#set BCMethod="subtract" in the environment limmaGUIenvironment if it doesn't already exist there, ie subtract will be the default value
	Try(
		if(!exists("BCMethod",envir=limmaGUIenvironment) ){
			Try(BCMethod <- "subtract")#set to this value if it is not set previously
			Try(assign("BCMethod",BCMethod,limmaGUIenvironment))
		}
	)
	#now get the current value of BCMethod from the limmaGUIenvironment: it may have just been set in the few lines above
	Try(BCMethod <- get("BCMethod",envir=limmaGUIenvironment))
	#define the Tcl variable BCMethodTcl = to current Bacground Correction method
	Try(BCMethodTcl   <- tclVar(BCMethod))
	###DEBUG - display value of a variable
	###TempVal <- tkmessageBox(title="Info",message=paste("BCMethod = ",tclvalue(BCMethodTcl)),icon="info",type="ok")
	#define the buttons
	Try(none.but     <- tkradiobutton(tkframe2,text="None",    variable=BCMethodTcl,value="none",    font=.limmaGUIglobals$limmaGUIfont2))
	Try(subtract.but <- tkradiobutton(tkframe2,text="Subtract",variable=BCMethodTcl,value="subtract",font=.limmaGUIglobals$limmaGUIfont2))
	Try(half.but     <- tkradiobutton(tkframe2,text="Half",    variable=BCMethodTcl,value="half",    font=.limmaGUIglobals$limmaGUIfont2))
	Try(minimum.but  <- tkradiobutton(tkframe2,text="Minimum", variable=BCMethodTcl,value="minimum", font=.limmaGUIglobals$limmaGUIfont2))
	Try(edwards.but  <- tkradiobutton(tkframe2,text="Edwards", variable=BCMethodTcl,value="edwards", font=.limmaGUIglobals$limmaGUIfont2))
	Try(normexp.but  <- tkradiobutton(tkframe2,text="Normexp", variable=BCMethodTcl,value="normexp", font=.limmaGUIglobals$limmaGUIfont2))
	#now grid up all the buttons
	Try(tkgrid(none.but,    column=2))
	Try(tkgrid(subtract.but,column=2))
	Try(tkgrid(half.but,    column=2))
	Try(tkgrid(minimum.but, column=2))
	Try(tkgrid(edwards.but, column=2))
	Try(tkgrid(normexp.but, column=2))
	#configure the grid
	Try(tkgrid.configure(none.but, subtract.but, half.but, minimum.but, edwards.but, normexp.but, sticky="w"))
	Try(tkgrid(tkframe2))
	#set this local function(GetBackgroundCorrectionMethod) variable, NewBCMethod, to be nothing and then the OK function will set it to the users choice
	Try(NewBCMethod   <- "")
	#define the onOK function before the OK button is defined
	onOK <- function()
	{
		#set the function-GetBackgroundCorrectionMethod-variable, NewBCMethod, to be equal to the value chosen by the user from the radio button box
		Try(NewBCMethod   <<- tclvalue(BCMethodTcl));
		Try(tkgrab.release(ttGetBCMethod))#ttGetBCMethod is the window used as the dialog to set the BC Method - release this window
		Try(tkdestroy     (ttGetBCMethod))#destroy the window
		Try(tkfocus       (.limmaGUIglobals$ttMain))#and focus back on the main window
		Try(
			if(NewBCMethod == BCMethod){#if no change in Background method then just return nothing
				return()
			}
		)
		#If it is a differnet value, then store the new value in the function-GetBackgroundCorrectionMethod-variable, BC Method
		Try(BCMethod <<- NewBCMethod)
		#Now also store the BCMmethod in the limmaGUIenvironment
		Try(assign("BCMethod",BCMethod,limmaGUIenvironment))
		# Maybe update the status window!
		#
		#if normexp was chosen, then get normexp offset
		Try(
			if(BCMethod == "normexp"){
				#get or set NEOffset=NEOffsetDefault in the environment limmaGUIenvironment if it doesn't already exist there
				Try(NEOffsetDefault <- get("NEOffsetDefault",envir=limmaGUIenvironment))
				Try(
					if(!exists("NEOffset",envir=limmaGUIenvironment) ){
						Try(NEOffset <- NEOffsetDefault)#set local variable to this value does not exist previously in limmaGUIenvironment
						Try(assign("NEOffset",NEOffset,limmaGUIenvironment))#assign this local value to NEOffset in the limmaGUIenvironment
					}
				)
				#now get the current value of NEOffset from the environment, even if it was just set in the few lines above
				Try(NEOffset <- get("NEOffset",envir=limmaGUIenvironment))
				#
				#define the Tcl variable NEoffsetTcl = to current normexp offset value
				Try(NEoffsetTcl <- tclVar(NEOffset))
				#so ask the user for a value of NEoffsetTcl
				Try(NewNEOffset <- as.numeric(GetNormexpOffsetValue(NEOffset)))
				#set onOK function-variable NEOffset to returned new value
				Try(NEOffset <- NewNEOffset)
				#Now also store the NEOffset in the limmaGUIenvironment
				Try(assign("NEOffset",NEOffset,limmaGUIenvironment))
			}#end of if(NewBCMethod == "normexp")
		)#end of Try
		###DEBUG - display value of a BCMethod
		###Try(BCMethod <- get("BCMethod",envir=limmaGUIenvironment))
		###Try(BCMethodTcl <- tclVar(BCMethod))
		###TempVal <- tkmessageBox(title="Info",message=paste("BCMethod = ",tclvalue(BCMethodTcl)),icon="info",type="ok")
		###Try(tkfocus(.limmaGUIglobals$ttMain))
		###Now show new normexp offset value
		###Try(NEOffset <- get("NEOffset",envir=limmaGUIenvironment))
		###Try(NEOffsetTcl <- tclVar(NEOffset))
		###TempVal <- tkmessageBox(title="Info",message=paste("NEOffset = ",tclvalue(NEOffsetTcl)),icon="info",type="ok")
		###Try(tkfocus(.limmaGUIglobals$ttMain))
	}#end of onOK function
	#now define the OK button and the Cancel and Help buttons
	Try(OK.but     <-tkbutton(tkframe4,text="   OK   ",command=onOK,      font=.limmaGUIglobals$limmaGUIfont2))
	Try(Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=function(){Try(tkgrab.release(ttGetBCMethod));Try(tkdestroy(ttGetBCMethod));NewBCMethod<-"";Try(tkfocus(.limmaGUIglobals$ttMain))},font=.limmaGUIglobals$limmaGUIfont2))
	Try(Help.but   <-tkbutton(tkframe4,text=" Help "  ,command=BChelp,    font=.limmaGUIglobals$limmaGUIfont2))

	Try(tkgrid(tklabel(tkframe4,text="                    ")))
	Try(tkgrid(OK.but, Cancel.but, Help.but))
	Try(tkgrid.configure(OK.but,    sticky="e"))
	Try(tkgrid.configure(Cancel.but,sticky="e"))
	Try(tkgrid.configure(Help.but,  sticky="e"))
	Try(tkgrid(tklabel(tkframe4,text="       ")))
	Try(tkgrid(tkframe4))
	Try(tkgrid(tkframe1))
	Try(tkfocus(OK.but))
	Try(tkbind(ttGetBCMethod, "<Return>",onOK))
	Try(tkbind(ttGetBCMethod, "<Destroy>", function() {Try(tkgrab.release(ttGetBCMethod));Try(tkfocus(.limmaGUIglobals$ttMain))}))
	Try(tkwait.window(ttGetBCMethod))

	Try(tkdestroy(ttGetBCMethod))

	return(NewBCMethod)
}#end of GetBackgroundCorrectionMethod <- function()

GetNormexpOffsetValue <- function(CurrentNormexpOffsetValue)
{
	Try(ttNormexpOffsetGetValueWindow <- tktoplevel(.limmaGUIglobals$ttMain))
	Try(tkwm.deiconify(ttNormexpOffsetGetValueWindow))
	Try(tkgrab.set(    ttNormexpOffsetGetValueWindow))
	Try(tkfocus(       ttNormexpOffsetGetValueWindow))
	Try(tkwm.title(    ttNormexpOffsetGetValueWindow,"Normexp Offset"))
	Try(tkgrid(tklabel(ttNormexpOffsetGetValueWindow,text="    ")))
	Try(tkgrid(tklabel(ttNormexpOffsetGetValueWindow,text="Enter the Normexp Offset",font=.limmaGUIglobals$limmaGUIfont2)))
	Try(tkgrid(tklabel(ttNormexpOffsetGetValueWindow,text="    ")))
	Try(NormexpOffsetTcl <- tclVar(CurrentNormexpOffsetValue))
	Try(entry.NormexpOffset<-tkentry(ttNormexpOffsetGetValueWindow,width="6",         font=.limmaGUIglobals$limmaGUIfont2,textvariable=NormexpOffsetTcl,bg="white"))
	Try(tkgrid(tklabel(ttNormexpOffsetGetValueWindow,text="Normexp Offset Value     ",font=.limmaGUIglobals$limmaGUIfont2),entry.NormexpOffset,sticky="w"))
	ReturnVal <- ""
	onOK <- function()
	{
		Try(NOff <- tclvalue(NormexpOffsetTcl))
		Try(tkgrab.release(ttNormexpOffsetGetValueWindow))
		Try(tkdestroy     (ttNormexpOffsetGetValueWindow))
		Try(tkfocus       (.limmaGUIglobals$ttMain))
		Try(ReturnVal <<- NOff)
	}#end of onOK function
	onCancel   <- function() {
		Try(tkgrab.release(ttNormexpOffsetGetValueWindow));
		Try(tkdestroy(ttNormexpOffsetGetValueWindow));
		Try(tkfocus(.limmaGUIglobals$ttMain));
		Try(ReturnVal <<- "")
	}#end of onCancel
	OK.but     <- tkbutton(ttNormexpOffsetGetValueWindow,text="   OK   ",command=onOK,     font=.limmaGUIglobals$limmaGUIfont2)
	Cancel.but <- tkbutton(ttNormexpOffsetGetValueWindow,text=" Cancel ",command=onCancel, font=.limmaGUIglobals$limmaGUIfont2)
	Try(tkgrid(tklabel(ttNormexpOffsetGetValueWindow,text="    ")))
	Try(tkgrid(OK.but, Cancel.but))
	Try(tkgrid(tklabel(ttNormexpOffsetGetValueWindow,text="    ")))
	Try(tkfocus       (ttNormexpOffsetGetValueWindow))
	Try(tkbind        (ttNormexpOffsetGetValueWindow, "<Destroy>", function() {Try(tkgrab.release(ttNormexpOffsetGetValueWindow));Try(tkfocus(.limmaGUIglobals$ttMain));}))
	Try(tkwait.window (ttNormexpOffsetGetValueWindow))
	#
	return (ReturnVal)
}
