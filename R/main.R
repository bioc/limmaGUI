require(tcltk) || stop ("Cannot find package tcltk")
if (data.class(try(require(limma),TRUE))=="try-error")
{
    tkmessageBox(title="An error has occured!",message=paste("Cannot find package limma"),icon="error",type="ok")
    stop("Cannot find package limma")
} 
if (require(limma)==FALSE)
{
    tkmessageBox(title="An error has occured!",message=paste("Cannot find package limma"),icon="error",type="ok")
    stop("Cannot find package limma")
}


Try <- function(expr) 
{
    require(tcltk)
    if (data.class(result<-try(expr,TRUE))=="try-error")
    {
        tkmessageBox(title="An error has occured!",message=as.character(result),icon="error",type="ok")
    } 
    else 
    {
        return (result)
    }
}

TryReadImgProcFile <- function(expr) 
{
    require(tcltk)
    if (data.class(result<-try(expr,TRUE))=="try-error")
    {
        tkmessageBox(title="Reading Image Processing Files Failed!",
          message="limmaGUI was unable to read the image processing files listed in the Targets file.",icon="error",type="ok")
    } 
    else 
    {
        return (result)
    }
}


Require <- function(pkg) 
{
    require(tcltk)
    if (data.class(result<-try(.find.package(pkg),TRUE))=="try-error")
    {
        tkmessageBox(title="An error has occured!",message=paste("Cannot find package",pkg),icon="error",type="ok")
    } 
    else 
    {
        result <- Try(require(pkg,character.only=TRUE))
    }
    return (result)
}


TclRequire <- function(tclPkg)
{
    require(tcltk)
    if ((data.class(result<-try(tclRequire(tclPkg),TRUE))=="try-error") || (is.logical(result) && result==FALSE))
    {
      Try(winTitle<-"Tcl/Tk Extension(s) Not Found")
      Try(message<-paste("Cannot find Tcl/Tk package \"", tclPkg,
      "\".  Exit limmaGUI (recommended)?\n\n",
      "limmaGUI requires the Tcl/Tk extensions, BWidget and Tktable.  You must have Tcl/Tk installed\n",
      "on your computer, not just the minimal Tcl/Tk installation which comes with R.  If you do have\n",
      "Tcl/Tk installed, including the extensions (e.g. using the ActiveTcl distribution in Windows),\n",
      "make sure that R can find the path to the Tcl library, e.g. C:\\Tcl\\lib (on Windows) or\n",
      "/usr/lib (on Linux/Unix) or /sw/lib on Mac OSX.\n\n",
      "If you don't know how to set environment variables in Windows, one way to make sure the R can\n",
      "find the Tcl/Tk extensions Tktable2.8 and bwidget1.6 is to copy them from your ActiveTcl installation\n",
      "e.g. in C:\\Tcl\\lib into the Tcl subdirectory of your R installation\n\n",
      "If you do understand how to set environment variables...\n",
      "make sure that you have the TCL_LIBRARY environment variable set to the appropriate path, e.g.\n",
      "C:\\Tcl\\lib\\tcl8.4 and the MY_TCLTK environment variable set to a non-empty string, e.g. \"Yes\".\n\n",
      "If using Windows, be sure to read the R for windows FAQ at\nhttp://www.stats.ox.ac.uk/pub/R/rw-FAQ.html\n\n",
      "If your Tcl/Tk extensions still can't be found, try ",
      "addTclPath(\"<path to Tcl library>\").\nThis could be put in $HOME/.Rprofile\n\n",
      "If you need further ",
      "instructions, please contact your system administrator and consider emailing\n ",
      "r-help@stat.math.ethz.ch, or browse through the R-help archives for a similar question.",
      sep=""))
      
      Try(ttTclTkExtension <- tktoplevel(ttMain))
      Try(tkwm.title(ttTclTkExtension,winTitle))
      Try(tkwm.deiconify(ttTclTkExtension))
      Try(tkgrab.set(ttTclTkExtension))
      Try(scr <- tkscrollbar(ttTclTkExtension, repeatinterval=5,
                             command=function(...)tkyview(txt,...)))
      Try(txt <- tktext(ttTclTkExtension,bg="white",yscrollcommand=function(...)tkset(scr,...)))
      Try(tkgrid(txt,scr,columnspan=2))
      Try(tkgrid.configure(scr,columnspan=1,sticky="ns"))
      Try(tkgrid.configure(txt,sticky="nsew"))
      Try(tkinsert(txt,"end",message))
      Try(tkconfigure(txt, state="disabled"))
      Try(tkfocus(txt))
      Try(onYes <- function() 
      {
        Try(tkgrab.release(ttTclTkExtension))
        Try(tkdestroy(ttTclTkExtension))
        try(tkdestroy(ttMain))
        Try(LimmaFileName <- get("LimmaFileName",envir=limmaGUIenvironment))    
        Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
        if (limmaDataSetNameText!="Untitled")
        {
          Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
          Try(mbVal <- tkmessageBox(title="Aborting from limmaGUI",
                message=paste("Save changes to ",LimmaFileName,"?",sep=""),
                icon="question",type="yesno",default="yes"))
          try(if (tclvalue(mbVal)=="yes")
              try(SaveLimmaFile(),silent=TRUE),silent=TRUE)
         }

        quit()                
      })
      Try(onNo <- function()
      {
        Try(tkgrab.release(ttTclTkExtension))
        Try(tkdestroy(ttTclTkExtension))      
      })
      Try(Yes.but <- tkbutton(ttTclTkExtension,text="  Yes  ",command=onYes,font=limmaGUIfont2))  
      Try(No.but <- tkbutton(ttTclTkExtension,text="  No  ",command=onNo,font=limmaGUIfont2)) 
      Try(tkgrid.configure(txt,columnspan=2))
      Try(tkgrid(tklabel(ttTclTkExtension,text="    ")))
      Try(tkgrid(tklabel(ttTclTkExtension,text="Exit limmaGUI (recommended)?",font=limmaGUIfont2),columnspan=2))
      Try(tkgrid(tklabel(ttTclTkExtension,text="    ")))      
      Try(tkgrid(Yes.but,No.but))
      Try(tkgrid.configure(Yes.but,sticky="e"))
      Try(tkgrid.configure(No.but,sticky="w"))      
      Try(tkgrid(tklabel(ttTclTkExtension,text="    ")))    
      Try(tkfocus(Yes.but))
      Try(tkwait.window(ttTclTkExtension))            
    }
}

limmaGUI <- function(BigfontsForlimmaGUIpresentation=FALSE)
{
  assign("limmaGUIenvironment",new.env(),.GlobalEnv)
  assign("Try",get("Try",envir=.GlobalEnv),limmaGUIenvironment)
  # This option is for when I give a Presentation/talk on limmaGUI and want large limmaGUIfonts.  Currently, there are
  # some limmaGUIfonts which limmaGUI can't control, like menus, so as well as changing limmaGUIpresentation to TRUE here, I
  # Right-Click the Windows Desktop, click Properties (to get Display properties which can also be accessed
  # through the Control Panel) then click on Appearance, and then change the limmaGUIfont size for menu,window title, etc.)
  # Rather than change each limmaGUIfont (menu,window title,...) manually each time, I save the changes as a "scheme".  
  Try(if (BigfontsForlimmaGUIpresentation==TRUE)
    Try(assign("limmaGUIpresentation", TRUE, .GlobalEnv))
  else
    Try(assign("limmaGUIpresentation", FALSE, .GlobalEnv)))
  Try(assign("limmaDataSetName",tclVar("Untitled"),.GlobalEnv))
  Try(initGlobals())

  Try(assign("Myhscale", 1, .GlobalEnv))
  Try(assign("Myvscale", 1, .GlobalEnv))

  Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows") 
  {
 
   Try(if (Sys.info()["sysname"]=="Darwin")
   {
     Try(addTclPath("/sw/lib/tcl8.4"))
     Try(addTclPath("/sw/lib"))
     Try(addTclPath("./lib"))
     Try(addTclPath("/sw/lib/tk8.4"))
     Try(addTclPath(paste(Sys.getenv("HOME"),.Platform$file.sep,"TkExtensions",sep="")))
   })    
   Try(addTclPath("/usr/local/lib"))
   Try(addTclPath("/usr/local/Tcl/lib"))
   Try(addTclPath("/usr/local/lib/Tcl"))
   Try(addTclPath("/usr/lib"))
   Try(addTclPath("/usr/lib/Tcl"))
   Try(addTclPath("/usr/local/ActiveTcl/lib"))
   Try(assign("Myhscale", 1, .GlobalEnv))
   Try(assign("Myvscale", 1, .GlobalEnv))
 })

  Try(if (Sys.info()["sysname"] == "Windows") 
  {
    Try(assign("Myhscale",1.6,.GlobalEnv))
    Try(assign("Myvscale",1.6,.GlobalEnv))
  })

  Try(if (Sys.info()["sysname"] == "Darwin" && !exists("X11", env=.GlobalEnv)) 
  {
    Try(addTclPath("/Library/Tcl"))
    Try(addTclPath("/Network/Library/Tcl"))
    Try(addTclPath("/System/Library/Tcl"))
    Try(addTclPath("/Library/Frameworks/Tcl"))
    Try(HOME <- Sys.getenv("HOME"))
    Try(if (nchar(HOME)>0)
    {
      Try(addTclPath(paste(HOME,"/Library/Tcl",sep="")))
      Try(addTclPath(paste(HOME,"/Network/Library/Tcl",sep="")))
      Try(addTclPath(paste(HOME,"/System/Library/Tcl",sep="")))
      Try(addTclPath(paste(HOME,"/Library/Frameworks/Tcl",sep="")))
    })
    Try(assign("Myhscale", 1, .GlobalEnv))
    Try(assign("Myvscale", 1, .GlobalEnv))
  })

  Try(assign("limmaDataSetNameText",get("limmaDataSetNameText",envir=limmaGUIenvironment),.GlobalEnv))

  Try(if (limmaGUIpresentation==TRUE)
    Try(assign("limmaGUIfont1",tkfont.create(family="times",size=48,weight="bold",slant="italic"),.GlobalEnv))
  else
    Try(assign("limmaGUIfont1",tkfont.create(family="times",size=24,weight="bold",slant="italic"),.GlobalEnv)))
  Try(if (limmaGUIpresentation==TRUE)  
    Try(assign("limmaGUIfont2",tkfont.create(family="arial",size=16),.GlobalEnv))
  else
    Try(assign("limmaGUIfont2",tkfont.create(family="arial",size=10),.GlobalEnv)))
  Try(if (limmaGUIpresentation==TRUE)  
    Try(assign("limmaGUIfontTree",tkfont.create(family="arial",size=14),.GlobalEnv))
  else
    Try(assign("limmaGUIfontTree",tkfont.create(family="arial",size=10),.GlobalEnv)))

  Try(if (limmaGUIpresentation==TRUE)  
    Try(assign("limmaGUIfontTable",tkfont.create(family="arial",size=16),.GlobalEnv))
  else
    Try(assign("limmaGUIfontTable",tkfont.create(family="arial",size=10),.GlobalEnv)))
  Try(if (limmaGUIpresentation==TRUE)  
    Try(assign("limmaGUIfontTopTable",tkfont.create(family="arial",size=12,weight="bold"),.GlobalEnv))
  else
    Try(assign("limmaGUIfontTopTable",limmaGUIfontTable,.GlobalEnv)))

  Try(if (limmaGUIpresentation==TRUE)    
    Try(assign("limmaGUIfont2b",tkfont.create(family="arial",size=16,weight="bold"),.GlobalEnv))
  else
    Try(assign("limmaGUIfont2b",tkfont.create(family="arial",size=10,weight="bold"),.GlobalEnv)))

  Try(if (limmaGUIpresentation==TRUE)  
    Try(assign("limmaGUIfontCourier",tkfont.create(family="courier",size=16),.GlobalEnv))
  else
    Try(assign("limmaGUIfontCourier",tkfont.create(family="courier",size=10),.GlobalEnv)))

  Try(assign("mainTreeWidth",30,.GlobalEnv))

  Try(if (limmaGUIpresentation==TRUE)
    Try(assign("ParameterizationTREEWidth",40,.GlobalEnv))
  else
    Try(assign("ParameterizationTREEWidth",30,.GlobalEnv)))

  Try(if (limmaGUIpresentation==TRUE)  
  {
    Try(assign("ParameterizationTREEHeight",20,.GlobalEnv))
    Try(assign("mainTreeHeight",20,.GlobalEnv))
  }
  else
  {
    Try(assign("ParameterizationTREEHeight",15,.GlobalEnv))
    Try(assign("mainTreeHeight",15,.GlobalEnv))  
  })
  # Try(assign("limmaGUIfontMenu",tkfont.create(family="arial",size=10),.GlobalEnv))

  # Try(assign("opar",par(bg="white"),.GlobalEnv))
  Try(assign("oldOptions",options(warn=-1),.GlobalEnv)) # Otherwise R complains that I'm trying to set main in plots, i.e. set a plot title)
# Maybe it would be nice to eventually use the MainFrame widget from BWidget so we can have a nice toolbar etc.
  TclRequire("BWidget")
  Try(assign("ttMain",tktoplevel(),.GlobalEnv))
  Try(if (limmaGUIpresentation==FALSE)
    Try(mainFrame <- tkframe(ttMain,relief="groove",borderwidth="2"))
  else
    Try(mainFrame <- tkframe(ttMain)))    
  Try(if (limmaGUIpresentation==FALSE)
  {
    Try(toolbarFrame <- tkframe(mainFrame,relief="groove",borderwidth="2"))
    Try(tb <- tkframe(toolbarFrame,relief="groove",borderwidth="2"))
    # The Bitmap::get stuff below requires the BWidget package.
    # I think this could be done more simply with something like :
    #   Try(newButton <- tkbutton(tb,image=tkcmd("Bitmap::get","new"),command=NewLimmaFile))
    Try(newButton <- .Tcl(paste("button",.Tk.subwin(tb),"-image [Bitmap::get new]",.Tcl.args(command=NewLimmaFile))))
    Try(openButton <- .Tcl(paste("button",.Tk.subwin(tb),"-image [Bitmap::get open]",.Tcl.args(command=OpenLimmaFile))))
    Try(saveButton <- .Tcl(paste("button",.Tk.subwin(tb),"-image [Bitmap::get save]",.Tcl.args(command=SaveLimmaFile))))  
    Try(tkgrid(newButton,openButton,saveButton,sticky="w"))
    Try(tkgrid(tb,sticky="nw"))
  #  Try(tkgrid(toolbarFrame,sticky="ew"))
    Try(tkgrid(toolbarFrame,sticky="w"))
  #  Try(tkgrid.configure(tb,sticky="w"))
  })
  
  Try(LimmaFileName <- get("LimmaFileName",limmaGUIenvironment))
  Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only 
  Try(tkwm.title(ttMain,paste("LimmaGUI -",LimmaFileName)))
  Try(assign("GALfileBoxTitle",tclVar("Please select a GenePix Array List (GAL) file."),.GlobalEnv))
  Try(assign("GALfileName",tclVar("No filename is selected at the moment.  Press the Select GAL File Button."),.GlobalEnv))
  Try(assign("TargetsfileBoxTitle",tclVar("Please select a tab-delimited file listing the microarray hybridizations."),.GlobalEnv))
  Try(assign("TargetsfileName",tclVar("No filename is selected at the moment.  Press the Select Targets File Button."),.GlobalEnv))
  Try(assign("SpotTypesfileBoxTitle",tclVar("Please select a tab-delimited file listing the spot types."),.GlobalEnv))
  Try(assign("SpotTypesfileName",tclVar("No filename is selected at the moment.  Press the Select Spot-Types File Button."),.GlobalEnv))
   
  Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=3))
  Try(if (limmaGUIpresentation==TRUE)
    Try(tkgrid(tklabel(mainFrame,text="LimmaGUI",font=limmaGUIfont1),column=1,columnspan=3,sticky="ew"))
  else
    Try(tkgrid(tklabel(mainFrame,text="     LimmaGUI ",font=limmaGUIfont1),column=2,sticky="ew")))
  Try(tkgrid(tklabel(mainFrame,text="Welcome to LimmaGUI, a package for Linear Modelling of Microarray Data",font=limmaGUIfont2),columnspan=5))
  Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))
  Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))
  Try(limmaDataSetName.but <- tkbutton(mainFrame,text="Data Set Name",command=GetlimmaDataSetName,font=limmaGUIfont2))
  Try(tkgrid(limmaDataSetName.but,column=2,columnspan=1))
  Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))
  Try(TclRequire("BWidget"))
  Try(mainTreeXScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkxview(mainTree,...),orient="horizontal"))
  Try(mainTreeYScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkyview(mainTree,...)))
  Try(assign("mainTree",tkwidget(mainFrame,"Tree",xscrollcommand=function(...)tkset(mainTreeXScr,...),yscrollcommand=function(...)tkset(mainTreeYScr,...),width=mainTreeWidth,height=mainTreeHeight,bg="white"),.GlobalEnv))
  Try(LinModTreeXScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkxview(ParameterizationTREE,...),orient="horizontal"))
  Try(LinModTreeYScr <- tkscrollbar(mainFrame, repeatinterval=5,command=function(...)tkyview(ParameterizationTREE,...)))
  Try(ParameterizationTREE <- tkwidget(mainFrame,"Tree",xscrollcommand=function(...)tkset(LinModTreeXScr,...),yscrollcommand=function(...)tkset(LinModTreeYScr,...),width=ParameterizationTREEWidth,height=ParameterizationTREEHeight,bg="white"))
  Try(assign("ParameterizationTREE",ParameterizationTREE,.GlobalEnv)) 
  Try(assign("limmaDataSetNameTextLabel",tklabel(mainFrame,text=limmaDataSetNameText,font=limmaGUIfont2b),.GlobalEnv))
  Try(tkgrid(tklabel(mainFrame,text="    "),limmaDataSetNameTextLabel,tklabel(mainFrame,text="    "),tklabel(mainFrame,text="PARAMETERIZATIONS",font=limmaGUIfont2b),tklabel(mainFrame,text="                ")))
  Try(tkgrid(tklabel(mainFrame,text="    "),mainTree,mainTreeYScr,ParameterizationTREE,LinModTreeYScr))
  Try(tkconfigure(limmaDataSetNameTextLabel,textvariable=limmaDataSetName))
  Try(tkgrid.configure(mainTree,rowspan=6,sticky="ns"))
  Try(tkgrid.configure(mainTreeYScr,rowspan=6,sticky="wns"))
  Try(tkgrid.configure(ParameterizationTREE,rowspan=6,sticky="ns"))
  Try(tkgrid.configure(LinModTreeYScr,rowspan=6,sticky="wns"))
  Try(tkgrid(tklabel(mainFrame,text="    "),mainTreeXScr,tklabel(mainFrame,text="    "),LinModTreeXScr))
  Try(tkgrid.configure(mainTreeXScr,sticky="ewn"))
  Try(tkgrid.configure(LinModTreeXScr,sticky="ewn"))

  Try(tkgrid(tklabel(mainFrame,text="         "),columnspan=5))
  
  Try(tkgrid(mainFrame))

  Try(tkinsert(mainTree,"end","root","RG" ,text="R and G",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","RG","RG.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","root","WeightingType" ,text="Spot Quality Weighting",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","WeightingType","WeightingType.Status" ,text="none",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","root","MA" ,text="M and A",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","MA","Raw" ,text="Raw",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","Raw","Raw.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","MA","WithinOnly" ,text="Within-Array Normalized",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","WithinOnly","WithinOnly.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","MA","BetweenOnly" ,text="Between-Array Normalized",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","BetweenOnly","BetweenOnly.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","MA","WithinAndBetween",text="Within and Between-Array Normalized",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","WithinAndBetween","WithinAndBetween.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","root","Layout", text="Layout",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","Layout","Layout.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","root","Parameterizations" ,text="Parameterizations",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","Parameterizations","Parameterizations.Status.1" ,text="None",font=limmaGUIfontTree))


  Try(assign("ArraysLoaded", FALSE,.GlobalEnv))    
  Try(assign("ArraysLoaded",FALSE,limmaGUIenvironment))

  # Menu code below was taken from Rcmdr (and slightly modified)

  Try(etc <- system.file("etc",package="limmaGUI"))
  Try(cat(paste("\nSearching for user-defined limmaGUI commands in",etc,"...\n")))
  Try(source.files <- list.files(etc, pattern="*.R$"))
    Try(for (file in source.files) {
        Try(source(file.path(etc, file)))
        Try(cat(paste("Sourced:", file, "\n")))
        })

  Try(topMenu <- tkmenu(ttMain))
  Try(tkconfigure(ttMain,menu=topMenu))
  Try(Menus <- read.table(file.path(system.file("etc",package="limmaGUI"),"limmaGUI-menus.txt"), as.is=TRUE))
  Try(for (m in 1:nrow(Menus)){
		Try(if (Menus[m, 1] == "menu") assign(Menus[m, 2], tkmenu(eval(parse(text=Menus[m, 3])), tearoff=FALSE)) 
		else if (Menus[m, 1] == "item") {
				if (Menus[m, 3] == "command")
						tkadd(eval(parse(text=Menus[m, 2])),"command", label=Menus[m, 4], command=eval(parse(text=Menus[m, 5])))
				else if (Menus[m, 3] == "cascade")
						tkadd(eval(parse(text=Menus[m, 2])),"cascade", label=Menus[m, 4], menu=eval(parse(text=Menus[m, 5])))
			  else if (Menus[m, 3] == "separator")
			      tkadd(eval(parse(text=Menus[m, 2])),"separator")
				else stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
				}
		else stop(paste("menu defintion error:", Menus[m, ], collapse=" ")))
		})

  Try(assign("mainMenu",topMenu,.GlobalEnv))

  Try(if (limmaGUIpresentation==FALSE)
  {
    Try(labelStatusBar <- tklabel(ttMain,font=limmaGUIfont2))
    Try(tkgrid(labelStatusBar,sticky="w"))
    Try(CurrentStatus <- tclVar("    "))
    Try(tkconfigure(labelStatusBar,textvariable=CurrentStatus))
    Try(tkbind(saveButton,"<Enter>",function() tclvalue(CurrentStatus) <- "Save the current Limma file."))
    Try(tkbind(saveButton,"<Leave>",function() tclvalue(CurrentStatus) <- "    "))
    Try(tkbind(openButton,"<Enter>",function() tclvalue(CurrentStatus) <- "Open an existing Limma file."))
    Try(tkbind(openButton,"<Leave>",function() tclvalue(CurrentStatus) <- "    "))
    Try(tkbind(newButton,"<Enter>",function() tclvalue(CurrentStatus) <- "Start a new Limma analysis."))
    Try(tkbind(newButton,"<Leave>",function() tclvalue(CurrentStatus) <- "    "))
  })
  
  #Try(tkwm.resizable(ttMain,"true","false"))

  Try(onDestroy <- function()
  {
    Try(.JustAskedWhetherToSave <- get(".JustAskedWhetherToSave",envir=.GlobalEnv))    
    Try(if (.JustAskedWhetherToSave==FALSE)
    {
      Try(LimmaFileName <- get("LimmaFileName",envir=limmaGUIenvironment))    
      Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
      if (limmaDataSetNameText!="Untitled")
      {
        Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
        Try(mbVal <- tkmessageBox(title="Aborting from limmaGUI",
              message=paste("Save changes to ",LimmaFileName,"?",sep=""),
              icon="question",type="yesno",default="yes"))
        try(if (tclvalue(mbVal)=="yes")
            try(SaveLimmaFile(),silent=TRUE),silent=TRUE)              
      }
      Try(assign(".JustAskedWhetherToSave",TRUE,.GlobalEnv))
    })
  })    

  Try(tkbind(ttMain, "<Destroy>", onDestroy))
  Try(tkbind(ttMain, "<Control-N>", NewLimmaFile))
  Try(tkbind(ttMain, "<Control-S>", SaveLimmaFile))
  Try(tkbind(ttMain, "<Control-O>", OpenLimmaFile))
  Try(tkbind(ttMain, "<Control-n>", NewLimmaFile))
  Try(tkbind(ttMain, "<Control-s>", SaveLimmaFile))
  Try(tkbind(ttMain, "<Control-o>", OpenLimmaFile))

  Try(tkfocus(ttMain))

  Try(temp <- options(oldOptions))
  invisible()
}

limmaGUIhelp <- function() 
{
    Try(limmaGUIhelpIndex <- file.path(system.file("doc",package="limmaGUI"),"index.html"))
    Try(browseURL(limmaGUIhelpIndex))
    Try(tkmessageBox(title="limmaGUI Help",message=paste("Opening limmaGUI help...\nIf nothing happens, please open :\n",limmaGUIhelpIndex,"\nyourself.",sep="")))
}

limmaHelp <- function() 
{
    Try(limmaHelpIndex <- file.path(system.file("doc",package="limma"),"usersguide.html"))
    Try(browseURL(limmaHelpIndex))
    Try(tkmessageBox(title="limma Help",message=paste("Opening limma help...\nIf nothing happens, please open :\n",limmaHelpIndex,"\nyourself.",sep="")))
}


getPackageVersion <- function(pkgName)
{
  DESCRIPTION <- readLines(paste(system.file(package=pkgName),"/DESCRIPTION",sep=""))
  lineNum <- grep("Version",DESCRIPTION)
  VersionLineWords <- strsplit(DESCRIPTION[lineNum]," ")[[1]]
  numWords <- length(VersionLineWords)
  VersionLineWords[numWords]
}

initGlobals <- function()
{
  assign("limmaGUIVersion",getPackageVersion("limmaGUI"),limmaGUIenvironment)
  assign("limmaVersion",getPackageVersion("limma"),limmaGUIenvironment)  
  assign("LimmaFileName","Untitled",limmaGUIenvironment)
  assign("maLayout",list(),limmaGUIenvironment)
  assign("MA" , list(M=matrix(data=0,nrow=1,ncol=1),A=matrix(data=0,nrow=1,ncol=1)),limmaGUIenvironment)
  assign("MAraw" , list(),limmaGUIenvironment)
  assign("MAwithinArrays" , list(),limmaGUIenvironment)
  assign("MAbetweenArrays" , list(),limmaGUIenvironment)  
  assign("MAboth" , list(),limmaGUIenvironment)    
  assign("RG" , 0,limmaGUIenvironment)
  assign("GALFile" , "",limmaGUIenvironment)
  assign("ParameterizationList" , list(),limmaGUIenvironment)
  assign("gal" , data.frame(),limmaGUIenvironment)
  assign("NumSlides" , 0,limmaGUIenvironment)
  assign("NumParameterizations", 0, limmaGUIenvironment)
  assign("ParameterizationNamesVec", c(), limmaGUIenvironment)
  assign("ParameterizationTreeIndexVec",c(), limmaGUIenvironment)
  assign("NumParameters" , 0,limmaGUIenvironment)
  assign("SlideNamesVec" , c(),limmaGUIenvironment)
  assign("Targets" , data.frame(),limmaGUIenvironment)
  assign("SpotTypes" , data.frame(),limmaGUIenvironment)
  assign("SpotTypeStatus" , c(),limmaGUIenvironment)
  assign("ndups" , 1,limmaGUIenvironment)
  assign("spacing" , 1,limmaGUIenvironment)
  assign("limmaDataSetNameText" , "Untitled",limmaGUIenvironment)
  Try(limmaDataSetName <- get("limmaDataSetName",envir=.GlobalEnv))
  Try(tclvalue(limmaDataSetName) <- "Untitled")
  assign("ArraysLoaded",FALSE,limmaGUIenvironment)
  assign("LinearModelComputed",rep(FALSE,100),limmaGUIenvironment)  # Maximum of 100 parameterizations for now.
  assign("WeightingType","none", limmaGUIenvironment)
  assign("AreaLowerLimit",160, limmaGUIenvironment)
  assign("AreaUpperLimit",170, limmaGUIenvironment)
#  assign("FlagSpotWeighting", 0.1, limmaGUIenvironment)
  assign("MA.Available",list(Raw=FALSE,WithinArrays=FALSE,BetweenArrays=FALSE,Both=FALSE),limmaGUIenvironment)
  assign("RG.Available",FALSE,limmaGUIenvironment)
  assign("Layout.Available",FALSE,limmaGUIenvironment)  
  assign("numConnectedSubGraphs",1,limmaGUIenvironment)
  assign("connectedSubGraphs",list(),limmaGUIenvironment)  
  assign("NumRNATypes",2,limmaGUIenvironment)
  assign("WithinArrayNormalizationMethod","printtiploess",limmaGUIenvironment)
  assign("menuItemNamesVec",c(),.GlobalEnv)
  assign(".JustAskedWhetherToSave",FALSE,.GlobalEnv)
}

deleteItemFromList <- function(list1,itemName=NULL,index=NULL)
{
    if (is.null(index))
      index <- match(itemName,attributes(list1)$names)
    if (is.na(index))
        return(list1)
    len <- length(list1)
    newlist <- list()
    count <- 0
    for (i in (1:len))
        if (i!=index)
        {
            count <- count + 1
            if (!is.null(attributes(list1)$names[i]))
            {
              newlist <- c(newlist,list(foo=list1[[i]]))
              attributes(newlist)$names[count] <- attributes(list1)$names[i]
            }
            else
              newlist[[count]] <- list1[[i]]
        }
    return (newlist)    
}

SetLayoutParameters <- function()
{
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment)) 
  
  if (ArraysLoaded==FALSE)
  {
      Try(tkmessageBox(title="Layout Parameters",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(ttMain))      
      return()
  }
  
  Try(gal <- get("gal",envir=limmaGUIenvironment))
  
  ttLayout<-tktoplevel(ttMain)
  tkwm.deiconify(ttLayout)
  tkgrab.set(ttLayout)
  tkfocus(ttLayout)
  tkwm.title(ttLayout,"Layout Parameters")
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
  if (length(maLayout)==0)
  {
    Try(nspot.r <- tclVar(init="0"))
    Try(nspot.c <- tclVar(init="0"))
    Try(ngrid.r <- tclVar(init="0"))
    Try(ngrid.c <- tclVar(init="0"))
  }
  else
  {
    Try(nspot.r <- tclVar(init=paste(maLayout$nspot.r)))
    Try(nspot.c <- tclVar(init=paste(maLayout$nspot.c)))
    Try(ngrid.r <- tclVar(init=paste(maLayout$ngrid.r)))
    Try(ngrid.c <- tclVar(init=paste(maLayout$ngrid.c)))
  }
  tkgrid(tklabel(ttLayout,text="       "))
  entry.nspot.r <-tkentry(ttLayout,width="12",font=limmaGUIfont2,textvariable=nspot.r,bg="white")
  entry.nspot.c <-tkentry(ttLayout,width="12",font=limmaGUIfont2,textvariable=nspot.c,bg="white")
  entry.ngrid.r <-tkentry(ttLayout,width="12",font=limmaGUIfont2,textvariable=ngrid.r,bg="white")
  entry.ngrid.c <-tkentry(ttLayout,width="12",font=limmaGUIfont2,textvariable=ngrid.c,bg="white")

  GetFromGAL <- function()
  {
    Try(gal <- get("gal",envir=limmaGUIenvironment))
    tmpLayout <- getLayout(gal)
    tclvalue(nspot.r) <- tmpLayout$nspot.r
    tclvalue(nspot.c) <- tmpLayout$nspot.c
    tclvalue(ngrid.r) <- tmpLayout$ngrid.r
    tclvalue(ngrid.c) <- tmpLayout$ngrid.c
  }

  ReturnVal <- 0
  
  onOK <- function(){
        Try(assign("Layout.Available",TRUE,limmaGUIenvironment))
        Try(tkdelete(mainTree,"Layout.Status"))        
        Try(tkinsert(mainTree,"end","Layout","Layout.Status",text="Available",font=limmaGUIfontTree))
        Try(assign("maLayout",list(ngrid.r=as.integer(tclvalue(ngrid.r)), ngrid.c=as.integer(tclvalue(ngrid.c)), nspot.r=as.integer(tclvalue(nspot.r)), nspot.c=as.integer(tclvalue(nspot.c))),limmaGUIenvironment))
        Try(tkgrab.release(ttLayout));Try(tkdestroy(ttLayout));Try(tkfocus(ttMain)); ReturnVal <<- 1}
  onCancel <- function() {Try(tkgrab.release(ttLayout));Try(tkdestroy(ttLayout));Try(tkfocus(ttMain)); ReturnVal <<- 0}        
  OK.but <-tkbutton(ttLayout,text="   OK   ",command=onOK,font=limmaGUIfont2)
  guess.but <-tkbutton(ttLayout,text="Determine from GAL file",command=GetFromGAL,font=limmaGUIfont2)
  Cancel.but <-tkbutton(ttLayout,text=" Cancel ",command=onCancel,font=limmaGUIfont2)

  tkgrid(tklabel(ttLayout,text="Number of rows of blocks",font=limmaGUIfont2),entry.ngrid.r,sticky="w")
  tkgrid(tklabel(ttLayout,text="Number of columns of blocks",font=limmaGUIfont2),entry.ngrid.c,sticky="w")
  tkgrid(tklabel(ttLayout,text="Number of rows per block",font=limmaGUIfont2),entry.nspot.r,sticky="w")
  tkgrid(tklabel(ttLayout,text="Number of columns per block",font=limmaGUIfont2),entry.nspot.c,sticky="w")
  tkgrid(guess.but)
  tkgrid(OK.but,Cancel.but)
  tkgrid(tklabel(ttLayout,text="    "))
  if (length(maLayout)==0 && length(gal)>0)
      GetFromGAL()
  Try(tkfocus(ttLayout))

  Try(tkbind(ttLayout, "<Destroy>", function() {Try(tkgrab.release(ttLayout));Try(tkfocus(ttMain))}))
  Try(tkwait.window(ttLayout))

  return(ReturnVal)
}


# This will be deleted soon, but it will remain a bit longer, just to make sure limmaGUI
# users are using limma 1.2.1 or later.
readGALlimmaGUI <- function(galfile=NULL,path=NULL,header=TRUE,sep="\t",quote="\"",skip=NULL,as.is=TRUE,fill=TRUE,...) {
# Read GenePix Allocation List (GAL) file
# Gordon Smyth
# 1 Mar 2003.  Last revised 16 Sept 2003.

  if(is.null(galfile)) {
    if(is.null(path)) path <- "."
    galfile <- dir(path=path,pattern="*\\.gal$")
    nfiles <- length(galfile)
    if(nfiles == 0) stop("Cannot find GAL file")
    if(nfiles > 1) {
      galfile <- galfile[1]
      warning(paste("More than one GAL file found. Reading",galfile))
    }
  }
  if(!is.null(path)) galfile <- file.path(path,galfile)
  if(is.null(skip)) {
    chunk <- readLines(galfile,n=100)
    skip <- intersect(grep("Name",chunk), grep("ID",chunk)) - 1
    n <- length(skip)
    if(n == 0) stop("Cannot find ID and Name columns in GAL file")
    if(n > 1) stop("Multiple lines with ID and Name labels")
  }
  read.table(galfile,header=header,sep=sep,quote=quote,skip=skip,as.is=as.is,comment.char="",fill=fill,...)
}


OpenGALFile <- function()
{
  Try(tmpGALFile <- tclvalue(tkgetOpenFile(filetypes="{{GAL Files} {.gal}} {{All files} *}")))
  Try(if (!nchar(tmpGALFile)) return())
  Try(assign("GALFile",tmpGALFile,limmaGUIenvironment))
  Try(GALFile <- get("GALFile",envir=limmaGUIenvironment))
  Try(tclvalue(GALfileBoxTitle) <- "GenePix Array List (GAL) File")
  Try(tclvalue(GALfileName) <-paste(GALFile))
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(gal <- readGALlimmaGUI(galfile=GALFile,fill=TRUE))
  Try(gal$ID <- as.character(gal$ID))
  Try(gal$Name <- as.character(gal$Name))  
  Try(assign("gal",gal,limmaGUIenvironment))
  Try(tkconfigure(ttMain,cursor="arrow"))  
  Try(ArraysLoaded <- FALSE)
  Try(assign("ArraysLoaded",ArraysLoaded,limmaGUIenvironment))

  tkfocus(ttMain)
}

OpenTargetsFile <- function()
{
  Try(tmpTargetsFile <- tclvalue(tkgetOpenFile(filetypes="{{Targets Files} {.txt}} {{All files} *}")))
  Try(if (!nchar(tmpTargetsFile)) return())
  Try(TargetsFile <- tmpTargetsFile)
  Try(assign("TargetsFile",TargetsFile,limmaGUIenvironment))
  Try(tclvalue(TargetsfileBoxTitle) <- paste("Targets File"))
  Try(tclvalue(TargetsfileName) <- paste(TargetsFile))
  Try(Targets <- read.table(TargetsFile,header=TRUE,sep="\t",quote="\"",as.is=TRUE))
  Try(if (!("FileName" %in% colnames(Targets)) && !("FileNameCy3" %in% colnames(Targets) && "FileNameCy5" %in% colnames(Targets)))
  {
    Try(tkmessageBox(title="RNA Targets File Error",message="The RNA Targets file should have a \"FileName\" column (or for ImaGene, a \"FileNameCy3\" column and a \"FileNameCy5\" column).",icon="error"))
    Try(tkconfigure(ttMain,cursor="arrow"))
    return()
  })
  Try(if (!("SlideNumber" %in% colnames(Targets)) || !("Cy3" %in% colnames(Targets)) || !("Cy5" %in% colnames(Targets)))
  {
    Try(tkmessageBox(title="RNA Targets File Error",message="The RNA Targets file should have a \"SlideNumber\" column, a \"Cy3\" column and a \"Cy5\" column, where the Cy3 and Cy5 columns list the RNA types for the targets (e.g. \"wild type\" or \"mutant\").",icon="error"))
    Try(tkconfigure(ttMain,cursor="arrow"))
    return()
  })
    
  
  Try(assign("Targets",Targets,limmaGUIenvironment))
  Try(assign("NumSlides",nrow(Targets),limmaGUIenvironment))

  Try(ArraysLoaded <- FALSE)
  Try(assign("ArraysLoaded",ArraysLoaded,limmaGUIenvironment))

  Try(tkfocus(ttMain))
}

OpenSpotTypesFile <- function()
{
  Try(tmpSpotTypesFile <- tclvalue(tkgetOpenFile(filetypes="{{Spot-Type Files} {.txt}} {{All files} *}")))
  Try(if (!nchar(tmpSpotTypesFile)) return())
  Try(SpotTypesFile <- tmpSpotTypesFile)
  Try(assign("SpotTypesFile",SpotTypesFile,limmaGUIenvironment))
  Try(tclvalue(SpotTypesfileBoxTitle) <- paste("Spot-Types File"))
  Try(tclvalue(SpotTypesfileName) <- paste(SpotTypesFile))
  Try(SpotTypes <- read.table(SpotTypesFile,header=TRUE,sep="\t",quote="\"",as.is=TRUE,comment.char=""))
  Try(if (!("SpotType" %in% colnames(SpotTypes))||
      !("ID" %in% colnames(SpotTypes)) ||
      !("Name" %in% colnames(SpotTypes)) ||
      !(("Color" %in% colnames(SpotTypes))||("col" %in% colnames(SpotTypes)))
     )
  {
    Try(tkmessageBox(title="Spot Types",message=paste("Error: SpotTypes table should contain column headings \"SpotType\",",
      "\"ID\",\"Name\" and (\"Color\" or \"col\")."),icon="error"))
    return()
  })

  Try(for (i in (1:ncol(SpotTypes)))
  {
    if (colnames(SpotTypes)[i]=="col")
      colnames(SpotTypes)[i] <- "Color"
    if (colnames(SpotTypes)[i]=="cex")
      colnames(SpotTypes)[i] <- "PointSize"
  })

  Try(if (length(SpotTypes$SpotType)!=length(unique(SpotTypes$SpotType)))
  {
    Try(tkmessageBox(title="Spot Types",message=paste("Error: Each spot type in the SpotType column should be unique",
      "(but one spot type can have multiple spot sub-types, e.g. you could have a spot type \"ratio\" which matches ID's in the GAL file equivalent to \"Ratio_Control_*\", i.e. \"Ratio_Control_1\", \"Ratio_Control_2\", etc.)"),icon="error"))
    return()
  })
  Try(assign("SpotTypes",SpotTypes,limmaGUIenvironment))  
  Try(UpdateSpotTypesStatus())
  Try(ArraysLoaded <- FALSE)
  Try(assign("ArraysLoaded",ArraysLoaded,limmaGUIenvironment))
  Try(tkfocus(ttMain))
}



GetImageProcessingFileType <- function()
{
  ttGetImageProcessingFileType<-tktoplevel(ttMain)
  tkwm.deiconify(ttGetImageProcessingFileType)
  tkgrab.set(ttGetImageProcessingFileType)  
  Try(tkwm.title(ttGetImageProcessingFileType,"Type of Image Processing Files"))
  Try(fileTypeTcl <- tclVar("spot"))
  Try(tkframe1 <- tkframe(ttGetImageProcessingFileType,borderwidth=2))
  Try(tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2))
  Try(tkframe4<-tkframe(tkframe1,borderwidth=2))

  Try(tkgrid(tklabel(tkframe1,text="    ")))

  Try(tkgrid(tklabel(tkframe2,text="Which type of image-processing files are these?",font=limmaGUIfont2),rowspan=1,columnspan=2,sticky="w"))
  Try(Spot.but <- tkradiobutton(tkframe2,text="Spot",variable=fileTypeTcl,value="spot",font=limmaGUIfont2))
  Try(Spot.close.open.but <- tkradiobutton(tkframe2,text="Spot close/open",variable=fileTypeTcl,value="spot.close.open",font=limmaGUIfont2))
  Try(GenePix.but <- tkradiobutton(tkframe2,text="GenePix",variable=fileTypeTcl,value="genepix",font=limmaGUIfont2))
  Try(QuantArray.but <- tkradiobutton(tkframe2,text="QuantArray",variable=fileTypeTcl,value="quantarray",font=limmaGUIfont2))
  Try(ImaGene.but <- tkradiobutton(tkframe2,text="ImaGene",variable=fileTypeTcl,value="imagene",font=limmaGUIfont2))
  Try(ArrayVision.but <- tkradiobutton(tkframe2,text="ArrayVision",variable=fileTypeTcl,value="arrayvision",font=limmaGUIfont2))  
#  Try(SMD.but <- tkradiobutton(tkframe2,text="SMD (Stanford Microarray Database)",variable=fileTypeTcl,value="smd",font=limmaGUIfont2))  
  
  Try(tkgrid(Spot.but))
  Try(tkgrid(Spot.close.open.but))
  Try(tkgrid(GenePix.but))  
  Try(tkgrid(QuantArray.but))    
  Try(tkgrid(ImaGene.but))
  Try(tkgrid(ArrayVision.but))  
#  Try(tkgrid(SMD.but))  
  
  Try(tkgrid.configure(Spot.but,Spot.close.open.but,GenePix.but,QuantArray.but,ImaGene.but,ArrayVision.but,sticky="w"))
#  Try(tkgrid.configure(Spot.but,Spot.close.open.but,GenePix.but,QuantArray.but,ImaGene.but,ArrayVision.but,SMD.but,sticky="w"))
  Try(tkgrid(tkframe2))
  Try(ReturnVal <- "")
  onOK <- function()
  {
      Try(fileTypeVal <- as.character(tclvalue(fileTypeTcl)))
      Try(tkgrab.release(ttGetImageProcessingFileType));Try(tkdestroy(ttGetImageProcessingFileType));Try(tkfocus(ttMain))
      Try(ReturnVal <<- fileTypeVal)
  }
  onCancel <- function(){tkgrab.release(ttGetImageProcessingFileType);tkdestroy(ttGetImageProcessingFileType);tkfocus(ttMain);ReturnVal <<- ""} 
  Try(OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=limmaGUIfont2))
  Try(Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=limmaGUIfont2))
  Try(tkgrid(tklabel(tkframe4,text="                    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="e"))
  Try(tkgrid(tklabel(tkframe4,text="       ")))
  Try(tkgrid(tkframe4))
  Try(tkgrid(tkframe1))
  Try(tkfocus(ttGetImageProcessingFileType))
  Try(tkbind(ttGetImageProcessingFileType, "<Destroy>", function(){tkgrab.release(ttGetImageProcessingFileType);tkfocus(ttMain);} )) 
  Try(tkwait.window(ttGetImageProcessingFileType))
  return (ReturnVal)
}

ReadImageProcessingFiles <- function()
{
  Try(imageFileType <- GetImageProcessingFileType())
  Try(if (nchar(imageFileType)==0) return(0))

  Try(WeightingType     <- get("WeightingType",envir=limmaGUIenvironment))
#  Try(FlagSpotWeighting <- get("FlagSpotWeighting",envir=limmaGUIenvironment))  
  Try(AreaLowerLimit    <- get("AreaLowerLimit",envir=limmaGUIenvironment))
  Try(AreaUpperLimit    <- get("AreaUpperLimit",envir=limmaGUIenvironment))

  Try(if (imageFileType=="spot")
    Try(WhetherToUseBackgroundCorrection <- tclvalue(tkmessageBox(title="Background Correction",message="Use Background Correction (highly recommended) ?",type="yesnocancel",icon="question",default="yes")))
  else
    Try(WhetherToUseBackgroundCorrection <- tclvalue(tkmessageBox(title="Background Correction",message="Use Background Correction?",type="yesnocancel",icon="question",default="yes"))))
  Try(if (WhetherToUseBackgroundCorrection=="cancel")
    return(0))    

  Try(WhetherToUseSpotQualityWeighting <- tkmessageBox(title="Spot Quality Weighting",message="Use Spot Quality Weighting?",type="yesnocancel",icon="question",default="no"))
  Try(WhetherToUseSpotQualityWeighting <- tclvalue(WhetherToUseSpotQualityWeighting))
  Try(if (WhetherToUseSpotQualityWeighting=="cancel")
    return(0))
  Try(if (WhetherToUseSpotQualityWeighting=="yes" && imageFileType!="spot" && imageFileType!="spot.close.open" && imageFileType!="genepix" && imageFileType!="quantarray")
    {
      Try(tkmessageBox(title="Spot Quality Weighting",message="Currently, spot quality weighting is only available for Spot, GenePix and QuantArray files.  Arrays will be processed without spot quality weighting.",icon="warning",type="ok"))
      Try(WhetherToUseSpotQualityWeighting <- "no")    
    })
        
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(tkfocus(ttMain))
  
  Try(WeightingType <- "none")
  Try(assign("WeightingType",WeightingType,limmaGUIenvironment))
  Try(if (WhetherToUseSpotQualityWeighting=="yes")
  {
    if (imageFileType=="spot" || imageFileType=="spot.close.open")
    {
      if (GetWtAreaParams()==0)
      {
        Try(tkconfigure(ttMain,cursor="arrow"))
        return(0)
      }
      Try(AreaLowerLimit    <- get("AreaLowerLimit",envir=limmaGUIenvironment))
      Try(AreaUpperLimit    <- get("AreaUpperLimit",envir=limmaGUIenvironment))
    }
    if (imageFileType=="genepix")        
    {
      Try(GenePixFlagWeightings <- GetGenePixFlagWeightings())
      Try(tkconfigure(ttMain,cursor="watch"))      
      if (length(GenePixFlagWeightings)==0)
      {
        Try(tkconfigure(ttMain,cursor="arrow"))         
        return(0)
      }
#      Try(FlagSpotWeighting <- get("FlagSpotWeighting",envir=limmaGUIenvironment))  
       Try(assign("WeightingType","wtflagsVec",limmaGUIenvironment))
    }
    if (imageFileType=="quantarray")
      Try(assign("WeightingType","wtIgnore.Filter",limmaGUIenvironment))
    if (imageFileType=="imagene")
      Try(assign("WeightingType","none",limmaGUIenvironment))      
    Try(WeightingType <- get("WeightingType",envir=limmaGUIenvironment))        
  })
  Try(TargetsFile <- get("TargetsFile",envir=limmaGUIenvironment))
  Try(if (!nchar(TargetsFile))  
  {
    tkmb <- tkmessageBox(title="ERROR",message="Please select a Targets file first, e.g. SwirlSample.txt",icon="warning",type="ok",default="ok")
    Try(tkconfigure(ttMain,cursor="arrow"))
    return(0)  
  })
  Try(Targets <- read.table(TargetsFile,header=TRUE,sep="\t",quote="\"",as.is=TRUE))
  Try(assign("Targets",Targets,limmaGUIenvironment))
  Try(if (!("FileName" %in% colnames(Targets)) && !("FileNameCy3" %in% colnames(Targets) && "FileNameCy5" %in% colnames(Targets)))
  {
    Try(tkmessageBox(title="RNA Targets File Error",message="The RNA Targets file should have a \"FileName\" column (or for ImaGene, a \"FileNameCy3\" column and a \"FileNameCy5\" column)."))
    Try(tkconfigure(ttMain,cursor="arrow"))
    return(0)
  })
  Try(if (!("FileName" %in% colnames(Targets)) && imageFileType!="imagene")
  {
    Try(tkmessageBox(title="RNA Targets File Error",message="The RNA Targets file should have a \"FileName\" column."))
    Try(tkconfigure(ttMain,cursor="arrow"))
    return(0)
  })  
  Try(if (imageFileType=="imagene" && !("FileNameCy3" %in% colnames(Targets) && "FileNameCy5" %in% colnames(Targets)))
  {
    Try(tkmessageBox(title="RNA Targets File Error",message="When using ImaGene, the RNA Targets file should have a \"FileNameCy3\" column and a \"FileNameCy5\" column."))
    Try(tkconfigure(ttMain,cursor="arrow"))
    return(0)
  })  
  Try(if ("FileName" %in% colnames(Targets))
    Try(slides <- Targets$FileName))
  Try(if ("FileNameCy3" %in% colnames(Targets) && "FileNameCy5" %in% colnames(Targets))
    Try(slides <- cbind(as.matrix(Targets[,"FileNameCy3"]),as.matrix(Targets[,"FileNameCy5"]))))
  Try(SlideNamesVec <- c())
  Try(assign("NumSlides",nrow(Targets),limmaGUIenvironment))
  Try(NumSlides <- get("NumSlides",envir=limmaGUIenvironment))
  Try(for (j in (1:NumSlides))
  {
      if ("Name" %in% colnames(Targets))
          SlideNamesVec[j] <- Targets[j,"Name"]
      else    
          SlideNamesVec[j] <- paste(Targets[j,"SlideNumber"])
  })
  Try(assign("SlideNamesVec",SlideNamesVec,limmaGUIenvironment))
  
  Try(filesExist <- file.exists(slides))
  Try(filesWhichDontExist <- slides[!filesExist])
  Try(if (length(filesWhichDontExist)>0)
    Try(for (i in (1:length(filesWhichDontExist)))
      Try(tkmessageBox(title="Error opening file",message=paste("Failed to open file: \"",filesWhichDontExist[i],"\"",sep=""),icon="error"))))
  Try(if (length(filesWhichDontExist)>0) 
  {
    Try(tkconfigure(ttMain,cursor="arrow"))
    return(0)
  })
  
  Try(if (WhetherToUseSpotQualityWeighting=="yes")
  {
    if (imageFileType=="spot")
        TryReadImgProcFile(RG<-read.maimages(slides,wt.fun=wtarea(ideal=c(AreaLowerLimit,AreaUpperLimit)),source="spot"))
    if (imageFileType=="spot.close.open")
        TryReadImgProcFile(RG<-read.maimages(slides,wt.fun=wtarea(ideal=c(AreaLowerLimit,AreaUpperLimit)),source="spot.close.open"))
    if (imageFileType=="genepix")   
        TryReadImgProcFile(RG<-read.maimages(slides,
        wt.fun=wtflags2(GenePixFlagWeightings),source="genepix"))
    if (imageFileType=="quantarray")
        TryReadImgProcFile(RG<-read.maimages(slides,wt.fun=wtIgnore.Filter,source="quantarray"))
  }
  else
      TryReadImgProcFile(RG<-read.maimages(slides,source=imageFileType)))
  Try(if (WhetherToUseBackgroundCorrection=="no")
    Try(RG <- backgroundCorrect(RG,method="none")))
  Try(assign("RG",RG,limmaGUIenvironment))
  Try(assign("RG.Available",TRUE,limmaGUIenvironment)  )
  Try(tkdelete(mainTree,"RG.Status"))        
  Try(tkinsert(mainTree,"end","RG","RG.Status",text="Available",font=limmaGUIfontTree))
  Try(tkdelete(mainTree,"WeightingType.Status"))
  Try(tkinsert(mainTree,"end","WeightingType","WeightingType.Status" ,text=WeightingType,font=limmaGUIfontTree))

  # Let's automatically computed MAraw:

  Try(RG <- get("RG",envir=limmaGUIenvironment))
  Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
  Try (MAraw <- MA.RG(RG))
  Try(assign("MAraw",MAraw,limmaGUIenvironment))
  Try(assign("MA",MAraw,limmaGUIenvironment))
  Try(MA.Available$Raw <- TRUE)
  Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
  Try(tkdelete(mainTree,"Raw.Status"))
  Try(tkinsert(mainTree,"end","Raw","Raw.Status" ,text="Available",font=limmaGUIfontTree))

  Try(tkconfigure(ttMain,cursor="arrow"))

  Try(ArraysLoaded <- TRUE)    
  Try(assign("ArraysLoaded",ArraysLoaded,limmaGUIenvironment))
  return (1)
}

GetGenePixFlagWeightings <- function()
{
  Try(ttGenePixFlagWeightings <- tktoplevel(ttMain))
  Try(tkwm.deiconify(ttGenePixFlagWeightings))
  Try(tkgrab.set(ttGenePixFlagWeightings))
  Try(tkfocus(ttGenePixFlagWeightings))
  Try(tkwm.title(ttGenePixFlagWeightings,"GenePix Flag Weightings"))
  Try(tkgrid(tklabel(ttGenePixFlagWeightings,text="    ")))
  Try(tkgrid(tklabel(ttGenePixFlagWeightings,text="GenePix Flag Weightings",font=limmaGUIfont2)))
  Try(tkgrid(tklabel(ttGenePixFlagWeightings,text="    ")))
  Try(GoodFlagWeightingTcl <- tclVar("1"))
  Try(entry.GoodFlagWeighting<-tkentry(ttGenePixFlagWeightings,width="20",font=limmaGUIfont2,textvariable=GoodFlagWeightingTcl,bg="white"))
  Try(tkgrid(tklabel(ttGenePixFlagWeightings,text="Good (100)         ",font=limmaGUIfont2),entry.GoodFlagWeighting,sticky="w"))
  Try(BadFlagWeightingTcl <- tclVar("0.1"))
  Try(entry.BadFlagWeighting<-tkentry(ttGenePixFlagWeightings,width="20",font=limmaGUIfont2,textvariable=BadFlagWeightingTcl,bg="white"))
  Try(tkgrid(tklabel(ttGenePixFlagWeightings,text="Bad (-100)         ",font=limmaGUIfont2),entry.BadFlagWeighting,sticky="w"))
  Try(NotFoundFlagWeightingTcl <- tclVar("0.1"))
  Try(entry.NotFoundFlagWeighting<-tkentry(ttGenePixFlagWeightings,width="20",font=limmaGUIfont2,textvariable=NotFoundFlagWeightingTcl,bg="white"))
  Try(tkgrid(tklabel(ttGenePixFlagWeightings,text="Not Found (-50)    ",font=limmaGUIfont2),entry.NotFoundFlagWeighting,sticky="w"))
  Try(AbsentFlagWeightingTcl <- tclVar("0.1"))
  Try(entry.AbsentFlagWeighting<-tkentry(ttGenePixFlagWeightings,width="20",font=limmaGUIfont2,textvariable=AbsentFlagWeightingTcl,bg="white"))
  Try(tkgrid(tklabel(ttGenePixFlagWeightings,text="Absent (-75)       ",font=limmaGUIfont2),entry.AbsentFlagWeighting,sticky="w"))
  Try(UnflaggedFlagWeightingTcl <- tclVar("1"))
  Try(entry.UnflaggedFlagWeighting<-tkentry(ttGenePixFlagWeightings,width="20",font=limmaGUIfont2,textvariable=UnflaggedFlagWeightingTcl,bg="white"))
  Try(tkgrid(tklabel(ttGenePixFlagWeightings,text="Unflagged (0)       ",font=limmaGUIfont2),entry.UnflaggedFlagWeighting,sticky="w"))

  ReturnVal <- list()
  
  onOK <- function()
  {
    Try(Good     <- as.numeric(tclvalue(GoodFlagWeightingTcl)))
    Try(Bad      <- as.numeric(tclvalue(BadFlagWeightingTcl)))
    Try(NotFound <- as.numeric(tclvalue(NotFoundFlagWeightingTcl)))
    Try(Absent   <- as.numeric(tclvalue(AbsentFlagWeightingTcl)))
    Try(Unflagged<- as.numeric(tclvalue(UnflaggedFlagWeightingTcl)))
    Try(tkgrab.release(ttGenePixFlagWeightings))
    Try(tkdestroy(ttGenePixFlagWeightings))
    Try(tkfocus(ttMain))
    Try(ReturnVal <<- list(Good=Good,Bad=Bad,NotFound=NotFound,Absent=Absent,Unflagged=Unflagged))
  }
  onCancel <- function() {Try(tkgrab.release(ttGenePixFlagWeightings));Try(tkdestroy(ttGenePixFlagWeightings));Try(tkfocus(ttMain));Try(ReturnVal <<- list())}
  OK.but <- tkbutton(ttGenePixFlagWeightings,text="   OK   ",command=onOK,font=limmaGUIfont2)
  Cancel.but <- tkbutton(ttGenePixFlagWeightings,text=" Cancel ",command=onCancel,font=limmaGUIfont2)
  Try(tkgrid(tklabel(ttGenePixFlagWeightings,text="    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid(tklabel(ttGenePixFlagWeightings,text="    ")))
  Try(tkfocus(ttGenePixFlagWeightings))
  Try(tkbind(ttGenePixFlagWeightings, "<Destroy>", function() {Try(tkgrab.release(ttGenePixFlagWeightings));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttGenePixFlagWeightings))

  return (ReturnVal)
}

wtflags2 <- function(Weightings)
function(gpr)
{
  w <- rep(1,nrow(gpr))
  w[gpr[,"Flags"]==100]  <- Weightings$Good
  w[gpr[,"Flags"]==-100] <- Weightings$Bad
  w[gpr[,"Flags"]==-50]  <- Weightings$NotFound
  w[gpr[,"Flags"]==-75]  <- Weightings$Absent
  w[gpr[,"Flags"]==0]    <- Weightings$Unflagged
  w
}



tclArrayVar <- function()
{
    Try(n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env))
    Try(name <- paste("::RTcl", n,sep = ""))
    Try(l <- list(env = new.env()))
    Try(assign(name, NULL, envir = l$env))
    Try(reg.finalizer(l$env, function(env) tkcmd("unset", ls(env))))
    Try(class(l) <- "tclArrayVar")
    Try(.Tcl(paste("set ",name,"(0,0) \"\"",sep="")))
    l
}


GetDesignOrContrasts <- function(Design=FALSE,Contrasts=FALSE,NumContrasts=0,
  parameterizationIndex=0) # parameterizationIndex argument is for contrasts only
{ 
  Try(if ((Design==TRUE && Contrasts==TRUE) || (Design==FALSE && Contrasts==FALSE))
    tkmessageBox(title="GetDesignOrContrasts",message="Error: Only one of DesignOrContrasts and Contrasts should be set to TRUE",icon="error"))    

  Try(NumSlides     <- get("NumSlides",    envir=limmaGUIenvironment))
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))  
  Try(SlideNamesVec <- get("SlideNamesVec",envir=limmaGUIenvironment))  
  Try(Targets       <- get("Targets",      envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment)) 
  
  Try(if (Contrasts==TRUE)
    ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  
  if (ArraysLoaded==FALSE)
  {
      if (Design==TRUE)
        tkmessageBox(title="Design Matrix",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error")
      else
        tkmessageBox(title="Contrasts Matrix",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error")      
      Try(tkfocus(ttMain))      
      return()
  }

  GetDesignOrContrastsTable <- function(designOrContrastsFromDropDowns)
  {

    Try(TclRequire("Tktable"))
    Try(ttDesignOrContrastsTable <- tktoplevel(ttMain))
    Try(tkwm.deiconify(ttDesignOrContrastsTable))
    Try(tkgrab.set(ttDesignOrContrastsTable))
    Try(tkfocus(ttDesignOrContrastsTable))
    if (Design==TRUE)
      Try(tkwm.title(ttDesignOrContrastsTable,"Design Matrix"))
    else
      Try(tkwm.title(ttDesignOrContrastsTable,"Contrasts Matrix"))
    if (Design==TRUE)
    {
      Try(ReturnVal <- list(design=data.frame(),designCreatedFromDropDowns=FALSE))
      Try(designOrContrasts <- designOrContrastsFromDropDowns$design)
    }
    else
    {
      Try(ReturnVal <- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=FALSE))
      Try(designOrContrasts <- designOrContrastsFromDropDowns$contrasts)
    }
    
#   Try(n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env))
#   Try(tclArrayName <- paste("::RTcl", n, sep = ""))
    Try(tclArrayVar1 <- tclArrayVar())
    Try(tclArrayName <- ls(tclArrayVar1$env))

    onOK <- function()
    {      
        Try(.Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<Leave>"))))
        if (Design==TRUE)
        {
          NumRows <- NumSlides
          NumCols <- NumParameters
        }
        else
        {
          NumRows <- NumParameters
          NumCols <- NumContrasts
        }
        Try(designOrContrasts <- as.data.frame(matrix(nrow=NumRows,ncol=NumCols)))
        Try(rownamesDesignOrContrasts <- c())
        for (i in (1:NumRows))
            Try(rownamesDesignOrContrasts[i] <- tclvalue(paste(tclArrayName,"(",i,",0)",sep="")))
        Try(colnamesDesignOrContrasts <- c())
        if (NumCols>0)
          for (j in (1:NumCols))
            Try(colnamesDesignOrContrasts[j] <- tclvalue(paste(tclArrayName,"(0,",j,")",sep="")))
        Try(rownames(designOrContrasts) <- rownamesDesignOrContrasts)
        Try(colnames(designOrContrasts) <- colnamesDesignOrContrasts)
        if (Design==TRUE)
        {
          Try(assign("SlideNamesVec",rownamesDesignOrContrasts,limmaGUIenvironment))
          Try(SlideNamesVec <- get("SlideNamesVec",envir=limmaGUIenvironment))
        }
        if (NumCols>0)
          for (i in (1:NumRows))
            for (j in (1:NumCols))
                Try(designOrContrasts[i,j] <- as.numeric(tclvalue(paste(tclArrayName,"(",i,",",j,")",sep=""))))
        Try(tkgrab.release(ttDesignOrContrastsTable))
        Try(tkdestroy(ttDesignOrContrastsTable))
        Try(tkfocus(ttMain))
        if (Design==TRUE)
          Try(ReturnVal <<- list(design=designOrContrasts,designCreatedFromDropDowns=FALSE))
        else
          Try(ReturnVal <<- list(contrasts=designOrContrasts,contrastsCreatedFromDropDowns=FALSE))        
    }
    onCancel <- function() 
    {
      Try(tkgrab.release(ttDesignOrContrastsTable))
      Try(tkdestroy(ttDesignOrContrastsTable))
      Try(tkfocus(ttMain))
      if (Design==TRUE) 
        ReturnVal <<- list(design=data.frame(),designCreatedFromDropDowns=FALSE)
      else
        ReturnVal <<- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=FALSE)
    }       
    Try(OK.but <-tkbutton(ttDesignOrContrastsTable,text="   OK   ",command=onOK,font=limmaGUIfont2))
    Try(Cancel.but <-tkbutton(ttDesignOrContrastsTable,text=" Cancel ",command=onCancel,font=limmaGUIfont2))
    Try(tkgrid(tklabel(ttDesignOrContrastsTable,text="    ")))
    if (Design==TRUE)
      Try(PleaseEnterDesignOrContrastsLabel<-tklabel(ttDesignOrContrastsTable,text="Please enter the design matrix to be used for linear-modelling.",font=limmaGUIfont2))
    else
      Try(PleaseEnterDesignOrContrastsLabel<-tklabel(ttDesignOrContrastsTable,text="Please enter the contrasts matrix to be used for linear-modelling.",font=limmaGUIfont2))      
    Try(tkgrid(tklabel(ttDesignOrContrastsTable,text="    "),PleaseEnterDesignOrContrastsLabel))
    Try(tkgrid.configure(PleaseEnterDesignOrContrastsLabel,columnspan=2))
    Try(tkgrid(tklabel(ttDesignOrContrastsTable,text="    ")))
    if (Design==TRUE)
    {
      NumRows <- NumSlides
      NumCols <- NumParameters
    }
    else
    {
      NumRows <- NumParameters
      NumCols <- NumContrasts
    }
    
    if (Design==TRUE)
    {
      Try(if (nrow(designOrContrasts)==0)
      {
        Try(ParameterNamesVec <- c())
        if (NumParameters>0)
          for (i in (1:NumParameters)) 
            Try(ParameterNamesVec <- c(ParameterNamesVec,paste("Param ",i,sep="")))
      }
      else
          Try(ParameterNamesVec <- colnames(designOrContrasts)))
      Try(ColNamesVec <- ParameterNamesVec)
    }
    else
    {
      Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
      Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))          
      Try(if (nrow(designOrContrasts)==0)
      {
        Try(ContrastsNamesVec <- c())
        if (NumContrasts>0)
          for (i in (1:NumContrasts)) 
            Try(ContrastsNamesVec <- c(ContrastsNamesVec,paste("Contrast ",i,sep="")))              
      }
      else
          Try(ContrastsNamesVec <- colnames(designOrContrasts)))      
      Try(ColNamesVec <- ContrastsNamesVec)
    }

    
    Try(rownamesDesignOrContrasts <- c())
    Try(myRarray <- "    ")
    for (i in (1:NumRows))
    {
        if (Design==TRUE)
          Try(RowName <- SlideNamesVec[i])
        else
          Try(RowName <- ParameterNamesVec[i])
        Try(rownamesDesignOrContrasts <- c(rownamesDesignOrContrasts,RowName))
        Try(myRarray <- c(myRarray,paste(RowName)))
    }
    if (NumCols>0)
      for (j in (1:NumCols))      
      {
        Try(myRarray <- c(myRarray,paste(ColNamesVec[j])))
        for (i in (1:NumRows))
        {          
            if (nrow(designOrContrasts)==0)
                Try(myRarray <- c(myRarray,"0"))
            else
                Try(myRarray <- c(myRarray,paste(designOrContrasts[i,j])))
        }      
      } 
      # This will give an error if tclArray doesn't exist.
      # .Tcl("unset tclArray")
      Try(dim(myRarray) <- c(NumRows+1,NumCols+1))
      if (NumCols>0)
        for (i in (0:NumRows))
          for (j in (0:NumCols))
          {
             # Modified to use tkcmd!
             Try(tkcmd("set",paste(tclArrayName,"(",i,",",j,")",sep=""),paste(myRarray[i+1,j+1])))             
          }

      # Below, can I just use tkwidget(ttDesignOrContrastsTable,"table",...) ?
      Try(table1 <- .Tk.subwin(ttDesignOrContrastsTable))
      Try(.Tcl(paste("table",.Tk.ID(table1),.Tcl.args(variable=tclArrayName,rows=paste(NumRows+1),cols=paste(NumCols+1),titlerows="0",titlecols="0",selectmode="extended",colwidth="13",background="white",rowseparator="\"\n\"",colseparator="\"\t\"",resizeborders="col",multiline="0"))))
      Try(tkgrid(tklabel(ttDesignOrContrastsTable,text="    "),table1))

      Try(tkcmd(.Tk.ID(table1),"width","0",paste(max(4,max(nchar(rownamesDesignOrContrasts))+2))))
      Try(if (nrow(designOrContrasts)>0)
      {     

        Try(for (j in (1:NumCols))      
          Try(tkcmd(.Tk.ID(table1),"width",paste(j),paste(max(4,max(nchar(ColNamesVec))+2,max(nchar(designOrContrasts[,j]))+2)))))
      })

#       if (Contrasts==TRUE)
#         Try(tkcmd(.Tk.ID(table1),"width","0","25"))

      Try(tkconfigure(table1,font=limmaGUIfontTable))
      Try(tkgrid.configure(table1,columnspan=2))

      Try(copyFcn <-      function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<<Copy>>"))))

      openDesignOrContrastsMatrixFile <- function()
      {
        Try(if (Design==TRUE)
          Try(DesignOrContrastsFileName <- tclvalue(tkgetOpenFile(filetypes="{{Design Matrix Files} {.txt}} {{All files} *}")))
        else
          Try(DesignOrContrastsFileName <- tclvalue(tkgetOpenFile(filetypes="{{Contrasts Matrix Files} {.txt}} {{All files} *}"))))
        Try(if (!nchar(DesignOrContrastsFileName)) return())
        Try(DesignOrContrastsTable <- read.table(DesignOrContrastsFileName,header=FALSE,sep="\t",quote="\"",as.is=TRUE))
        # This will give an error if tclArray doesn't exist.
        # .Tcl("unset tclArray")
        if (NumCols>0)
          for (i in (0:NumRows))
            for (j in (0:NumCols))
#             Try(.Tcl(paste("set ",tclArrayName,"(",i,",",j,") \"",DesignOrContrastsTable[i+1,j+1],"\"",sep="")))
              Try(tkcmd("set",paste(tclArrayName,"(",i,",",j,")",sep=""),paste(DesignOrContrastsTable[i+1,j+1])))
      }

      saveDesignOrContrastsMatrixFile <- function()
      {
        Try(DesignOrContrastsFileName <- tclvalue(tkgetSaveFile(filetypes="{{DesignOrContrasts Matrix Files} {.txt}} {{All files} *}")))
        Try(if (!nchar(DesignOrContrastsFileName)) return())
        Try(len <- nchar(DesignOrContrastsFileName))
        if (len<=4)
          Try(  DesignOrContrastsFileName <- paste(DesignOrContrastsFileName,".txt",sep=""))
        else if (substring(DesignOrContrastsFileName,len-3,len)!=".txt")
              Try(DesignOrContrastsFileName <- paste(DesignOrContrastsFileName,".txt",sep=""))
        Try(designOrContrasts <- as.data.frame(matrix(nrow=NumSlides,ncol=NumParameters)))
        Try(rownamesDesignOrContrasts <- c())
        Try(for (i in (1:NumRows))
            rownamesDesignOrContrasts[i] <- tclvalue(paste(tclArrayName,"(",i,",0)",sep="")))
        Try(colnamesDesignOrContrasts <- c())
        if (NumParameters>0)
          Try(for (j in (1:NumCols))
            colnamesDesignOrContrasts[j] <- tclvalue(paste(tclArrayName,"(0,",j,")",sep="")))
        Try(rownames(designOrContrasts) <- rownamesDesignOrContrasts)
        Try(colnames(designOrContrasts) <- colnamesDesignOrContrasts)
        if (NumParameters>0)
          Try(for (i in (1:NumRows))
            for (j in (1:NumParameters))
                designOrContrasts[i,j] <- as.numeric(tclvalue(paste(tclArrayName,"(",i,",",j,")",sep=""))))

        Try(write.table(designOrContrasts,file=DesignOrContrastsFileName,col.names=NA,sep="\t",quote=FALSE,row.names=TRUE))
      }

      Try(topMenu <- tkmenu(ttDesignOrContrastsTable, tearoff=FALSE))
      Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
      Try(tkadd(fileMenu, "command", label="Open",      command=openDesignOrContrastsMatrixFile)) # ) # ,font=limmaGUIfontMenu))
      Try(tkadd(fileMenu, "command", label="Save As",      command=saveDesignOrContrastsMatrixFile)) # ) # ,font=limmaGUIfontMenu))
      Try(tkadd(topMenu,  "cascade", label="File",menu=fileMenu)) # ) # ,font=limmaGUIfontMenu))  

      Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
      Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",      command=copyFcn)) # ) # ,font=limmaGUIfontMenu))
      Try(tkadd(topMenu,  "cascade", label="Edit",menu=editMenu)) # ) # ,font=limmaGUIfontMenu))  

      Try(tkconfigure(ttDesignOrContrastsTable,menu=topMenu))

      Try(BlankLabel1<-tklabel(ttDesignOrContrastsTable,text="    "))
      Try(tkgrid(BlankLabel1))
      Try(BlankLabel2<-tklabel(ttDesignOrContrastsTable,text="    "))
      Try(tkgrid(BlankLabel2,OK.but,Cancel.but))
      Try(tkgrid.configure(OK.but,sticky="e"))
      Try(tkgrid.configure(Cancel.but,sticky="w"))
      Try(BlankLabel3<-tklabel(ttDesignOrContrastsTable,text="    "))
      Try(tkgrid(BlankLabel3))

      Try(tkfocus(ttDesignOrContrastsTable))
      Try(tkbind(ttDesignOrContrastsTable, "<Destroy>", function() {Try(tkgrab.release(ttDesignOrContrastsTable));Try(tkfocus(ttMain));}))
      Try(tkwait.window(ttDesignOrContrastsTable))
      return (ReturnVal)
  }

  if (Design==TRUE)
  {
    # We will try to determine the number of parameters to be estimated (one less than
    # the number of RNA targets).  For now, we'll assume column
    # names are Cy3 and Cy5, but later we can allow for Red, Green, RED, GREEN, CY5, CY3.

    Try(Cy3Targets <- as.vector(Targets["Cy3"])$Cy3)
    Try(Cy5Targets <- as.vector(Targets["Cy5"])$Cy5)

    Cy3Copy <- Cy3Targets
    Cy5Copy <- Cy5Targets

  #  Target <- Cy3Targets[1]
    TargetVector <- c(Cy3Targets[1])
    TargetsCounted <- 1
    for (i in (1:length(Cy3Targets)))
    {
      for (j in (1:length(TargetVector)))
      {
          if (Cy3Targets[i]==TargetVector[j])
            Cy3Copy[i]<-"Counted"
      }
      if (Cy3Copy[i]!="Counted")
      {
        TargetVector <- c(TargetVector,Cy3Targets[i])
        TargetsCounted <- TargetsCounted + 1
      }
    }
    for (i in (1:length(Cy5Targets)))
    {
      for (j in (1:length(TargetVector)))
      {
        if (Cy5Targets[i]==TargetVector[j])
          Cy5Copy[i]<-"Counted"
      }
      if (Cy5Copy[i]!="Counted")
      {
        TargetVector <- c(TargetVector,Cy5Targets[i])
        TargetsCounted <- TargetsCounted + 1
      }
    }

  # NumParameters should be OK as a global, because the number of parameters is the same, no matter what the 
  # parametrization (unless we start dealing with Scorecard Controls, but they would need a whole new Targets file).

  # We now allow for unconnected designOrContrastss.
  # The "graph" below is a network graph, not at plot.

    RNATypesGraph <- list()
    Try(for (i in (1:NumSlides))
      RNATypesGraph[[i]] <- c(Cy3Targets[i],Cy5Targets[i]))
    countConnectedSubGraphs <- function(graph)  
    {
      count <- 0  
      connectedSubGraphs <- list()
      while (length(graph))
      {
        open <- graph[[1]][1]
        closed <- c()
        while (length(open))
        {
          test <- open[1]
          if (length(open)>1)
            open <- open[2:length(open)]
          else
            open <- c()
          g <- 1
          if (!(test %in% closed))
          {
            closed <- c(closed,test)
            while (g <= length(graph))
              if (test %in% graph[[g]])
              {  
                n <- graph[[g]]
                open <- c(open,n[1],n[2])
                graph <- deleteItemFromList(graph,index=g)                
              }            
              else
                g <- g + 1
          }  
        }
        connectedSubGraphs <- c(connectedSubGraphs,list(closed))
        count <- count + 1
      }
      return (list(count=count,connectedSubGraphs=connectedSubGraphs))
    }

    Try(result <- countConnectedSubGraphs(RNATypesGraph))
    Try(numConnectedSubGraphs <- result$count)
    Try(connectedSubGraphs <- result$connectedSubGraphs)

    Try(assign("numConnectedSubGraphs",numConnectedSubGraphs,limmaGUIenvironment))
    Try(assign("connectedSubGraphs",connectedSubGraphs,limmaGUIenvironment))

    Try(assign("NumParameters", TargetsCounted - numConnectedSubGraphs,limmaGUIenvironment))
    Try(assign("NumRNATypes", TargetsCounted,limmaGUIenvironment))
    Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))    
    ParameterNamesVec <- c()
    if (NumParameters>0)
      for (i in (1:NumParameters))
        ParameterNamesVec <- c(ParameterNamesVec,paste("Param",i))
  }

  if (NumParameters<=0)
  {
    Try(tkmessageBox(title="At Least Two RNA Types Are Required",message="You must have at least two types of RNA in your Targets file.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    if (Design==TRUE)
      return(list(design=data.frame(),designCreatedFromDropDowns=FALSE))
    else
      return(list(contrasts=data.frame(),contrastsCreatedFromDropDowns=FALSE))    
  }
  
  if (Design==TRUE)
  {
    NumRows <- NumSlides
    NumCols <- NumParameters
  }
  else
  {
    NumRows <- NumParameters
    NumCols <- NumContrasts
  }

  ttDesignOrContrasts<-tktoplevel(ttMain)
  tkwm.deiconify(ttDesignOrContrasts)
  tkgrab.set(ttDesignOrContrasts)
  tkfocus(ttDesignOrContrasts)
  if (Design==TRUE)
    tkwm.title(ttDesignOrContrasts,"Parameters To Estimate")
  else
    tkwm.title(ttDesignOrContrasts,"Contrasts")  

  if (Design==TRUE)
    lbl2<-tklabel(ttDesignOrContrasts,text="Please specify pairs of RNA for which M parameters will be estimated",font=limmaGUIfont2)
  else
    lbl2<-tklabel(ttDesignOrContrasts,text="Please specify pairs of parameters for which contrasts will be estimated",font=limmaGUIfont2)  
  lbl3<-tklabel(ttDesignOrContrasts,text="                                                                    ")
  tkgrid(tklabel(ttDesignOrContrasts,text="       "),row=0,column=1,columnspan=1)
  tkgrid(tklabel(ttDesignOrContrasts,text="       "),row=0,column=4,columnspan=1)
  tkgrid(lbl2,row=1,column=2,columnspan=4,rowspan=1,sticky="ew");
  tkgrid.configure(lbl2,sticky="w")
  tkgrid(tklabel(ttDesignOrContrasts,text="         "),column=1)
  tkgrid(tklabel(ttDesignOrContrasts,text="         "))
  tkgrid(tklabel(ttDesignOrContrasts,text="         "),column=1)
#  plus<-tklabel(ttDesignOrContrasts,text="   +   ",font=limmaGUIfont2)
#  minus<-tklabel(ttDesignOrContrasts,text="   -   ",font=limmaGUIfont2)
#  tkgrid(plus,row=3, column=2,sticky="ew")
#  tkgrid(minus,row=3,column=6,sticky="ew")

  if (Contrasts==TRUE)
  {
    Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
    Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))          
  }

  Try(if (Design==TRUE)
  {
    TclList1AsString <- "{"
    for (i in (1:TargetsCounted))
      TclList1AsString <- paste(TclList1AsString,"{",TargetVector[i],"} ",sep="")
    TclList1AsString <- paste(TclList1AsString,"}",sep="")
    TclList2AsString <- TclList1AsString
  }
  else
  {
    TclList1AsString <- "{"
    for (i in (1:NumParameters))
      TclList1AsString <- paste(TclList1AsString,"{",ParameterNamesVec[i],"} ",sep="")
    TclList1AsString <- paste(TclList1AsString,"}",sep="")
    TclList2AsString <- TclList1AsString
  })
  
  Try(if (Design==TRUE) 
    plusOrMinusTclListAsString <- "{{minus}}"
  else
    plusOrMinusTclListAsString <- "{{minus} {plus}}")

  Try(TclRequire("BWidget"))      

  combo1 <- c()
  combo2 <- c()
  combo3 <- c()    
  Try(if (NumCols>0)
    for (paramORcontrast in (1:NumCols))
    {
      Try(FirstDropDownColumn <- .Tk.subwin(ttDesignOrContrasts))
      combo1 <- c(combo1,FirstDropDownColumn)
      Try(.Tcl(paste("ComboBox",.Tk.ID(FirstDropDownColumn),"-editable false -values",TclList1AsString)))
      Try(SecondDropDownColumn <- .Tk.subwin(ttDesignOrContrasts))
      Try(combo2 <- c(combo2,SecondDropDownColumn))
      Try(.Tcl(paste("ComboBox",.Tk.ID(SecondDropDownColumn),"-editable false -values",TclList2AsString)))
      Try(plusOrMinusDropDown <- .Tk.subwin(ttDesignOrContrasts))
      Try(combo3 <- c(combo3,plusOrMinusDropDown))
      Try(.Tcl(paste("ComboBox",.Tk.ID(plusOrMinusDropDown),"-editable false -values",plusOrMinusTclListAsString)))
      Try(tkcmd(.Tk.ID(plusOrMinusDropDown),"setvalue","first"))
      Try(if (limmaGUIpresentation==TRUE)            
      {
        Try(tkconfigure(FirstDropDownColumn,width=10))
        Try(tkconfigure(SecondDropDownColumn,width=10))      
        Try(tkconfigure(plusOrMinusDropDown,width=10))              
      })
      if (Design==TRUE)
        Try(dropdownLabel <- paste("Parameter",paramORcontrast,"  "))
      else
        Try(dropdownLabel <- paste("Contrast",paramORcontrast, "  ")  )
      
      Try(tkgrid(tklabel(ttDesignOrContrasts,text=dropdownLabel,font=limmaGUIfont2),row=2+paramORcontrast,
                        column=0,sticky="w"))
      Try(tkconfigure(FirstDropDownColumn,font=limmaGUIfont2))
      Try(tkconfigure(SecondDropDownColumn,font=limmaGUIfont2))
      Try(tkconfigure(plusOrMinusDropDown,font=limmaGUIfont2))      
      Try(tkgrid(FirstDropDownColumn,row=2+paramORcontrast,column=2,columnspan=1,rowspan=1))
      Try(tkgrid(plusOrMinusDropDown,row=2+paramORcontrast,column=4,columnspan=1,rowspan=1))      
      Try(tkgrid(SecondDropDownColumn,row=2+paramORcontrast,column=6,columnspan=1,rowspan=1))

      Try(tkgrid(tklabel(ttDesignOrContrasts,text="    "),row=2+paramORcontrast,column=7))
    })
  tkgrid(tklabel(ttDesignOrContrasts,text="                                                 "),rowspan=1,columnspan=4);

  if (Design==TRUE)
    ReturnVal <- list(design=data.frame(),designCreatedFromDropDowns=TRUE,TargetVector=TargetVector,RNAType1=c(),RNAType2=c(),plusOrMinus=c())
  else
    ReturnVal <- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,TargetVector=c(),Param1=c(),Param2=c(),plusOrMinus=c())  
  
  OnAdvanced <- function()
  {
      Try(designOrContrastsFromDropDowns <- GetDesignOrContrastsFromDropDowns())
      Try(ReturnValDesignOrContrastsTable <- GetDesignOrContrastsTable(designOrContrastsFromDropDowns)) # Returns designOrContrasts list object including designOrContrasts matrix as data.frame
      if (Design==TRUE)
        NumRows <- nrow(ReturnValDesignOrContrastsTable$design)
      else
        NumRows <- nrow(ReturnValDesignOrContrastsTable$contrasts)
      if (NumRows>0 ) # OK was clicked, not Cancel
      {
          Try(tkgrab.release(ttDesignOrContrasts))
          Try(tkdestroy(ttDesignOrContrasts))
          Try(tkfocus(ttMain))
          ReturnVal <<- ReturnValDesignOrContrastsTable   # List contains designOrContrasts matrix as data.frame
      }   
  }

  GetDesignOrContrastsFromDropDowns <- function()
  {
   
    if (Design==TRUE)
    {
      NumRows <- NumSlides
      NumCols <- NumParameters
    }
    else
    {
      NumRows <- NumParameters
      NumCols <- NumContrasts
    }
    
    RNATypeOrParam1 <-c()
    RNATypeOrParam2 <-c()    
    plusOrMinusStringVec <- c("-","+")
    plusOrMinus <- c()
        
    if (NumCols>0)
      for (paramORcontrast in (1:NumCols))
      {
        # I think I wrote this code when I was an R Tcl/Tk beginner.  Check and update!
        # I think combo1 and combo2 should really be lists, not vectors!!!
        # *2 below, because the c() combines the tkwin objects which are acutally         
        # lists with 2 components: window ID and environment.

        selection1 <- tclvalue(.Tcl(paste(.Tk.ID(combo1[paramORcontrast*2-1]),"getvalue")))
        selection2 <- tclvalue(.Tcl(paste(.Tk.ID(combo2[paramORcontrast*2-1]),"getvalue")))        
        selection3 <- tclvalue(.Tcl(paste(.Tk.ID(combo3[paramORcontrast*2-1]),"getvalue")))                
        if (Design==TRUE)
        {        
          Try(if ((selection1=="-1")||(selection2=="-1")) 
            return (list(design=data.frame(),designCreatedFromDropDowns=TRUE,TargetVector=TargetVector,RNAType1=c(),RNAType2=c(),plusOrMinus=c())))
        }
        else
        {
          Try(if ((selection1=="-1")||(selection2=="-1"))
            return (list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,TargetVector=c(),Param1=c(),Param2=c(),plusOrMinus=c())))
        }        
        RNATypeOrParam1 <- c(RNATypeOrParam1,as.numeric(selection1)+1)
        RNATypeOrParam2 <- c(RNATypeOrParam2,as.numeric(selection2)+1)
        plusOrMinus <- c(plusOrMinus,plusOrMinusStringVec[as.numeric(selection3)+1])
        
      }

    designOrContrasts <- as.data.frame(matrix(nrow=NumRows,ncol=NumCols))
    if (Design==TRUE)
      Try(rownames(designOrContrasts) <- SlideNamesVec)
    else
      Try(rownames(designOrContrasts) <- ParameterNamesVec)

    Try(if (Design==TRUE)    
    {
      ParameterNamesVec <- vector(length=NumParameters)
      if (NumParameters>0)
        for (j in (1:NumParameters))
              ParameterNamesVec[j] <- paste("(",TargetVector[RNATypeOrParam1[j]],")-(",TargetVector[RNATypeOrParam2[j]],")",sep="")
      colnames(designOrContrasts) <- ParameterNamesVec
    }
    else
    {
      Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
      Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))            
      ContrastNamesVec <- vector(length=NumContrasts)    
      if (NumContrasts>0)
        for (j in (1:NumContrasts))          
          ContrastNamesVec[j] <- SimplifyContrastsExpression(paste("(",ParameterNamesVec[RNATypeOrParam1[j]],")",plusOrMinus[j],"(",ParameterNamesVec[RNATypeOrParam2[j]],")",sep=""))                                       
      colnames(designOrContrasts) <- ContrastNamesVec    
    })

    if (Design==TRUE)
    {
      ParamMatrix <- matrix(nrow=TargetsCounted,ncol=NumParameters)
      if (NumParameters>0)
        for (i in (1:NumParameters))
        { 
          for (j in (1:TargetsCounted))
            ParamMatrix[j,i] <- 0
          ParamMatrix[RNATypeOrParam1[i],i] <-  1
          ParamMatrix[RNATypeOrParam2[i],i] <- -1
        }
      Try(QR <- qr(ParamMatrix))
      Try(if (QR$rank!=ncol(QR$qr))
      {
        Try(tkmessageBox(title="Design Matrix",message=paste("Error:  Parameters are not linearly independent.\n\n",
          "Try entering independent parameters or click on the Advanced button to enter the design matrix directly.",sep=""),icon="error"))
        Try(return (list(design=data.frame(),designOrContrastsCreatedFromDropDowns=TRUE,TargetVector=TargetVector,RNAType1=c(),RNAType2=c(),plusOrMinus=c())))
      })
      RNATargetsVec <- vector(length=TargetsCounted)
      for (i in (1:NumSlides))
      {
        for (j in (1:TargetsCounted))
        { 
          RNATargetsVec[j] <- 0 
          if (paste(Cy5Targets[i])==paste(TargetVector[j]))
            RNATargetsVec[j] <- 1
          if (paste(Cy3Targets[i])==paste(TargetVector[j]))
            RNATargetsVec[j] <- -1
        }
        designOrContrasts[i,] <- qr.solve(ParamMatrix,RNATargetsVec)
      }
      designOrContrasts <- round(designOrContrasts,digits=8)
    }
    else # (Contrasts==TRUE)
    {
      Try(for (i in (1:NumParameters))
        for (j in (1:NumContrasts))
        {
          Try(designOrContrasts[i,j] <- 0)
          Try(if (RNATypeOrParam1[j]==i)  
            designOrContrasts[i,j] <- 1)
          Try(if (plusOrMinus[j]=="-")
          {
            Try(if(RNATypeOrParam2[j]==i)
              designOrContrasts[i,j] <- -1)         
          }
          else  # "+"
          {
            Try(if(RNATypeOrParam2[j]==i)
              designOrContrasts[i,j] <-  1)
          })
        })        
    }
    if (Design==TRUE)   
      return(list(design=designOrContrasts,designCreatedFromDropDowns=TRUE,TargetVector=TargetVector,RNAType1=RNATypeOrParam1,RNAType2=RNATypeOrParam2,plusOrMinus=plusOrMinus))  
    else
      return(list(contrasts=designOrContrasts,contrastsCreatedFromDropDowns=TRUE,TargetVector=c(),Param1=RNATypeOrParam1,Param2=RNATypeOrParam2,plusOrMinus=plusOrMinus))      
  }

  onOK <- function()
  {
    Try(designOrContrastsList <- GetDesignOrContrastsFromDropDowns())
    Try(if (Design==TRUE)
    {
      Try(if (nrow(designOrContrastsList$design)==0)
      {
        Try(tkmessageBox(title="Parameterization",message=paste("Error in creating parameterization from drop-down selection. ",
                       "Make sure you have selected an RNA pair for each parameter and that your parameters are linearly independent."),type="ok",icon="error"))                    
        Try(ReturnVal <<- list(design=data.frame(),designCreatedFromDropDowns=TRUE,TargetVector=TargetVector,RNAType1=c(),RNAType2=c(),plusOrMinus=c()))        
        return()
      }
      else
      {
          Try(tkgrab.release(ttDesignOrContrasts))
          Try(tkdestroy(ttDesignOrContrasts))
          Try(tkfocus(ttMain))
          Try(ReturnVal <<- designOrContrastsList)      
          Try(tkfocus(ttMain))
          return()
      })
      
    })
    Try(if (Contrasts==TRUE)
    {   
      Try(if (nrow(designOrContrastsList$contrasts)==0)    
      {
        Try(tkmessageBox(title="Contrasts",message=paste("Error in creating contrasts matrix from drop-down selection. ",
                         "Make sure you have selected a parameter pair for each contrast."),type="ok",icon="error"))                    
        Try(ReturnVal <<- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,TargetVector=c(),Param1=c(),Param2=c(),plusOrMinus=c()))
        return()
      } 
      else
      {
        Try(tkgrab.release(ttDesignOrContrasts))
        Try(tkdestroy(ttDesignOrContrasts))
        Try(tkfocus(ttMain))
        Try(ReturnVal <<- designOrContrastsList)
        Try(tkfocus(ttMain))
        return()
      })
    })
  }
  onCancel <- function() 
  {
    Try(tkgrab.release(ttDesignOrContrasts))
    Try(tkdestroy(ttDesignOrContrasts))
    Try(tkfocus(ttMain))
    if (Design==TRUE)
      ReturnVal <<- list(design=data.frame(),designCreatedFromDropDowns=TRUE,TargetVector=TargetVector,RNAType1=c(),RNAType2=c(),plusOrMinus=c())
    else
      ReturnVal <<- list(contrasts=data.frame(),contrastsCreatedFromDropDowns=TRUE,TargetVector=c(),Param1=c(),Param2=c(),plusOrMinus=c())    
  }   
  Advanced.but <- tkbutton(ttDesignOrContrasts,text="Advanced...",command=OnAdvanced,font=limmaGUIfont2)
  Try(OK.but <-tkbutton(ttDesignOrContrasts,text="   OK   ",command=onOK,font=limmaGUIfont2))
  Try(Cancel.but <-tkbutton(ttDesignOrContrasts,text=" Cancel ",command=onCancel,font=limmaGUIfont2))
  Try(tkgrid(OK.but,column=2,row=9+NumParameters))
  Try(tkgrid(Cancel.but,column=4,row=9+NumParameters))
  Try(tkgrid(Advanced.but,column=6,row=9+NumParameters))
  Try(tkgrid(tklabel(ttDesignOrContrasts,text="    ")))
      
  Try(tkfocus(ttDesignOrContrasts))
  
  Try(tkbind(ttDesignOrContrasts, "<Destroy>", function() {Try(tkgrab.release(ttDesignOrContrasts));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttDesignOrContrasts))
  return (ReturnVal)
}


SimplifyContrastsExpression <- function(string)
{
  RNATypesAndSign <- GetRNATypesFrom.ContrastsFromDropDowns.String(string)
  RNA1 <- RNATypesAndSign$RNA1
  RNA2 <- RNATypesAndSign$RNA2
  RNA3 <- RNATypesAndSign$RNA3
  RNA4 <- RNATypesAndSign$RNA4
  plusOrMinusSign <- RNATypesAndSign$plusOrMinusSign
  ReturnVal <- string
  
  if (RNA1==RNA3&&plusOrMinusSign=='-')
    ReturnVal <- paste("(",RNA4,")-(",RNA2,")",sep="")  
  if (RNA2==RNA4&&plusOrMinusSign=='-')  
    ReturnVal <- paste("(",RNA1,")-(",RNA3,")",sep="")  
  if (RNA1==RNA4&&plusOrMinusSign=='+')
    ReturnVal <- paste("(",RNA3,")-(",RNA2,")",sep="")    
  if (RNA2==RNA3&&plusOrMinusSign=='+')  
    ReturnVal <- paste("(",RNA1,")-(",RNA4,")",sep="")      
  return(ReturnVal)
  
}

GetRNATypesFrom.ContrastsFromDropDowns.String <- function(string)
{
  len <- nchar(string)
  string <- substr(string,3,len)
  # string == "a)-(b))-((b)-(c"
  len <- nchar(string)
  i <- 1
  while (substr(string,i,i)!=")" && (i<=len))
    i <- i + 1
  RNA1 <- substr(string,1,i-1)
  len <- nchar(string)
  string <- substr(string,i+3,len)
  len <- nchar(string)  
  i<-1
  while (substr(string,i,i)!=")" && (i<=len))
    i <- i + 1
  RNA2 <- substr(string,1,i-1)
  len <- nchar(string)
  plusOrMinusSign <- substr(string,i+2,i+2)
  string <- substr(string,i+5,len)
  len <- nchar(string)  
  i<-1
  while (substr(string,i,i)!=")" && (i<=len))
    i <- i + 1
  RNA3 <- substr(string,1,i-1)
  len <- nchar(string)
  string <- substr(string,i+3,len)
  len <- nchar(string)  
  i<-1
  while (substr(string,i,i)!=")" && (i<=len))
    i <- i + 1
  RNA4 <- substr(string,1,i-1)
  list(RNA1=RNA1,RNA2=RNA2,RNA3=RNA3,RNA4=RNA4,plusOrMinusSign=plusOrMinusSign)
}


GetParameterNames <- function(parameterizationTreeIndex)
{
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))  
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(ParameterNamesVec <- c())
  if (NumParameters>0)
    for (i in (1:NumParameters))
    {
      Try(ParametersNameNode<- paste("PMFParams.",parameterizationTreeIndex,".",i,sep=""))
      Try(ParameterNamesVec[i] <- (ParameterizationList[[ParameterizationNameNode]])[[ParametersNameNode]])    
    }  
  return (ParameterNamesVec)
}

GetReducedDuplicateSpacing <- function(parameterizationTreeIndex)
{
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))  
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(spacing <- get("spacing",envir=limmaGUIenvironment))
  Try(ndups   <- get("ndups",envir=limmaGUIenvironment))
  Try(gal     <- get("gal",envir=limmaGUIenvironment))
  Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
  Try(numSpotTypes <- nrow(SpotTypes))
  Try(SpotTypeStatus <- get("SpotTypeStatus",envir=limmaGUIenvironment))    
  Try(SpotTypesForLinearModel <- ParameterizationList[[ParameterizationNameNode]]$SpotTypesForLinearModel)  
  Try(RG <- get("RG",envir=limmaGUIenvironment))
  Try(MA <- MA.RG(RG)) # Locally only.
  Try(oldNumRows <- nrow(MA$M))
  Try(Omit <- "")
  Try(count <- 0)
  Try(for (i in (1:numSpotTypes))
  {
    Try(if (SpotTypesForLinearModel[i]==TRUE)
      next())
    Try(count <- count + 1)
    Try(if (count>1)
      Try(Omit <-paste(Omit,"|"))
    else
      Try(Omit <- "("))
    Try(Omit <- paste(Omit," (SpotTypeStatus==\"",SpotTypes[i,"SpotType"],"\")",sep=""))
  })
  
  Try(if (nchar(Omit)>0)
  {
    Try(Omit <- paste(Omit,")"))
    Try(Omit <- eval(parse(text=Omit)))  
    MA$M <- MA$M[!Omit,]
    MA$A <- MA$A[!Omit,]  
  })
  
  Try(newNumRows <- nrow(MA$M))  
  Try(parameterizationSpacing <- spacing)
  Try(spacingCorrected <- 0)
  Try(if (oldNumRows/spacing==2  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/2;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==3  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/3;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==4  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/4;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==5  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/5;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==6  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/6;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==7  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/7;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==8  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/8;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==9  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/9;  spacingCorrected <- 1})      
  Try(if (oldNumRows/spacing==10 && oldNumRows!=newNumRows){ parameterizationSpacing <- newNumRows/10; spacingCorrected <- 1})  
  
  return (parameterizationSpacing)
}


GetContrastsParameterizationNames <- function(parameterizationTreeIndex)
{
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(NumContrastParameterizations <- ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations)
  Try(ContrastsParameterizationNamesVec <- c())
  if (NumContrastParameterizations>0)
    for (i in (1:NumContrastParameterizations))
    {
      Try(ContrastsParameterizationNamesNode<- paste("ContrastsParameterizationNames.",parameterizationTreeIndex,".",i,sep=""))
      Try(ContrastsParameterizationNamesVec[i] <- (ParameterizationList[[ParameterizationNameNode]])[[ContrastsParameterizationNamesNode]])    
    }  
  return (ContrastsParameterizationNamesVec)
}

GetSpotTypesIncludedNames <- function(parameterizationTreeIndex)
{
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
  Try(numSpotTypes <- nrow(SpotTypes))
  Try(SpotTypesForLinearModel <- (ParameterizationList[[ParameterizationNameNode]])$SpotTypesForLinearModel)
  
  count <- 0
  Try(SpotTypesIncludedNamesVec <- c())
  if (numSpotTypes>0)
    for (i in (1:numSpotTypes))
    {
      if (SpotTypesForLinearModel[i]==FALSE)
          next()
      count <- count + 1
      Try(SpotTypesNode<- paste("PMFSpotTypes.",parameterizationTreeIndex,".",i,sep=""))
      Try(SpotTypesIncludedNamesVec[count] <- (ParameterizationList[[ParameterizationNameNode]])[[SpotTypesNode]])    
    }  
  return (SpotTypesIncludedNamesVec)
}

HowManyDups <- function()
{
  Try(ndups   <- get("ndups",envir=limmaGUIenvironment))
  Try(spacing <- get("spacing",envir=limmaGUIenvironment))    
  ttHowManyDups<-tktoplevel(ttMain)
  tkwm.deiconify(ttHowManyDups)
  tkgrab.set(ttHowManyDups)
  tkfocus(ttHowManyDups)  
  tkwm.title(ttHowManyDups,"Number of Duplicates")

  tkframe1 <- tkframe(ttHowManyDups,relief="groove",borderwidth=2)
  tkframe2 <- tkframe(ttHowManyDups,relief="groove",borderwidth=2)
  
  tkgrid(tklabel(ttHowManyDups,text="    "))
  tkgrid(tklabel(ttHowManyDups,text="Looking at the GAL file will help you to answer these questions.",font=limmaGUIfont2),columnspan=2)
  tkgrid(tklabel(ttHowManyDups,text="    "))
  tkgrid(tklabel(tkframe1,text="How many prints of each gene are there?          ",font=limmaGUIfont2),sticky="w")
  NumDups<- tclVar(paste(ndups))
  entry.NumDups<-tkentry(tkframe1,width="20",font=limmaGUIfont2,textvariable=NumDups,bg="white")
  tkgrid(tklabel(tkframe1,text="Number of prints for each gene : ",font=limmaGUIfont2),entry.NumDups,sticky="w")
  tkgrid(tkframe1,columnspan=2)
  tkgrid(tklabel(ttHowManyDups,text="    "))
  tkgrid(tklabel(tkframe2,text="What is the spacing between duplicate genes?  ",font=limmaGUIfont2),sticky="w")
  Spacing<- tclVar(paste(spacing))
  entry.Spacing<-tkentry(tkframe2,width="20",font=limmaGUIfont2,textvariable=Spacing,bg="white")
  tkgrid(tklabel(tkframe2,text="Spacing between duplicate genes : ",font=limmaGUIfont2),entry.Spacing,sticky="w")
  tkgrid(tkframe2,columnspan=2)
  tkgrid(tklabel(ttHowManyDups,text="    "))
  ReturnVal <- 0
  onOK <- function()
  {
      ndups <- as.integer(tclvalue(NumDups))
      Try(assign("ndups",ndups,limmaGUIenvironment))
      spacing <- as.integer(tclvalue(Spacing))
      Try(assign("spacing",spacing,limmaGUIenvironment))
      Try(tkgrab.release(ttHowManyDups))
      Try(tkdestroy(ttHowManyDups))
      Try(tkfocus(ttMain))
      ReturnVal <<- 1
  }
  onCancel <- function() {Try(tkgrab.release(ttHowManyDups));Try(tkdestroy(ttHowManyDups));Try(tkfocus(ttMain));ReturnVal <<- 0}
  OK.but <-tkbutton(ttHowManyDups,text="   OK   ",command=onOK,font=limmaGUIfont2)
  Cancel.but <-tkbutton(ttHowManyDups,text=" Cancel ",command=onCancel,font=limmaGUIfont2)
  tkgrid(OK.but,Cancel.but)
  tkgrid.configure(OK.but,sticky="e")
  tkgrid.configure(Cancel.but,sticky="w")
  tkgrid(tklabel(ttHowManyDups,text="    "))
  Try(tkfocus(ttHowManyDups))
  Try(tkbind(ttHowManyDups, "<Destroy>", function() {Try(tkgrab.release(ttHowManyDups));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttHowManyDups))
  return (ReturnVal)
}

ViewDesignOrContrastsMatrixInTable <- function(DesignOrContrasts,designOrContrastsList,parameterizationIndex,contrastsParameterizationIndex=NULL)
{
  Try(if (DesignOrContrasts!="Design" && DesignOrContrasts!="Contrasts")
  {
    Try(tkmessageBox(title="View Design Or Contrasts Matrix In Table",message="Error: First argument must be \"Design\" or \"Contrasts\".",icon="error"))
    return()
  })

  Try(SlideNamesVec <- get("SlideNamesVec",envir=limmaGUIenvironment))  
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))  
  Try(NumSlides <- get("NumSlides",envir=limmaGUIenvironment))    
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
  Try(ContrastsParameterizationNamesVec <- GetContrastsParameterizationNames(parameterizationTreeIndex))  
  Try(TclRequire("Tktable"))
  Try(ttViewDesignOrContrastsTable <- tktoplevel(ttMain))
  Try(tkwm.deiconify(ttViewDesignOrContrastsTable))
  Try(tkgrab.set(ttViewDesignOrContrastsTable))
  Try(tkfocus(ttViewDesignOrContrastsTable))
  Try(if (DesignOrContrasts=="Design")
    Try(tkwm.title(ttViewDesignOrContrastsTable,paste("Design matrix for parameterization ",ParameterizationNamesVec[parameterizationIndex],".",sep="")))
  else
  {
    Try(tkwm.title(ttViewDesignOrContrastsTable,paste("Contrasts matrix for contrasts parameterization ", ContrastsParameterizationNamesVec[contrastsParameterizationIndex],
     " in parameterization ",ParameterizationNamesVec[parameterizationIndex],".",sep="")))  
  })
  Try(if (DesignOrContrasts=="Design")
    Try(designOrContrasts <- designOrContrastsList$design)
  else
    Try(designOrContrasts <- designOrContrastsList$contrasts))
    
  onClose <- function() {Try(.Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<Leave>"))));Try(tkgrab.release(ttViewDesignOrContrastsTable));Try(tkdestroy(ttViewDesignOrContrastsTable));Try(tkfocus(ttMain))}

  Try(if (DesignOrContrasts=="Design")
  {
    NumRows <- NumSlides
    NumCols <- NumParameters
  }
  else # Contrasts
  {
    Try(NumContrasts <- ncol(designOrContrasts))
    NumRows <- NumParameters
    NumCols <- NumContrasts
  })
    
  if (is.null(colnames(designOrContrasts)))
  {
      Try(ColumnNamesVec <- c())
      if (NumCols>0)
        for (i in (1:NumCols)) 
          Try(if (DesignOrContrasts=="Design")
            Try(ColumnNamesVec <- c(ColumnNamesVec,paste("Param",i)))
          else
            Try(ColumnNamesVec <- c(ColumnNamesVec,paste("Contrast",i))))            
  }
  else
      Try(ColumnNamesVec <- colnames(designOrContrasts))

  Try(RowNamesVec <- c())
  Try(myRarray <- "    ")
  Try(for (i in (1:NumRows))
  {
      Try(if (DesignOrContrasts=="Design")      
      {
        Try(if(is.null(rownames(designOrContrasts)))
          Try(RowName <- SlideNamesVec[i])
        else
          Try(RowName <- rownames(designOrContrasts)[i]))        
      }
      else # Contrasts
      {
        Try(if (is.null(rownames(designOrContrasts)))
          Try(RowName <- paste("Param",i))
        else
          Try(RowName <- rownames(designOrContrasts)[i]))
      })
      Try(RowNamesVec <- c(RowNamesVec,RowName))
      Try(myRarray <- c(myRarray,paste(RowName)))
  })
  
  if (NumCols>0)
    for (j in (1:NumCols))      
    {
      Try(myRarray <- c(myRarray,paste(ColumnNamesVec[j])))
      for (i in (1:NumRows))
      {          
          if (nrow(designOrContrasts)==0)
              Try(myRarray <- c(myRarray,"0"))
          else
              Try(myRarray <- c(myRarray,paste(designOrContrasts[i,j])))
      }      
    }      

#      Try(n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env))
#      Try(tclArrayName <- paste("::RTcl", n, sep = ""))  
#      Try(l <- list(env = new.env()))
#      Try(assign(tclArrayName, NULL, envir = l$env))
#      Try(reg.finalizer(l$env, function(env) tkcmd("unset", ls(env))))
      Try(tclArrayVar1 <- tclArrayVar())
      Try(tclArrayName <- ls(tclArrayVar1$env))

      Try(dim(myRarray) <- c(NumRows+1,NumCols+1))

      # This will give an error if tclArray doesn't exist.
      # .Tcl("unset tclArray")
      if (NumCols>0)
        for (i in (0:NumRows))
          for (j in (0:NumCols))
             Try(tkcmd("set",paste(tclArrayName,"(",i,",",j,")",sep=""),paste(myRarray[i+1,j+1])))

      Try(table1 <- tkwidget(ttViewDesignOrContrastsTable,"table"))
      Try(tkconfigure(table1,variable=tclArrayName,rows=paste(NumRows+1),cols=paste(NumCols+1),
        titlerows="0",titlecols="0",selectmode="extended",colwidth="13",background="white",rowseparator="\"\n\"",
        colseparator="\"\t\"",resizeborders="col",multiline="0",
        xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...),state="disabled"))
      Try(xscr <- tkscrollbar(ttViewDesignOrContrastsTable,orient="horizontal", command=function(...)tkxview(table1,...)))
      Try(yscr <- tkscrollbar(ttViewDesignOrContrastsTable,command=function(...)tkyview(table1,...)))               
      Try(tkgrid(table1,yscr))
      Try(tkgrid.configure(yscr,sticky="nsw"))
      Try(tkconfigure(table1,font=limmaGUIfontTable))
      Try(tkgrid(xscr,sticky="new"))
      Try(copyFcn <-      function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<<Copy>>"))))


      Try(tkcmd(.Tk.ID(table1),"width","0",paste(max(4,max(nchar(rownames(designOrContrasts)))+2))))
      Try(for (j in (1:NumCols))      
        Try(tkcmd(.Tk.ID(table1),"width",paste(j),paste(max(4,nchar(colnames(designOrContrasts)[j])+2,max(nchar(designOrContrasts[,j]))+2)))))


      onSaveDesignOrContrastsMatrixAs <- function()
      {
        Try(if (DesignOrContrasts=="Design")        
          Try(DesignOrContrastsFileName <- tclvalue(tkgetSaveFile(filetypes="{{Design Matrix Files} {.txt}} {{All files} *}")))
        else
          Try(DesignOrContrastsFileName <- tclvalue(tkgetSaveFile(filetypes="{{Contrasts Matrix Files} {.txt}} {{All files} *}"))))         
        Try(if (!nchar(DesignOrContrastsFileName)) return())
        Try(len <- nchar(DesignOrContrastsFileName))
        if (len<=4)
          Try(DesignOrContrastsFileName <- paste(DesignOrContrastsFileName,".txt",sep=""))
        else if (substring(DesignOrContrastsFileName,len-3,len)!=".txt")
              Try(DesignOrContrastsFileName <- paste(DesignOrContrastsFileName,".txt",sep=""))
        Try(designOrContrasts <- as.data.frame(matrix(nrow=NumRows,ncol=NumCols)))
        Try(rownamesDesignOrContrasts <- c())
        Try(for (i in (1:NumRows))
            rownamesDesignOrContrasts[i] <- tclvalue(paste(tclArrayName,"(",i,",0)",sep="")))
        Try(colnamesdesignOrContrasts <- c())
        if (NumParameters>0)
          Try(for (j in (1:NumCols))
            colnamesdesignOrContrasts[j] <- tclvalue(paste(tclArrayName,"(0,",j,")",sep="")))
        Try(rownames(designOrContrasts) <- rownamesDesignOrContrasts)
        Try(colnames(designOrContrasts) <- colnamesdesignOrContrasts)
        if (NumParameters>0)
          Try(for (i in (1:NumRows))
            for (j in (1:NumParameters))
                designOrContrasts[i,j] <- as.numeric(tclvalue(paste(tclArrayName,"(",i,",",j,")",sep=""))))

        Try(write.table(designOrContrasts,file=DesignOrContrastsFileName,col.names=NA,sep="\t",quote=FALSE,row.names=TRUE))
      }



      Try(topMenu <- tkmenu(ttViewDesignOrContrastsTable, tearoff=FALSE))

      Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
      Try(tkadd(fileMenu, "command", label="Save As",    command=onSaveDesignOrContrastsMatrixAs))       
      Try(tkadd(fileMenu, "command", label="Close",      command=onClose)) 
      Try(tkadd(topMenu,  "cascade", label="File",menu=fileMenu)) 
      Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
      Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",      command=copyFcn)) 
      Try(tkadd(topMenu,  "cascade", label="Edit",menu=editMenu)) 

      Try(tkconfigure(ttViewDesignOrContrastsTable,menu=topMenu))

      Try(tkfocus(ttViewDesignOrContrastsTable))
      Try(tkbind(ttViewDesignOrContrastsTable, "<Destroy>", function () {Try(tkgrab.release(ttViewDesignOrContrastsTable));Try(tkfocus(ttMain))}))
      Try(tkwait.window(ttViewDesignOrContrastsTable))
}

ViewDesignOrContrastsMatrixAsPairs <- function(DesignOrContrasts,designOrContrastsList,parameterizationIndex,contrastsParameterizationIndex=NULL)
{
  Try(SlideNamesVec <- get("SlideNamesVec",envir=limmaGUIenvironment))  
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))  
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
  Try(ContrastsParameterizationNamesVec <- GetContrastsParameterizationNames(parameterizationTreeIndex))  
  Try(NumSlides <- get("NumSlides",envir=limmaGUIenvironment))    

  Try(if (DesignOrContrasts=="Design")
  {
    Try(designOrContrasts <- designOrContrastsList$design)
    NumRows <- NumSlides
    NumCols <- NumParameters
  }
  else # Contrasts
  {
    Try(designOrContrasts <- designOrContrastsList$contrasts)  
    Try(NumContrasts <- ncol(designOrContrasts))
    NumRows <- NumParameters
    NumCols <- NumContrasts
  })
    
  ttViewDesignOrContrastsAsPairs<-tktoplevel(ttMain)
  tkwm.deiconify(ttViewDesignOrContrastsAsPairs)
  tkgrab.set(ttViewDesignOrContrastsAsPairs)
  tkfocus(ttViewDesignOrContrastsAsPairs)
  Try(if (DesignOrContrasts=="Design")
  {
    Try(tkwm.title(ttViewDesignOrContrastsAsPairs,paste("Parameters in parameterization ",ParameterizationNamesVec[parameterizationIndex],".",sep="")))
    Try(TitleLabel<-tklabel(ttViewDesignOrContrastsAsPairs,text=paste("Parameters in parameterization ",ParameterizationNamesVec[parameterizationIndex],sep=""),font=limmaGUIfont2b))
  }
  else
  {
    Try(tkwm.title(ttViewDesignOrContrastsAsPairs,paste("Contrasts in contrasts parameterization ", ContrastsParameterizationNamesVec[contrastsParameterizationIndex],
     " in parameterization ",ParameterizationNamesVec[parameterizationIndex],".",sep="")))  
    Try(TitleLabel<-tklabel(ttViewDesignOrContrastsAsPairs,text=paste("Contrasts in contrasts parameterization ", ContrastsParameterizationNamesVec[contrastsParameterizationIndex],
     " in parameterization ",ParameterizationNamesVec[parameterizationIndex],".",sep=""),font=limmaGUIfont2b))     
  })

  Try(tkgrid(tklabel(ttViewDesignOrContrastsAsPairs,text="    ")))    
  Try(tkgrid(tklabel(ttViewDesignOrContrastsAsPairs,text="    "),TitleLabel))
  Try(tkgrid.configure(TitleLabel,columnspan=4))
  Try(tkgrid(tklabel(ttViewDesignOrContrastsAsPairs,text="    ")))  
  Try(if (DesignOrContrasts=="Design")
    ParameterOrContrastLabel <- tklabel(ttViewDesignOrContrastsAsPairs,text="Parameter",font=limmaGUIfont2b)
  else
    ParameterOrContrastLabel <- tklabel(ttViewDesignOrContrastsAsPairs,text="Contrast",font=limmaGUIfont2b))
  # Note that plusOrMinus IS A VECTOR (can be different for each contrast).
  Try(if (DesignOrContrasts=="Contrasts")
    Try(plusOrMinus <- designOrContrastsList$plusOrMinus)
  else
    plusOrMinus <- rep("-",NumCols))
  Try(tkgrid(tklabel(ttViewDesignOrContrastsAsPairs,text="    "),
             ParameterOrContrastLabel))

  if (is.null(colnames(designOrContrasts)))
  {
      Try(ColumnNamesVec <- c())
      if (NumCols>0)
        for (i in (1:NumCols)) 
          Try(if (DesignOrContrasts=="Design")
            Try(ColumnNamesVec <- c(ColumnNamesVec,paste("Param",i)))
          else
            Try(ColumnNamesVec <- c(ColumnNamesVec,paste("Contrast",i))))            
  }
  else
      Try(ColumnNamesVec <- colnames(designOrContrasts))

  Try(RowNamesVec <- c())
  Try(for (i in (1:NumRows))
  {
      Try(if (DesignOrContrasts=="Design")      
      {
        Try(if(is.null(rownames(designOrContrasts)))
          Try(RowName <- SlideNamesVec[i])
        else
          Try(RowName <- rownames(designOrContrasts)[i]))        
      }
      else # Contrasts
      {
        Try(if (is.null(rownames(designOrContrasts)))
          Try(RowName <- paste("Param",i))
        else
          Try(RowName <- rownames(designOrContrasts)[i]))
      })
      Try(RowNamesVec <- c(RowNamesVec,RowName))
  })

  if (NumCols>0)
    for (i in (1:NumCols))
    {
      Try(if (DesignOrContrasts=="Design")
      {
        Try(FirstItemOfPair  <- paste(designOrContrastsList$TargetVector[designOrContrastsList$RNAType1[i]]))
        Try(SecondItemOfPair <- paste(designOrContrastsList$TargetVector[designOrContrastsList$RNAType2[i]]))      
      }
      else
      {
        Try(FirstItemOfPair  <- paste(RowNamesVec[designOrContrastsList$Param1[i]]))
        Try(SecondItemOfPair <- paste(RowNamesVec[designOrContrastsList$Param2[i]]))
      })
      Try(if (plusOrMinus[i]=="+") plusOrMinusText <- "plus" else plusOrMinusText <- "minus")
      Try(tkgrid(tklabel(ttViewDesignOrContrastsAsPairs,text="    "),
                 tklabel(ttViewDesignOrContrastsAsPairs,text=ColumnNamesVec[i],background="white",font=limmaGUIfont2),
                 tklabel(ttViewDesignOrContrastsAsPairs,text="    "),
                 tklabel(ttViewDesignOrContrastsAsPairs,text=FirstItemOfPair,background="white",font=limmaGUIfont2),
                 tklabel(ttViewDesignOrContrastsAsPairs,text="    "),
                 tklabel(ttViewDesignOrContrastsAsPairs,text=plusOrMinusText,font=limmaGUIfont2,bg="white"),                 
                 tklabel(ttViewDesignOrContrastsAsPairs,text="    "),
                 tklabel(ttViewDesignOrContrastsAsPairs,text=SecondItemOfPair,background="white",font=limmaGUIfont2),
                 tklabel(ttViewDesignOrContrastsAsPairs,text="    ")                 
                 ))
      Try(tkgrid(tklabel(ttViewDesignOrContrastsAsPairs,text="    ")))
    }
  tkgrid(tklabel(ttViewDesignOrContrastsAsPairs,text="     "))
  
  Advanced.but <- tkbutton(ttViewDesignOrContrastsAsPairs,text="Advanced...",command=function() {ViewDesignOrContrastsMatrixInTable(DesignOrContrasts,designOrContrastsList,parameterizationIndex,contrastsParameterizationIndex)},font=limmaGUIfont2)
  onOK <- function() {Try(tkgrab.release(ttViewDesignOrContrastsAsPairs));Try(tkdestroy(ttViewDesignOrContrastsAsPairs));Try(tkfocus(ttMain))}
  OK.but <-tkbutton(ttViewDesignOrContrastsAsPairs,text="   OK   ",command=onOK,font=limmaGUIfont2)
  tkgrid(tklabel(ttViewDesignOrContrastsAsPairs,text="    "),OK.but,Advanced.but)
  tkgrid(tklabel(ttViewDesignOrContrastsAsPairs,text="    "))
      
  Try(tkfocus(ttViewDesignOrContrastsAsPairs))
  
  Try(tkbind(ttViewDesignOrContrastsAsPairs, "<Destroy>", function() {Try(tkgrab.release(ttViewDesignOrContrastsAsPairs));Try(tkfocus(ttMain))}))
  Try(tkwait.window(ttViewDesignOrContrastsAsPairs))
}

ViewExistingContrastsParameterization <- function()
{
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="View Existing Contrasts Parameterization",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  }
  
  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return())
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])

  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  
  Try(NumContrastParameterizations <- (ParameterizationList[[ParameterizationNameNode]])$NumContrastParameterizations)  
  
  if (NumContrastParameterizations==0)
  {
    Try(tkmessageBox(title="View Existing Contrasts Parameterization",message="There are no contrasts parameterizations available.  Select \"Compute Contrasts\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  }
  Try(contrastsParameterizationIndex <- ChooseContrastsParameterization(parameterizationTreeIndex))
  Try(if (contrastsParameterizationIndex==0)    return()    )
  Try(ContrastsParameterizationTreeIndexVec <- ParameterizationList[[ParameterizationNameNode]]$ContrastsParameterizationTreeIndexVec)
  Try(contrastsParameterizationTreeIndex <- ContrastsParameterizationTreeIndexVec[contrastsParameterizationIndex])          
  Try(ContrastsParameterizationListNode <- paste("ContrastsParameterization.",parameterizationTreeIndex,".",contrastsParameterizationTreeIndex, sep=""))  
  Try(contrastsList <- (ParameterizationList[[ParameterizationNameNode]]$Contrasts[[ContrastsParameterizationListNode]])$contrastsMatrixInList)  
  Try(if (contrastsList$contrastsCreatedFromDropDowns==FALSE)
      Try(ViewDesignOrContrastsMatrixInTable(DesignOrContrasts="Contrasts",contrastsList,parameterizationIndex,contrastsParameterizationIndex))
  else
      Try(ViewDesignOrContrastsMatrixAsPairs(DesignOrContrasts="Contrasts",contrastsList,parameterizationIndex,contrastsParameterizationIndex)))
}


ViewExistingParameterization <- function()
{
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="View Existing Parameterization",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  }
  
  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return())
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])

  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(designList <- (ParameterizationList[[ParameterizationNameNode]])$designList)
  Try(if (designList$designCreatedFromDropDowns==FALSE)
      ViewDesignOrContrastsMatrixInTable(DesignOrContrasts="Design",designList,parameterizationIndex)
  else
      ViewDesignOrContrastsMatrixAsPairs(DesignOrContrasts="Design",designList,parameterizationIndex))
}

InitNewParameterization <- function()
{
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))  
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment)) 
  Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
  Try(numSpotTypes <- nrow(SpotTypes))
  if (ArraysLoaded==FALSE)
  {
      Try(tkmessageBox(title="Create New Parameterization",
        message="No arrays have been loaded.  Please try New or Open from the File menu.",
        type="ok",icon="error"))
      Try(tkfocus(ttMain))
      return(list(result="cancel"))
  }

  Try(SpotTypesForLinearModel <- GetSpotTypesForLinearModel())
  Try(if (length(SpotTypesForLinearModel)==0)
      return(list(result="cancel")))

  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))  
  Try(designList <- GetDesignOrContrasts(Design=TRUE))
  Try(if (nrow(designList$design)==0) return(list(result="cancel")))
  Try(designCreatedFromDropDowns <- designList$designCreatedFromDropDowns)
  Try(design <- designList$design)
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))        
  Try(ParameterNamesVec <- colnames(design))
  Try(ParameterizationNameText <- GetParameterizationName())
  Try(if (ParameterizationNameText=="GetParameterizationName.CANCEL") return(list(result="cancel")))
  Try(while (nchar(ParameterizationNameText)==0)
  {
      Try(tkmessageBox(title="Parameterization Name",message="Please enter a name for this parameterization (design matrix)",type="ok",icon="error"))
      Try(ParameterizationNameText <- GetParameterizationName())
      if (ParameterizationNameText=="GetParameterizationName.CANCEL") 
      {
          Try(tkfocus(ttMain))          
          return(list(result="cancel"))
      }
  })
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(tkfocus(ttMain))
  Try(parameterizationIndex <- 0)
  Try(newParameterization <- 1)
  Try(if (ParameterizationNameText %in% ParameterizationNamesVec)
  {
      Try(parameterizationIndex <- match(ParameterizationNameText,ParameterizationNamesVec))
      Try(mbVal<-tclvalue(tkmessageBox(title="Parameterization Name",
        message="This parameterization name already exists.  Replace?",
        type="yesnocancel",icon="question")))
      Try(if (mbVal=="cancel") return(list(result="cancel")))
      Try(if (mbVal=="yes") newParameterization <- 0)
      Try(if (mbVal=="no") newParameterization <- 1)
  }
  else
      Try(newParameterization <- 1))

  Try(if (newParameterization==1)
  {
      Try(if (length(ParameterizationTreeIndexVec)!=NumParameterizations)
      {
          Try(tkmessageBox(title="Parameterizations",message="Length of ParameterizationTreeIndexVec is not equal to NumParameterizations.",type="ok",icon="error"))
          Try(tkfocus(ttMain))
          return(list(result="cancel"))
      })
      Try(NumParameterizations <- NumParameterizations + 1)
      Try(parameterizationIndex <- NumParameterizations)
      Try(if (length(ParameterizationTreeIndexVec)==0)
          Try(parameterizationTreeIndex <- 1)
      else
          Try(parameterizationTreeIndex <- max(ParameterizationTreeIndexVec)+1))
      Try(ParameterizationTreeIndexVec[parameterizationIndex] <- parameterizationTreeIndex)

      Try(ParameterizationNamesVec <- c(ParameterizationNamesVec,ParameterizationNameText))
      Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
      Try(ParameterizationList[[ParameterizationNameNode]] <- list())
  }
  else # Replace existing parameterization with the same name.
  {
      Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
      Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
      Try(tkdelete(ParameterizationTREE,ParameterizationNameNode))
      Try(ParameterizationList <- deleteItemFromList(ParameterizationList,paste("ParameterizationName.",
                                                                                    parameterizationTreeIndex,sep="")))
      Try(ParameterizationList[[ParameterizationNameNode]] <- list())
  })
  Try(assign("ParameterizationTreeIndexVec",ParameterizationTreeIndexVec,limmaGUIenvironment))
  Try(assign("ParameterizationNamesVec",ParameterizationNamesVec,limmaGUIenvironment))
  Try(assign("ParameterizationList",ParameterizationList,limmaGUIenvironment))
  Try(assign("NumParameterizations",NumParameterizations,limmaGUIenvironment))

  Try(tkconfigure(ttMain,cursor="arrow")) 

  return(list(result="ok",parameterizationIndex=parameterizationIndex,
         parameterizationTreeIndex=parameterizationTreeIndex,
         ParameterizationNameText=ParameterizationNameText,
         ParameterNamesVec=ParameterNamesVec,
         SpotTypesForLinearModel=SpotTypesForLinearModel,
         designList=designList))
}

CreateTreeAndList <- function(parameterizationIndex,parameterizationTreeIndex,
    ParameterizationNameText,ParameterNamesVec,designList,SpotTypesForLinearModel)
{
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))        
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec     <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList         <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(design <- designList$design) 
  Try(ndups   <- get("ndups",envir=limmaGUIenvironment))
  Try(spacing <- get("spacing",envir=limmaGUIenvironment))  
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment)) 
  Try(WeightingType <- get("WeightingType",envir=limmaGUIenvironment))
  Try(SpotTypeStatus <- get("SpotTypeStatus",envir=limmaGUIenvironment))  
  Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
  Try(RG <- get("RG",envir=limmaGUIenvironment))
  Try(numSpotTypes <- nrow(SpotTypes))
 
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(ParamMainFitNode   <- paste("ParamMainFit.",parameterizationTreeIndex,sep=""))
  Try(ParamContrastsNode <- paste("ParamContrasts.",parameterizationTreeIndex,sep=""))
  Try(PMFNoneNode <- paste("PMFNone.",parameterizationTreeIndex,sep=""))
  Try(PMFSpotTypesNode <- paste("PMFSpotTypes.",parameterizationTreeIndex,sep=""))  
  Try(PMFMANormMethodNode <- paste("PMFMANormMethod.",parameterizationTreeIndex,sep=""))    
  Try(PMFParamsNode <- paste("PMFParams.",parameterizationTreeIndex,sep=""))
  Try(PMFLinModNode <- paste("PMFLinMod.",parameterizationTreeIndex,sep=""))
  Try(PMFLinModStatusNode <- paste("PMFLinModStatus.",parameterizationTreeIndex,sep=""))
  Try(PMFEBayesNode <- paste("PMFEBayes.",parameterizationTreeIndex,sep=""))
  Try(PMFEBayesStatusNode <- paste("PMFEBayesStatus.",parameterizationTreeIndex,sep=""))
  Try(PMFDupCorNode <- paste("PMFDupCor.",parameterizationTreeIndex,sep=""))
  Try(PMFDupCorStatusNode <- paste("PMFDupCorStatus.",parameterizationTreeIndex,sep=""))

  Try(tkdelete(mainTree,"Parameterizations"))
  Try(tkinsert(mainTree,"end","root","Parameterizations" ,text="Parameterizations",font=limmaGUIfontTree))
  Try(if (NumParameterizations>0)
  {
    Try(for (i in (1:NumParameterizations))
    {
      Try(ParameterizationsStatusNameNode <- paste("Parameterizations.Status.",i,sep=""))
      Try(tkinsert(mainTree,"end","Parameterizations",ParameterizationsStatusNameNode ,text=ParameterizationNamesVec[i],font=limmaGUIfontTree))
    })
  }
  else
    Try(tkinsert(mainTree,"end","Parameterizations","Parameterizations.Status.1" ,text="None",font=limmaGUIfontTree)))

  Try(tkinsert(ParameterizationTREE,"end","root",ParameterizationNameNode ,text=ParameterizationNameText,font=limmaGUIfontTree))
  Try(ParameterizationList[[ParameterizationNameNode]][[ParameterizationNameNode]] <- ParameterizationNameText)

  Try(tkinsert(ParameterizationTREE,"0",ParameterizationNameNode,ParamMainFitNode,text="Main fit",font=limmaGUIfontTree))
  Try(ParameterizationList[[ParameterizationNameNode]][[ParamMainFitNode]] <- "Main fit")

  Try(tkinsert(ParameterizationTREE,"1",ParameterizationNameNode,ParamContrastsNode,text="Contrasts",font=limmaGUIfontTree))
  
  Try(ParameterizationList[[ParameterizationNameNode]][[ParamContrastsNode]] <- "Contrasts")

  # Here we are creating a parameterization, so initially there will be no contrasts, but we will still
  # have one contrasts node, labeled "none".
  Try(ContrastsParameterizationNamesNode <- paste("ContrastsParameterizationNames.",parameterizationTreeIndex,".1",sep=""))
  Try(tkinsert(ParameterizationTREE,"0",ParamContrastsNode,ContrastsParameterizationNamesNode,text="none",font=limmaGUIfontTree))
  Try(ParameterizationList[[ParameterizationNameNode]][[ContrastsParameterizationNamesNode]] <- "none") 

##################################################################################################################
  Try(tkinsert(ParameterizationTREE,"0",ParamMainFitNode,PMFSpotTypesNode,text="Spot Type(s) Included",font=limmaGUIfontTree))
  Try(ParameterizationList[[ParameterizationNameNode]][[PMFSpotTypesNode]] <- "SpotTypes") 
    
  Try(if (numSpotTypes>0)
    for (i in (1:numSpotTypes))
    {
      Try(if (SpotTypesForLinearModel[i]==FALSE)
          next())
      Try(NewNode <- paste("PMFSpotTypes.",parameterizationTreeIndex,".",i,sep=""))
      Try(tkinsert(ParameterizationTREE,"end",PMFSpotTypesNode, NewNode,text=SpotTypes[i,"SpotType"],font=limmaGUIfontTree))
      Try(ParameterizationList[[ParameterizationNameNode]][[NewNode]] <- SpotTypes[i,"SpotType"])
    })

# Insert SpotTypesForLinearModel vector (containing TRUEs and FALSEs)
  Try(ParameterizationList[[ParameterizationNameNode]][["SpotTypesForLinearModel"]] <- SpotTypesForLinearModel)

##################################################################################################################
  Try(tkinsert(ParameterizationTREE,"end",ParamMainFitNode,PMFMANormMethodNode,text="M A Normalization Method",font=limmaGUIfontTree))
  Try(ParameterizationList[[ParameterizationNameNode]][[PMFMANormMethodNode]] <- "MANormMethod") 
  Try(NewNode <- paste("PMFMANormMethod.",parameterizationTreeIndex,".",1,sep=""))
  Try(tkinsert(ParameterizationTREE,"end",PMFMANormMethodNode, NewNode,text="Not available",font=limmaGUIfontTree))
  Try(ParameterizationList[[ParameterizationNameNode]][[NewNode]] <- "Not available")

##################################################################################################################

      Try(tkinsert(ParameterizationTREE,"end",ParamMainFitNode,PMFParamsNode,text="Parameters",font=limmaGUIfontTree))
      Try(ParameterizationList[[ParameterizationNameNode]][[PMFParamsNode]] <- "Parameters") 

      if (NumParameters>0)
        for (i in (1:NumParameters))
        {
          Try(NewNode <- paste("PMFParams.",parameterizationTreeIndex,".",i,sep=""))
          Try(tkinsert(ParameterizationTREE,"end",PMFParamsNode, NewNode,text=ParameterNamesVec[i],font=limmaGUIfontTree))
          Try(ParameterizationList[[ParameterizationNameNode]][[NewNode]] <- ParameterNamesVec[i])
        }

    # Insert design matrix list object (containing design matrix and a flag saying whether
    # the design matrix was created from drop-downs) into list version of tree.
      Try(ParameterizationList[[ParameterizationNameNode]][["designList"]] <- designList) 

      Try(tkinsert(ParameterizationTREE,"end",ParamMainFitNode,PMFLinModNode,text="Linear Model Fit",font=limmaGUIfontTree))
      Try(ParameterizationList[[ParameterizationNameNode]][[PMFLinModNode]] <- "Linear Model Fit")

      Try(tkinsert(ParameterizationTREE,"0",PMFLinModNode,PMFLinModStatusNode,text="Not available",font=limmaGUIfontTree))
      Try(ParameterizationList[[ParameterizationNameNode]][[PMFLinModStatusNode]] <- "Not available")

      Try(tkinsert(ParameterizationTREE,"end",ParamMainFitNode,PMFEBayesNode,text="Empirical Bayes Statistics",font=limmaGUIfontTree))
      Try(ParameterizationList[[ParameterizationNameNode]][[PMFEBayesNode]] <- "Empirical Bayes Statistics")

      Try(tkinsert(ParameterizationTREE,"0",PMFEBayesNode,PMFEBayesStatusNode,text="Not available",font=limmaGUIfontTree))
      Try(ParameterizationList[[ParameterizationNameNode]][[PMFEBayesStatusNode]] <- "Not available")

      Try(tkinsert(ParameterizationTREE,"end",ParamMainFitNode,PMFDupCorNode,text="Duplicate Correlation",font=limmaGUIfontTree))
      Try(ParameterizationList[[ParameterizationNameNode]][[PMFDupCorNode]] <- "Duplicate Correlation")

      Try(tkinsert(ParameterizationTREE,"0",PMFDupCorNode,PMFDupCorStatusNode,text="Not available",font=limmaGUIfontTree))
      Try(ParameterizationList[[ParameterizationNameNode]][[PMFDupCorStatusNode]] <- "Not available")

    # Insert NumContrastParameterizations into list version of parameterization tree.
      Try(ParameterizationList[[ParameterizationNameNode]][["NumContrastParameterizations"]] <- 0)

      Try(ParameterizationList[[ParameterizationNameNode]][["Contrasts"]] <- list())

      Try(ParameterizationList[[ParameterizationNameNode]][["ContrastsParameterizationTreeIndexVec"]] <- c()) 

      Try(assign("ParameterizationTreeIndexVec",ParameterizationTreeIndexVec,limmaGUIenvironment))
      Try(assign("ParameterizationNamesVec",ParameterizationNamesVec,limmaGUIenvironment))
      Try(assign("ParameterizationList",ParameterizationList,limmaGUIenvironment))
      Try(assign("NumParameterizations",NumParameterizations,limmaGUIenvironment))

      Try(tkconfigure(ttMain,cursor="arrow"))
      return("ok")
}

CreateNewParameterization <- function()
{
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))  
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment)) 
  Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
  Try(numSpotTypes <- nrow(SpotTypes))
  
  if (ArraysLoaded==FALSE)
  {
      Try(tkmessageBox(title="Create New Parameterization",
          message="No arrays have been loaded.  Please try New or Open from the File menu.",
          type="ok",icon="error"))
      Try(tkfocus(ttMain))
      return()
  }

  Try(InitNewParameterizationReturnVal <- InitNewParameterization())
  Try(if (InitNewParameterizationReturnVal$result=="cancel") return())
  Try(parameterizationIndex <- InitNewParameterizationReturnVal$parameterizationIndex)
  Try(parameterizationTreeIndex <- InitNewParameterizationReturnVal$parameterizationTreeIndex)
  Try(ParameterizationNameText <- InitNewParameterizationReturnVal$ParameterizationNameText) 
  Try(ParameterNamesVec <- InitNewParameterizationReturnVal$ParameterNamesVec)
  Try(designList <- InitNewParameterizationReturnVal$designList)
  Try(SpotTypesForLinearModel <- InitNewParameterizationReturnVal$SpotTypesForLinearModel)

  Try(CreateTreeAndList(parameterizationIndex,parameterizationTreeIndex,
    ParameterizationNameText,ParameterNamesVec,designList,SpotTypesForLinearModel))
}

ViewRNATargets <- function()
{
      Try(NumSlides <- get("NumSlides",envir=limmaGUIenvironment))
      Try(Targets <- get("Targets",envir=limmaGUIenvironment))      
      Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment)) 
  
      if (ArraysLoaded==FALSE)
      {
          Try(tkmessageBox(title="RNA Targets",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
          Try(tkfocus(ttMain))
          return()
      }
      if (nrow(Targets)==0)
      {
          Try(tkmessageBox(title="RNA Targets",message="No RNA targets have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
          Try(tkfocus(ttMain))
          return()
      }

      Try(TclRequire("Tktable"))
      Try(ttViewRNATargets <- tktoplevel(ttMain))
      Try(tkwm.deiconify(ttViewRNATargets))
      Try(tkgrab.set(ttViewRNATargets))
      Try(tkfocus(ttViewRNATargets))
      Try(tkwm.title(ttViewRNATargets,"RNA Targets"))
      
#      Try(n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env))
#     Try(tclArrayName <- paste("::RTcl", n, sep = ""))
      Try(tclArrayVar1 <- tclArrayVar())
      Try(tclArrayName <- ls(tclArrayVar1$env))
      
      onClose <- function() {Try(tkgrab.release(ttViewRNATargets));Try(tkdestroy(ttViewRNATargets));Try(tkfocus(ttMain))}
  
      Try(NumCols <- ncol(Targets))
      Try(NumRows <- nrow(Targets))
     
      Try(myRarray <- c())

      if (NumCols>0)
        for (j in (1:NumCols))      
        {
          Try(myRarray <- c(myRarray,paste(colnames(Targets)[j])))
          for (i in (1:NumRows))
          {          
              Try(myRarray <- c(myRarray,paste(Targets[i,j])))
          }      
        }      

          Try(dim(myRarray) <- c(NumRows+1,NumCols))

          # This will give an error if tclArray doesn't exist.
          # .Tcl("unset tclArray")
          if (NumRows>0 && NumCols>0)
            for (i in (0:NumRows))
              for (j in (1:NumCols))
#                 Try(.Tcl(paste("set ",tclArrayName,"(",i,",",j-1,") ",myRarray[i+1,j],sep="")))                 
                 Try(tkcmd("set",paste(tclArrayName,"(",i,",",j-1,")",sep=""),paste(myRarray[i+1,j])))
         
          # Below, I should just use tkwidget(ttViewRNATargets,"table",...) 
          Try(table1 <- .Tk.subwin(ttViewRNATargets))
          Try(.Tcl(paste("table",.Tk.ID(table1),.Tcl.args(variable=tclArrayName,rows=paste(NumRows+1),cols=paste(NumCols),titlerows="0",titlecols="0",selectmode="extended",colwidth="13",background="white",rowseparator="\"\n\"",colseparator="\"\t\"",resizeborders="col",multiline="0",
                titlerows=1,colstretchmode="unset",
                xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...),state="disabled"))))
          Try(xscr <- tkscrollbar(ttViewRNATargets,orient="horizontal", command=function(...)tkxview(table1,...)))
          Try(yscr <- tkscrollbar(ttViewRNATargets,command=function(...)tkyview(table1,...)))               
          Try(tkgrid(table1,yscr))
          Try(tkgrid.configure(yscr,sticky="nsw"))
          Try(tkgrid(xscr))
          Try(tkgrid.configure(xscr,sticky="new"))
          Try(tkconfigure(table1,font=limmaGUIfontTable))

          for (j in (1:NumCols))      
            Try(tkcmd(.Tk.ID(table1),"width",paste(j-1),paste(max(4,nchar(colnames(Targets)[j])+2,max(nchar(Targets[,j]))+2))))

          Try(copyFcn <-      function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<<Copy>>"))))

          saveTargetsFile <- function()
          {
            Try(TargetsFileName <- tclvalue(tkgetSaveFile(filetypes="{{RNA Targets Files} {.txt}} {{All files} *}")))
            Try(if (!nchar(TargetsFileName)) return())
            Try(len <- nchar(TargetsFileName))
            if (len<=4)
              Try(  TargetsFileName <- paste(TargetsFileName,".txt",sep=""))
            else if (substring(TargetsFileName,len-3,len)!=".txt")
                  Try(TargetsFileName <- paste(TargetsFileName,".txt",sep=""))
            Try(Targets <- matrix(nrow=NumRows,ncol=NumCols))
            Try(colnamesTargets <- c())
            if (NumCols>0)
              Try(for (j in (1:NumCols))
                colnamesTargets[j] <- tclvalue(paste(tclArrayName,"(0,",j-1,")",sep="")))
            Try(colnames(Targets) <- colnamesTargets)       
            if (NumRows>0 && NumCols>0)
              Try(for (i in (1:NumRows))
                 for (j in (1:NumCols))
                    Targets[i,j] <- tclvalue(paste(tclArrayName,"(",i,",",j-1,")",sep="")))

            Try(write.table(Targets,file=TargetsFileName,sep="\t",quote=FALSE,col.names=TRUE,row.names=FALSE))
          }


          Try(topMenu <- tkmenu(ttViewRNATargets, tearoff=FALSE))

          Try(topMenu <- tkmenu(ttViewRNATargets, tearoff=FALSE))
          Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
          Try(tkadd(fileMenu, "command", label="Save As",   command=saveTargetsFile)) # ) # ,font=limmaGUIfontMenu))
          Try(tkadd(fileMenu, "command", label="Close",   command=onClose)) # ) # ,font=limmaGUIfontMenu))          
          Try(tkadd(topMenu,  "cascade", label="File",menu=fileMenu)) # ) # ,font=limmaGUIfontMenu))  

          Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
          Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",      command=copyFcn)) # ) # ,font=limmaGUIfontMenu))
          Try(tkadd(topMenu,  "cascade", label="Edit",menu=editMenu)) # ) # ,font=limmaGUIfontMenu))  
                   
          Try(tkconfigure(ttViewRNATargets,menu=topMenu))

#          Try(BlankLabel1<-tklabel(ttViewRNATargets,text="    "))
#          Try(tkgrid(BlankLabel1))
#          Try(BlankLabel2<-tklabel(ttViewRNATargets,text="    "))
#          Try(CloseButtonFrame <- tkframe(ttViewRNATargets)) 
#          Try(Close.but <-tkbutton(CloseButtonFrame,text=" Close ",command=onClose,font=limmaGUIfont2))      
#          Try(tkgrid(Close.but))
#          Try(tkgrid(CloseButtonFrame))
#          Try(BlankLabel3<-tklabel(ttViewRNATargets,text="    "))
#          Try(tkgrid(BlankLabel3))

          Try(tkfocus(ttViewRNATargets))
          Try(tkbind(ttViewRNATargets, "<Destroy>", function() {Try(tkgrab.release(ttViewRNATargets));Try(tkfocus(ttMain))}))
          Try(tkwait.window(ttViewRNATargets))
}

########################################################################################################
# Some C-style string searching functions, because I'm not very good at using regular expressions ;-) 

# Returns the index where needle is found in haystack or zero if not found.
nstrstr <- function(haystack,needle)
{
  lenHaystack <- nchar(haystack)
  lenNeedle   <- nchar(needle)
  if (lenHaystack < lenNeedle)
    return (0)
  if (lenHaystack == lenNeedle)
    return(haystack==needle)
  lenDiff <- lenHaystack-lenNeedle
  for (i in (1:lenDiff))
    if (needle==substr(haystack,i,i+lenNeedle-1))
      return(i)

  return (0)
}

strstr <- function(haystack,needle)
{
  strIndex <- nstrstr(haystack,needle)
  if (strIndex==0)
    return ("")
  return (substr(haystack,strIndex,nchar(haystack)))
}

#####################################################################################################

ViewSpotTypes <- function()
{
      Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))      
      Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment)) 
  
      if (ArraysLoaded==FALSE)
      {
          Try(tkmessageBox(title="Spot Types",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
          Try(tkfocus(ttMain))
          return()
      }
      if (nrow(SpotTypes)==0)
      {
          Try(tkmessageBox(title="Spot Types",message="No spot types have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
          Try(tkfocus(ttMain))
          return()
      }

      Try(TclRequire("Tktable"))
      Try(ttViewSpotTypes <- tktoplevel(ttMain))
      Try(tkwm.deiconify(ttViewSpotTypes))
      Try(tkgrab.set(ttViewSpotTypes))
      Try(tkfocus(ttViewSpotTypes))
      Try(tkwm.title(ttViewSpotTypes,"Spot Types"))

#      Try(n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env))
#     Try(tclArrayName <- paste("::RTcl", n, sep = ""))      
      Try(tclArrayVar1 <- tclArrayVar())
      Try(tclArrayName <- ls(tclArrayVar1$env))
      
      onOK <- function() 
      {
          Try(.Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<Leave>"))))

          NumRows <- as.integer(strsplit(tclvalue(tkcmd(table1,"configure","-rows"))," ")[[1]][5])-1
          NumCols <- as.integer(strsplit(tclvalue(tkcmd(table1,"configure","-cols"))," ")[[1]][5])
          
          Try(SpotTypes <- as.data.frame(matrix(nrow=NumRows,ncol=NumCols)))
          Try(colnamesSpotTypes <- c())
          if (NumCols>0)
            for (j in (1:NumCols))
              Try(colnamesSpotTypes[j] <- tclvalue(paste(tclArrayName,"(0,",j-1,")",sep="")))
          Try(colnames(SpotTypes) <- colnamesSpotTypes)
          if (NumRows>0 && NumCols>0)
            for (i in (1:NumRows))
               for (j in (1:NumCols))
                    Try(SpotTypes[i,j] <- tclvalue(paste(tclArrayName,"(",i,",",j-1,")",sep="")))
          Try(assign("SpotTypes",SpotTypes,limmaGUIenvironment))           
          tkconfigure(ttViewSpotTypes,cursor="watch")   
          Try(UpdateSpotTypesStatus())
          tkconfigure(ttViewSpotTypes,cursor="arrow")             
          Try(tkgrab.release(ttViewSpotTypes));Try(tkdestroy(ttViewSpotTypes));Try(tkfocus(ttMain))
      }
      onCancel <- function() {Try(tkgrab.release(ttViewSpotTypes));Try(tkdestroy(ttViewSpotTypes));Try(tkfocus(ttMain))}

#      Try(labelSpotTypes <- tklabel(ttViewSpotTypes,text=paste("Spot Types"),font=limmaGUIfont2b))
#      Try(tkgrid(labelSpotTypes,column=2,columnspan=2))
#      Try(tkgrid(tklabel(ttViewSpotTypes,text="    ")))
  
      Try(NumCols <- ncol(SpotTypes))
      Try(NumRows <- nrow(SpotTypes))
     
      Try(myRarray <- c())

      if (NumCols>0)
        for (j in (1:NumCols))      
        {
          Try(myRarray <- c(myRarray,paste(colnames(SpotTypes)[j])))
          for (i in (1:NumRows))
          {          
              Try(myRarray <- c(myRarray,paste(SpotTypes[i,j])))
          }      
        }      

          Try(dim(myRarray) <- c(NumRows+1,NumCols))

          # This will give an error if tclArray doesn't exist.
          # .Tcl("unset tclArray")
          if (NumRows>0 && NumCols>0)
            for (i in (0:NumRows))
              for (j in (1:NumCols))
#                 Try(.Tcl(paste("set ",tclArrayName,"(",i,",",j-1,") ",myRarray[i+1,j],sep="")))
                Try(tkcmd("set",paste(tclArrayName,"(",i,",",j-1,")",sep=""),paste(myRarray[i+1,j])))                 
         
          # Below, I should just use tkwidget(ttViewSpotTypes,"table",...) 
          Try(table1 <- .Tk.subwin(ttViewSpotTypes))
          Try(.Tcl(paste("table",.Tk.ID(table1),.Tcl.args(variable=tclArrayName,rows=paste(NumRows+1),cols=paste(NumCols),titlerows="0",titlecols="0",selectmode="extended",colwidth="13",background="white",rowseparator="\"\n\"",colseparator="\"\t\"",resizeborders="col",multiline="0",
                titlerows=1,
                xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...)))))
          Try(xscr <- tkscrollbar(ttViewSpotTypes,orient="horizontal", command=function(...)tkxview(table1,...)))
          Try(yscr <- tkscrollbar(ttViewSpotTypes,command=function(...)tkyview(table1,...)))                
          Try(tkgrid(table1,yscr))
          Try(tkgrid.configure(yscr,sticky="nsw"))
          Try(tkgrid(xscr))
          Try(tkgrid.configure(xscr,sticky="new"))
          Try(tkconfigure(table1,font=limmaGUIfontTable))

          for (j in (1:NumCols))      
            Try(tkcmd(.Tk.ID(table1),"width",paste(j-1),paste(max(4,nchar(colnames(SpotTypes)[j])+2,max(nchar(SpotTypes[,j]))+2))))

          Try(copyFcn <-      function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(table1),"<<Copy>>"))))

          openSpotTypesFile <- function()
          {
            Try(SpotTypesFileName <- tclvalue(tkgetOpenFile(filetypes="{{Spot-Type Files} {.txt}} {{All files} *}")))
            Try(if (!nchar(SpotTypesFileName)) return())
            Try(SpotTypes <- read.table(SpotTypesFileName,header=FALSE,sep="\t",quote="\"",as.is=TRUE,comment.char=""))
            Try(NumRows <<- nrow(SpotTypes)-1)
            Try(NumCols <<- ncol(SpotTypes))            
            Try(tkconfigure(table1,rows=paste(NumRows+1),cols=paste(NumCols)))
            # This will give an error if tclArray doesn't exist.
            # .Tcl("unset tclArray")
            if (NumRows>0 && NumCols>0)
               for (i in (0:NumRows))
                 for (j in (1:NumCols))
#                   Try(.Tcl(paste("set ",tclArrayName,"(",i,",",j-1,") \"",SpotTypes[i+1,j],"\"",sep="")))
                   Try(tkcmd("set",paste(tclArrayName,"(",i,",",j-1,")",sep=""),paste(SpotTypes[i+1,j])))                   
          }

          saveSpotTypesFile <- function()
          {
            Try(SpotTypesFileName <- tclvalue(tkgetSaveFile(filetypes="{{Spot-Type Files} {.txt}} {{All files} *}")))
            Try(if (!nchar(SpotTypesFileName)) return())
            Try(len <- nchar(SpotTypesFileName))
            if (len<=4)
              Try(  SpotTypesFileName <- paste(SpotTypesFileName,".txt",sep=""))
            else if (substring(SpotTypesFileName,len-3,len)!=".txt")
                  Try(SpotTypesFileName <- paste(SpotTypesFileName,".txt",sep=""))
            Try(SpotTypes <- matrix(nrow=NumRows,ncol=NumCols))
            Try(colnamesSpotTypes <- c())
            if (NumCols>0)
              Try(for (j in (1:NumCols))
                colnamesSpotTypes[j] <- tclvalue(paste(tclArrayName,"(0,",j-1,")",sep="")))
            Try(colnames(SpotTypes) <- colnamesSpotTypes)       
            if (NumRows>0 && NumCols>0)
              Try(for (i in (1:NumRows))
                 for (j in (1:NumCols))
                    SpotTypes[i,j] <- tclvalue(paste(tclArrayName,"(",i,",",j-1,")",sep="")))

            Try(write.table(SpotTypes,file=SpotTypesFileName,sep="\t",quote=FALSE,col.names=TRUE,row.names=FALSE))
          }

          addRowFcn <- function()
          {         
            Try(tkinsert(table1,"rows","end","1"))
          }
          
          insertRowFcn <- function()
          {         
            if (data.class(result<-try(tkinsert(table1,"rows",paste(as.integer(tclvalue(tkindex(table1,"active","row")))),"-1"),TRUE))=="try-error")
            {
              Try(errorMessage <- strstr(as.character(result),"[tcl]"))
              Try(if (errorMessage=="")
                errorMessage <- result)
              Try(tkmessageBox(title="Failed to Insert Row",message=errorMessage,icon="error",type="ok"))
            }
          }
          
          deleteLastRowFcn <- function()
          {
            Try(tkdelete(table1,"rows","end","1"))  
          }
          
          deleteCurrentRowFcn <- function()
          {
            if (data.class(result<-try(tkdelete(table1,"rows",tclvalue(tkindex(table1,"active","row")),"1"),TRUE))=="try-error")
            {
              Try(errorMessage <- strstr(as.character(result),"[tcl]"))
              Try(if (errorMessage=="")
                errorMessage <- result)
              Try(tkmessageBox(title="Failed to Delete Row",message=errorMessage,icon="error",type="ok"))
            }
          }

          Try(topMenu <- tkmenu(ttViewSpotTypes, tearoff=FALSE))

          Try(topMenu <- tkmenu(ttViewSpotTypes, tearoff=FALSE))
          Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
          Try(tkadd(fileMenu, "command", label="Open",      command=openSpotTypesFile)) # ) # ,font=limmaGUIfontMenu))
          Try(tkadd(fileMenu, "command", label="Save As",   command=saveSpotTypesFile)) # ) # ,font=limmaGUIfontMenu))
          Try(tkadd(topMenu,  "cascade", label="File",menu=fileMenu)) # ) # ,font=limmaGUIfontMenu))  

          Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
          Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",      command=copyFcn)) # ) # ,font=limmaGUIfontMenu))
          Try(tkadd(topMenu,  "cascade", label="Edit",menu=editMenu)) # ) # ,font=limmaGUIfontMenu))  
          
          Try(rowsMenu <- tkmenu(topMenu, tearoff=FALSE))
          Try(tkadd(rowsMenu, "command", label="Add Row",                            command=addRowFcn))
          Try(tkadd(rowsMenu, "command", label="Insert Row before Current Row",      command=insertRowFcn))          
          Try(tkadd(rowsMenu, "separator"))
          Try(tkadd(rowsMenu, "command", label="Delete Last Row",                    command=deleteLastRowFcn))
          Try(tkadd(rowsMenu, "command", label="Delete Current Row",                 command=deleteCurrentRowFcn))          
          Try(tkadd(topMenu,  "cascade", label="Rows",menu=rowsMenu))
          
          Try(columnsMenu <- tkmenu(topMenu, tearoff=FALSE))
          Try(editableColumnHeadings <- tclVar("0"))
          Try(tkadd(columnsMenu, "checkbutton",variable=editableColumnHeadings,label="Editable Column Headings",
                command=function() {if (tclvalue(editableColumnHeadings)=="1") tkconfigure(table1,titlerows=0) else tkconfigure(table1,titlerows=1)}))
          Try(tkadd(topMenu,  "cascade", label="Columns",menu=columnsMenu))
          
          Try(tkconfigure(ttViewSpotTypes,menu=topMenu))

          Try(BlankLabel1<-tklabel(ttViewSpotTypes,text="    "))
          Try(tkgrid(BlankLabel1))
          Try(BlankLabel2<-tklabel(ttViewSpotTypes,text="    "))
          Try(OKCancelFrame <- tkframe(ttViewSpotTypes)) 
          Try(OK.but <-tkbutton(OKCancelFrame,text="   OK   ",command=onOK,font=limmaGUIfont2))
          Try(Cancel.but <-tkbutton(OKCancelFrame,text=" Cancel ",command=onCancel,font=limmaGUIfont2))      
          Try(tkgrid(OK.but,Cancel.but))
          Try(tkgrid.configure(OK.but,sticky="e"))
          Try(tkgrid.configure(Cancel.but,sticky="w"))          
          Try(tkgrid(OKCancelFrame))
          Try(BlankLabel3<-tklabel(ttViewSpotTypes,text="    "))
          Try(tkgrid(BlankLabel3))

          Try(tkfocus(ttViewSpotTypes))
          Try(tkbind(ttViewSpotTypes, "<Destroy>", function() {Try(tkgrab.release(ttViewSpotTypes));Try(tkfocus(ttMain))}))
          Try(tkwait.window(ttViewSpotTypes))
}



ComputeContrasts <- function()
{
  # If we have a connected design, the number of parameters to be estimated initially is NumRNATypes - 1,
  # and the total number of possible contrasts is NumRNATypes Choose 2, so the number of additional contrasts
  # not covered in the Main Linear Model Fit is NumRNATypesChoose2 - (NumRNATypes-1).  This will be the default
  # number of columns of the contrasts matrix (for a connected design), unless this number is greater than 6.
  # In that case, it will be limited to 6 columns, and the user will have to add extra columns themselves.
  # (6 columns is  [ 5 Choose 2 - (5-1)  = 6 ] i.e. it is sufficient for 5 RNA types in a connected design.

  # For an unconnected design, the number of parameters estimated in the main linear model fit is 
  # NumRNATypes - NumSubGraphs (where the empty graph is not counted as a subgraph, and if there is 
  # only one connected graph, it is counted as one subgraph).
  # The total number of contrasts in this case is the sum of each of the numbers of contrasts for each
  # subgraph.  For each subgraph, the total number of contrasts is :
  # NumRNATypesSubgraph Choose 2, so the default number of columns of the contrasts matrix is :
  # TotalNumberOfContrasts = SUM_i ( NumRNATtypesSubgraph_i Choose 2 ) - (NumRNATTypes-NumSubGraphs)
  # But again we will limit it to a maximum of 6 for displaying the initial Tktable (but the user can add more).

  # In the same way as the GUI for entering the design matrix, we can have a simple drop-down interface and
  # an Advanced button.

  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))
  Try(Targets <- get("Targets",envir=limmaGUIenvironment))
  Try(NumRNATypes <- get("NumRNATypes",envir=limmaGUIenvironment))
  Try(numConnectedSubGraphs <- get("numConnectedSubGraphs",envir=limmaGUIenvironment))    
  Try(connectedSubGraphs <- get("connectedSubGraphs",envir=limmaGUIenvironment))        
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))  
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment)) 
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))   

  if (ArraysLoaded==FALSE)
  {
      Try(tkmessageBox(title="Compute Contrasts",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(ttMain))
      return()
  }
  
  if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="Compute Contrasts",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  }  
  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return())
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(fit <- (ParameterizationList[[ParameterizationNameNode]])$fit)

  Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))

  if (Try(LinearModelComputed[parameterizationIndex]==FALSE))
  {
      Try(tkmessageBox(title="Compute Contrasts",message=paste("No linear model fit is available for ",ParameterizationNamesVec[parameterizationIndex],".  Please try Compute Linear Model from the Linear Model menu.",sep=""),type="ok",icon="error"))
      Try(tkfocus(ttMain))
      return()
  }

#  Try(tkmessageBox(message=paste("NumRNATypes :",NumRNATypes)))
#  Try(tkmessageBox(message=paste("NumParameters :",NumParameters)))
#  Try(tkmessageBox(message=paste("numConnectedSubGraphs :",numConnectedSubGraphs)))

  # The unique(vec) function in R returns a vector with duplicate entries removed.    
  Try(RNATypesInSubGraph <- c())
  Try(NumContrastsInSubGraph <- c())  
  Try(TotalNumContrasts <- 0)
  Try(for (i in (1:numConnectedSubGraphs))
  {
    Try(RNATypesInSubGraph[i] <- length(unique(connectedSubGraphs[[i]])))
    Try(NumContrastsInSubGraph[i] <- choose(RNATypesInSubGraph[i],2))
    Try(TotalNumContrasts <- TotalNumContrasts + NumContrastsInSubGraph[i])
#    Try(tkmessageBox(message=paste("RNATypesInSubGraph[",i,"] : ",RNATypesInSubGraph[i],sep="")))
#    for (j in (1:RNATypesInSubGraph[i]))
#      tkmessageBox(message=paste("RNATypesInSubgraph[",i,"][",j,"] = ",connectedSubGraphs[[i]][j],sep=""))
#    Try(tkmessageBox(message=paste("NumContrastsInSubGraph[",i,"] : ",NumContrastsInSubGraph[i],sep="")))    
  })
  
  Try(NumContrasts <- TotalNumContrasts - NumParameters)

#  Try(tkmessageBox(message=paste("TotalNumContrasts :",TotalNumContrasts)))    
#  Try(tkmessageBox(message=paste("NumContrasts :",NumContrasts)))  
  
  Try(if (NumContrasts<=0)
  {
    tkmessageBox(title="Compute Contrasts",message=paste("For this experiment, all possible RNA comparisons can be obtained from one parameterization.  There is no need to compute contrasts.",
                         "\n\nThis is either because there are only 2 RNA types, or there is an unconnected experimental design and there only 2 RNA types within each sub-experiment."),type="ok",icon="error")
    Try(tkfocus(ttMain))
    return()
  })
  
  Try(NumContrasts <- min(NumContrasts,6))
  
  Try(contrastsMatrixInList <- GetDesignOrContrasts(Contrasts=TRUE,NumContrasts=NumContrasts,parameterizationIndex=parameterizationIndex))  
  Try(if (nrow(contrastsMatrixInList$contrasts)==0) return())
  Try(contrastsMatrix <- as.matrix(contrastsMatrixInList$contrasts))
  tkconfigure(ttMain,cursor="watch")     
  Try(contrastsFit <- contrasts.fit(fit,contrastsMatrix))
  Try(contrastsEbayes <- ebayes(contrastsFit))
  tkconfigure(ttMain,cursor="arrow")       
  Try(contrastsParameterizationNameText <- GetContrastsParameterizationName())  
  Try(if (contrastsParameterizationNameText=="GetContrastsParameterizationName.CANCEL") return())
  tkconfigure(ttMain,cursor="watch")     
  Try(while (nchar(contrastsParameterizationNameText)==0)
  {
      Try(tkmessageBox(title="Contrasts Name",message="Please enter a name for this set of contrasts",type="ok",icon="error"))
      Try(contrastsParameterizationNameText <- GetContrastsParameterizationName())
      if (contrastsParameterizationNameText=="GetContrastsParameterizationName.CANCEL") 
      {
          Try(tkfocus(ttMain))          
          return()
      }
  })
  
  Try(ContrastsParameterizationNamesVec <- GetContrastsParameterizationNames(parameterizationTreeIndex))

  Try(contrastsParameterizationIndex <- 0)
  Try(newContrastsParameterization <- 1)
  Try(if (contrastsParameterizationNameText %in% ContrastsParameterizationNamesVec)
  {
      Try(contrastsParameterizationIndex <- match(contrastsParameterizationNameText,ContrastsParameterizationNamesVec))
      Try(mbVal<-tclvalue(tkmessageBox(title="Contrasts Parameterization Name",message="This contrasts parameterization name already exists.  Replace?",type="yesnocancel",icon="question")))
      Try(if (mbVal=="cancel") return())
      Try(if (mbVal=="yes") newContrastsParameterization <- 0)
      Try(if (mbVal=="no") newContrastsParameterization <- 1)
  }
  else
      Try(newContrastsParameterization <- 1))

  Try(ContrastsParameterizationTreeIndexVec <- (ParameterizationList[[ParameterizationNameNode]])$ContrastsParameterizationTreeIndexVec)

  Try(NumContrastParameterizations <- (ParameterizationList[[ParameterizationNameNode]])$NumContrastParameterizations)

  if (newContrastsParameterization==1)
  {
      Try(if (length(ContrastsParameterizationTreeIndexVec)!=NumContrastParameterizations)
      {
          Try(tkmessageBox(title="Contrasts Parameterizations","Length of ContrastsParameterizationTreeIndexVec is not equal to NumContrastParameterizations.",type="ok",icon="error"))
          Try(tkfocus(ttMain))
          return()
      })
      Try(NumContrastParameterizations <- NumContrastParameterizations + 1)
      Try(contrastsParameterizationIndex <- NumContrastParameterizations)
      Try(if (length(ContrastsParameterizationTreeIndexVec)==0)
          Try(contrastsParameterizationTreeIndex <- 1)
      else
          Try(contrastsParameterizationTreeIndex <- max(ContrastsParameterizationTreeIndexVec)+1))
      Try(ContrastsParameterizationTreeIndexVec[contrastsParameterizationIndex] <- contrastsParameterizationTreeIndex)

      Try(ContrastsParameterizationNamesVec <- c(ContrastsParameterizationNamesVec,contrastsParameterizationNameText))
      
      
      Try(ContrastsParameterizationNamesNode <- paste("ContrastsParameterizationNames.",parameterizationTreeIndex,".",contrastsParameterizationTreeIndex,sep=""))
      Try(ParameterizationList[[ParameterizationNameNode]][[ContrastsParameterizationNamesNode]] <- contrastsParameterizationNameText)  
  }
  else # Replace existing contrasts parameterization with the same name.
  {
      Try(contrastsParameterizationTreeIndex <- ContrastsParameterizationTreeIndexVec[contrastsParameterizationIndex])
      Try(tkdelete(ParameterizationTREE,paste("ContrastsParameterizationName.",parameterizationTreeIndex,".",contrastsParameterizationTreeIndex,sep="")))
  }  
  
  Try(ParamContrastsNode <- paste("ParamContrasts.",parameterizationTreeIndex,sep=""))
  
  Try(ContrastsParameterizationNamesVec <- c(ContrastsParameterizationNamesVec,contrastsParameterizationNameText))

  Try(ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations <- NumContrastParameterizations)

  Try(ContrastsParameterizationNamesNode <- paste("ContrastsParameterizationNames.",parameterizationTreeIndex,".",contrastsParameterizationTreeIndex,sep=""))

  Try(ParameterizationList[[ParameterizationNameNode]][["Contrasts"]][[ContrastsParameterizationNamesNode]] <-
        list(fit=contrastsFit,eb=contrastsEbayes,contrastsMatrixInList=contrastsMatrixInList,contrastsParameterizationNameText=contrastsParameterizationNameText))
  
  Try(ParamContrastsNode <- paste("ParamContrasts.",parameterizationTreeIndex,sep=""))
  Try(ContrastsParameterizationNamesNode <- paste("ContrastsParameterizationNames.",parameterizationTreeIndex,".",contrastsParameterizationTreeIndex,sep=""))
  
  if (NumContrastParameterizations>0)
    Try(ContrastsNames <- colnames(contrastsMatrix))

  if (NumContrastParameterizations==1)
  {
    # This is the first one, so we have to delete "none"
    Try(NoneNode <- paste("ContrastsParameterizationNames.",parameterizationTreeIndex,".1",sep=""))  
    Try(tkdelete(ParameterizationTREE,NoneNode))
  }  
  Try(ContrastsParameterizationNamesNode <- paste("ContrastsParameterizationNames.",parameterizationTreeIndex,".",contrastsParameterizationTreeIndex,sep=""))  
  Try(tkinsert(ParameterizationTREE,"end",ParamContrastsNode,ContrastsParameterizationNamesNode,text=contrastsParameterizationNameText,font=limmaGUIfontTree))
  Try(NumContrastsInContrastParameterization <- length(ContrastsNames))
  Try(for (j in (1:NumContrastsInContrastParameterization))
    Try(tkinsert(ParameterizationTREE,"end",ContrastsParameterizationNamesNode,paste("Contrasts.",parameterizationIndex,".",contrastsParameterizationTreeIndex,".",j,sep=""),text=ContrastsNames[j],font=limmaGUIfontTree)))          
  
  Try(ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations <- NumContrastParameterizations)
  Try(ParameterizationList[[ParameterizationNameNode]]$ContrastsParameterizationTreeIndexVec <- ContrastsParameterizationTreeIndexVec)

  Try(assign("ParameterizationList",ParameterizationList,limmaGUIenvironment))
  tkconfigure(ttMain,cursor="arrow")       
}

ComputeLinearModelFit <- function()
{
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))        
  Try(ndups   <- get("ndups",envir=limmaGUIenvironment))
  Try(spacing <- get("spacing",envir=limmaGUIenvironment))  
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment)) 
  Try(WeightingType <- get("WeightingType",envir=limmaGUIenvironment))
  Try(SpotTypeStatus <- get("SpotTypeStatus",envir=limmaGUIenvironment))  
  Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
  Try(RG <- get("RG",envir=limmaGUIenvironment))
  Try(numSpotTypes <- nrow(SpotTypes))
  
  if (ArraysLoaded==FALSE)
  {
      Try(tkmessageBox(title="Linear Model",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(ttMain))
      return()
  }
  
  SetLayoutParamReturnVal<-1
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))  
  if (length(maLayout)==0) SetLayoutParamReturnVal <-Try(SetLayoutParameters())
  if (SetLayoutParamReturnVal==0) return()  
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))    
  if (NumParameterizations>0)
  {
      Try(NewParameterizationMB <-tkmessageBox(title="Linear Model",message="Create a new parameterization?",type="yesnocancel",icon="question",default="yes"))
      Try(WhetherToCreateNewParameterization <- tclvalue(NewParameterizationMB))
  }
  else
      Try(WhetherToCreateNewParameterization <- "yes")

  if (WhetherToCreateNewParameterization=="cancel")
      return()
  
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  
  if (WhetherToCreateNewParameterization=="yes")
  {
    Try(InitNewParameterizationReturnVal <- InitNewParameterization())
    Try(if (InitNewParameterizationReturnVal$result=="cancel") return())
    Try(parameterizationIndex <- InitNewParameterizationReturnVal$parameterizationIndex)
    Try(parameterizationTreeIndex <- InitNewParameterizationReturnVal$parameterizationTreeIndex)
    Try(ParameterizationNameText <- InitNewParameterizationReturnVal$ParameterizationNameText) 
    Try(ParameterNamesVec <- InitNewParameterizationReturnVal$ParameterNamesVec)
    Try(designList <- InitNewParameterizationReturnVal$designList)
    Try(design <- designList$design) 
    Try(SpotTypesForLinearModel <- InitNewParameterizationReturnVal$SpotTypesForLinearModel)

    Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
    Try(ParameterizationNamesVec     <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
    Try(ParameterizationList         <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(NumParameterizations         <- get("NumParameterizations",envir=limmaGUIenvironment))
  
  }
  else # The user said, "No, don't create a new parameterization"
  {
    if (NumParameterizations==0)
    {
      Try(tkmessageBox(title="Use Existing Parameterization",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
      Try(tkfocus(ttMain))
      return()  
    }  
    Try(parameterizationIndex <- ChooseParameterization())
    Try(if (parameterizationIndex==0)    return())
    Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
    Try(designList <-               (ParameterizationList[[ParameterizationNameNode]])$designList)  
    Try(design <- designList$design)
    Try(ParameterizationNameText <- (ParameterizationList[[ParameterizationNameNode]])[[ParameterizationNameNode]])      
    Try(ParameterNamesVec <- colnames(design))
    Try(SpotTypesForLinearModel <- ParameterizationList[[ParameterizationNameNode]]$SpotTypesForLinearModel)

    Try(tkdelete(ParameterizationTREE,paste("ParameterizationName.",parameterizationTreeIndex,sep="")))
    Try(ParameterizationList <- deleteItemFromList(ParameterizationList,paste("ParameterizationName.",parameterizationTreeIndex,sep="")))
    Try(ParameterizationList[[ParameterizationNameNode]] <- list())
  }
  
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(tkfocus(ttMain))

  Try(CreateTreeAndList(parameterizationIndex,parameterizationTreeIndex,
    ParameterizationNameText,ParameterNamesVec,designList,SpotTypesForLinearModel))

  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec     <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList         <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(NumParameterizations         <- get("NumParameterizations",envir=limmaGUIenvironment))

  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(ParamMainFitNode   <- paste("ParamMainFit.",parameterizationTreeIndex,sep=""))
  Try(ParamContrastsNode <- paste("ParamContrasts.",parameterizationTreeIndex,sep=""))
  Try(PMFNoneNode <- paste("PMFNone.",parameterizationTreeIndex,sep=""))
  Try(PMFSpotTypesNode <- paste("PMFSpotTypes.",parameterizationTreeIndex,sep=""))  
  Try(PMFMANormMethodNode <- paste("PMFMANormMethod.",parameterizationTreeIndex,sep=""))    
  Try(PMFParamsNode <- paste("PMFParams.",parameterizationTreeIndex,sep=""))
  Try(PMFLinModNode <- paste("PMFLinMod.",parameterizationTreeIndex,sep=""))
  Try(PMFLinModStatusNode <- paste("PMFLinModStatus.",parameterizationTreeIndex,sep=""))
  Try(PMFEBayesNode <- paste("PMFEBayes.",parameterizationTreeIndex,sep=""))
  Try(PMFEBayesStatusNode <- paste("PMFEBayesStatus.",parameterizationTreeIndex,sep=""))
  Try(PMFDupCorNode <- paste("PMFDupCor.",parameterizationTreeIndex,sep=""))
  Try(PMFDupCorStatusNode <- paste("PMFDupCorStatus.",parameterizationTreeIndex,sep=""))

  Try(tkconfigure(ttMain,cursor="arrow"))
  
  Try(NormalizeWithinArraysMB <-tkmessageBox(title="Normalization Within Arrays",message="Normalize Within Arrays?",type="yesnocancel",icon="question",default="yes"))
  Try(WhetherToNormalizeWithinArrays <- tclvalue(NormalizeWithinArraysMB))
  if (WhetherToNormalizeWithinArrays=="cancel")
      return()
  if (WhetherToNormalizeWithinArrays=="yes")
    Try(GetNormalizationMethod())
  Try(NormalizeBetweenArraysMB <-tkmessageBox(title="Normalization Between Arrays",message="Normalize Between Arrays?",type="yesnocancel",icon="question",default="no"))
  Try(WhetherToNormalizeBetweenArrays <- tclvalue(NormalizeBetweenArraysMB))
  if (WhetherToNormalizeBetweenArrays=="cancel")
      return()

  Try(if (!exists("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
  {
    Try(WithinArrayNormalizationMethod <- "printtiploess")
    Try(assign("WithinArrayNormalizationMethod",WithinArrayNormalizationMethod,limmaGUIenvironment))
  })
  Try(WithinArrayNormalizationMethod <- get("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
  Try(ParameterizationList[[ParameterizationNameNode]][["WithinArrayNormalizationMethod"]] <- WithinArrayNormalizationMethod)
  Try(ParameterizationList[[ParameterizationNameNode]][["WhetherToNormalizeWithinArrays"]] <- WhetherToNormalizeWithinArrays)
  Try(ParameterizationList[[ParameterizationNameNode]][["WhetherToNormalizeBetweenArrays"]] <- WhetherToNormalizeBetweenArrays)

  Try(MANormMethodValueNode <- paste("PMFMANormMethod.",parameterizationTreeIndex,".",1,sep=""))
  Try(ParameterizationList[[ParameterizationNameNode]] <- 
            deleteItemFromList(ParameterizationList[[ParameterizationNameNode]],MANormMethodValueNode))
  Try(tkdelete(ParameterizationTREE,MANormMethodValueNode))
  Try(if (WhetherToNormalizeWithinArrays=="yes" && WhetherToNormalizeBetweenArrays=="yes")
        MAnormalizationMethod <- paste("Within arrays (",WithinArrayNormalizationMethod,") and between arrays",sep=""))
  Try(if (WhetherToNormalizeWithinArrays=="yes" && WhetherToNormalizeBetweenArrays!="yes")
        MAnormalizationMethod <- paste("Within arrays (",WithinArrayNormalizationMethod,") only",sep=""))
  Try(if (WhetherToNormalizeWithinArrays!="yes" && WhetherToNormalizeBetweenArrays=="yes")
        MAnormalizationMethod <- "Between arrays only")
  Try(if (WhetherToNormalizeWithinArrays!="yes" && WhetherToNormalizeBetweenArrays!="yes")
        MAnormalizationMethod <- "No normalization")
  
  Try(tkinsert(ParameterizationTREE,"end",PMFMANormMethodNode, MANormMethodValueNode,text=MAnormalizationMethod,font=limmaGUIfontTree))
  Try(ParameterizationList[[ParameterizationNameNode]][[MANormMethodValueNode]] <- MAnormalizationMethod)

  ####################################################################################################################
  
  HowManyDupsReturnVal<-1
  Try(HowManyDupsReturnVal <- HowManyDups())
  if (HowManyDupsReturnVal==0) return()

  Try(ndups   <- get("ndups",envir=limmaGUIenvironment))
  Try(spacing <- get("spacing",envir=limmaGUIenvironment))    

  Try(if (ndups>1)
  {
    Try(WhetherToCalculateDuplicateCorrelation <- tclvalue(tkmessageBox(title="Duplicate Correlation",
         message="Should limmaGUI calculate duplicate correlation? (If not, you will need to provide an estimate.)",
         type="yesnocancel",icon="question",default="yes")))
    Try(if (WhetherToCalculateDuplicateCorrelation=="cancel")
      return())
    GetDupCor <- function()
    {
      Try(if ("dupcor" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
        Try(dupcor <- ParameterizationList[[ParameterizationNameNode]]$dupcor)
      else
        dupcor <- 0.75)

      Try(ttGetDupCor<-tktoplevel(ttMain))
      Try(tkwm.deiconify(ttGetDupCor))
      Try(tkgrab.set(ttGetDupCor))
      Try(tkfocus(ttGetDupCor))
      Try(tkwm.title(ttGetDupCor,"Duplicate Correlation Estimate"))
      Try(tkgrid(tklabel(ttGetDupCor,text="    ")))
      Try(DupCorTcl <- tclVar(paste(dupcor)))
      Try(entry.DupCor <-tkentry(ttGetDupCor,width="20",font=limmaGUIfont2,textvariable=DupCorTcl,bg="white"))
      Try(tkgrid(tklabel(ttGetDupCor,text="Enter an estimate for the duplicate correlation.",font=limmaGUIfont2),columnspan=2))
      Try(tkgrid(entry.DupCor,columnspan=2))
      Try(ReturnVal <- "ID_CancelFromGetDupCor")
      onOK <- function()
      {    
        Try(DupCorTxt <- tclvalue(DupCorTcl))
        Try(ReturnVal <<- DupCorTxt)
        Try(tkgrab.release(ttGetDupCor));Try(tkdestroy(ttGetDupCor));Try(tkfocus(ttMain))
      }
      onCancel <- function()
      {    
        Try(ReturnVal <<- "ID_CancelFromGetDupCor")
        Try(tkgrab.release(ttGetDupCor));Try(tkdestroy(ttGetDupCor));Try(tkfocus(ttMain))
      }     
      Try(OK.but <-tkbutton(ttGetDupCor,text="   OK   ",command=onOK,font=limmaGUIfont2))
      Try(Cancel.but <-tkbutton(ttGetDupCor,text=" Cancel ",command=onCancel,font=limmaGUIfont2))
      Try(tkgrid(tklabel(ttGetDupCor,text="    ")))
      Try(tkgrid(OK.but,Cancel.but))
      Try(tkgrid.configure(OK.but,sticky="e"))
      Try(tkgrid.configure(Cancel.but,sticky="w"))
      Try(tkgrid(tklabel(ttGetDupCor,text="       ")))
      Try(tkfocus(entry.DupCor))
      Try(tkbind(entry.DupCor, "<Return>",onOK))
      Try(tkbind(ttGetDupCor, "<Destroy>", function(){Try(tkgrab.release(ttGetDupCor));Try(tkfocus(ttMain));return("ID_CancelFromGetDupCor")}))
      Try(tkwait.window(ttGetDupCor))
      Try(tkfocus(ttMain))
      return (ReturnVal)          
    }
    Try(if (WhetherToCalculateDuplicateCorrelation=="no")    
    {
      Try(dupcorText <- GetDupCor())
      Try(if (dupcorText=="ID_CancelFromGetDupCor") return())
      Try(while (dupcorText=="" || inherits(try(dupcor <- eval(parse(text=dupcorText)),TRUE),"try-error"))
      {
        Try(tkmessageBox(title="Invalid Correlation Estimate",message="Please enter a decimal number for the duplicate correlation estimate.",icon="error",type="ok",default="ok"))
        Try(dupcorText <- GetDupCor())
        Try(if (dupcorText=="ID_CancelFromGetDupCor") return())
      })
      Try(dupcor <- list(cor=as.numeric(dupcorText)))
    })
  })
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(tkfocus(ttMain))

  Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
  Try(if (!exists("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
  {
    Try(WithinArrayNormalizationMethod <- "printtiploess")
    Try(assign("WithinArrayNormalizationMethod",WithinArrayNormalizationMethod,limmaGUIenvironment))
  })
  Try(WithinArrayNormalizationMethod <- get("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))

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
        Try(tkdelete(mainTree,"WithinOnly.Status"))
        Try(tkinsert(mainTree,"end","WithinOnly","WithinOnly.Status" ,text=paste("Available (using ",WithinArrayNormalizationMethod,")",sep=""),font=limmaGUIfontTree))        
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
        Try(tkdelete(mainTree,"Raw.Status"))
        Try(tkinsert(mainTree,"end","Raw","Raw.Status" ,text="Available",font=limmaGUIfontTree))
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
        Try(tkdelete(mainTree,"WithinAndBetween.Status"))
        Try(tkinsert(mainTree,"end","WithinAndBetween","WithinAndBetween.Status" ,text="Available",font=limmaGUIfontTree))                   
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
        Try(tkdelete(mainTree,"BetweenOnly.Status"))
        Try(tkinsert(mainTree,"end","BetweenOnly","BetweenOnly.Status" ,text="Available",font=limmaGUIfontTree))        
      }        
    }
  }      
  Try(MA <- get("MA",envir=limmaGUIenvironment))      
  
  # NOW THAT WE HAVE NORMALIZED, IT IS TIME TO REMOVE SOME OF THE SPOTS (e.g. CONTROLS AND BLANKS)

  Try(SpotTypesIncluded <- c())
  Try(for (i in (1:numSpotTypes))
    if (SpotTypesForLinearModel[i]==TRUE)
      SpotTypesIncluded <- c(SpotTypesIncluded,SpotTypes[i,"SpotType"]))


  Try(oldNumRows <- nrow(MA$M))
  Omit <- ""
  count <- 0
  Try(for (i in (1:numSpotTypes))
  {
    if (SpotTypesForLinearModel[i]==TRUE)
      next()
    count <- count + 1
    if (count>1)
      Omit <-paste(Omit,"|")
    else
      Omit <- "("
    Try(Omit <- paste(Omit," (SpotTypeStatus==\"",SpotTypes[i,"SpotType"],"\")",sep=""))
  })
  
  Try(if (nchar(Omit)>0)
  {
    Omit <- paste(Omit,")")
    Try(Omit <- eval(parse(text=Omit)))  
    MA$M <- MA$M[!Omit,]
    MA$A <- MA$A[!Omit,]  
  })
  
# Below: We shouldn't save MA globally because it depends on the spot-type ommissions of this parameterization.  
#  Try(assign("MA",MA,limmaGUIenvironment)) 
  Try(newNumRows <- nrow(MA$M))
  Try(parameterizationSpacing <- spacing)
  Try(spacingCorrected <- 0)
  Try(if (oldNumRows/spacing==2  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/2;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==3  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/3;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==4  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/4;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==5  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/5;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==6  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/6;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==7  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/7;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==8  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/8;  spacingCorrected <- 1})
  Try(if (oldNumRows/spacing==9  && oldNumRows!=newNumRows) { parameterizationSpacing <- newNumRows/9;  spacingCorrected <- 1})      
  Try(if (oldNumRows/spacing==10 && oldNumRows!=newNumRows){ parameterizationSpacing <- newNumRows/10; spacingCorrected <- 1})
#  Try(if (spacingCorrected==1) tkmessageBox(title="Duplicate Spacing",message="Correcting duplicate spacing to allow for omitted spot types",icon="info"))

  Try(spacing <- parameterizationSpacing) # Local assignment only
  
  # I should delete the above code and just call GetReducedDuplicateSpacing.

  # Calling the A matrix, "A" would risk getting confused with another attribute starting with 'A'.
  Try(ParameterizationList[[ParameterizationNameNode]][["Amatrix"]] <- MA$A)
  
  if (ndups==1)
  {
    Try(fit <- lm.series(MA$M,as.matrix(design)))

    Try(tkdelete(ParameterizationTREE,PMFDupCorStatusNode))  
    Try(ParameterizationList[[ParameterizationNameNode]] <- 
            deleteItemFromList(ParameterizationList[[ParameterizationNameNode]],PMFDupCorStatusNode))
    Try(tkinsert(ParameterizationTREE,"0",PMFDupCorNode,PMFDupCorStatusNode,text="No duplicates",font=limmaGUIfontTree))
    Try(ParameterizationList[[ParameterizationNameNode]] <- deleteItemFromList(ParameterizationList[[ParameterizationNameNode]],PMFDupCorStatusNode))
    Try(ParameterizationList[[ParameterizationNameNode]][[PMFDupCorStatusNode]] <- "No duplicates")

    Try(ParameterizationList[[ParameterizationNameNode]][["dupcor"]] <- 0)
  } else
  {
    Try(if (WhetherToCalculateDuplicateCorrelation=="yes")    # Otherwise user entered it (hopefully).
      Try(dupcor <- dupcor.series(MA$M,as.matrix(design),ndups=ndups,spacing=spacing)))
    Try(tkdelete(ParameterizationTREE,PMFDupCorStatusNode))  
    Try(ParameterizationList[[ParameterizationNameNode]] <- 
            deleteItemFromList(ParameterizationList[[ParameterizationNameNode]],PMFDupCorStatusNode))    
    Try(tkinsert(ParameterizationTREE,"0",PMFDupCorNode,PMFDupCorStatusNode,text=paste(dupcor$cor),font=limmaGUIfontTree))
    Try(ParameterizationList[[ParameterizationNameNode]] <- deleteItemFromList(ParameterizationList[[ParameterizationNameNode]],PMFDupCorStatusNode))
    Try(ParameterizationList[[ParameterizationNameNode]][[PMFDupCorStatusNode]] <- paste(dupcor$cor))

    Try(ParameterizationList[[ParameterizationNameNode]][["dupcor"]] <- dupcor)

    Try(fit <- gls.series(MA$M,as.matrix(design),ndups=ndups,spacing=spacing,correlation=dupcor$cor))
  }

  Try(tkdelete(ParameterizationTREE,PMFLinModStatusNode))  
  Try(ParameterizationList[[ParameterizationNameNode]] <- 
            deleteItemFromList(ParameterizationList[[ParameterizationNameNode]],PMFLinModStatusNode))
  Try(tkinsert(ParameterizationTREE,"0",PMFLinModNode,PMFLinModStatusNode,text="Available",font=limmaGUIfontTree))
  Try(ParameterizationList[[ParameterizationNameNode]] <- deleteItemFromList(ParameterizationList[[ParameterizationNameNode]],PMFLinModStatusNode))
  Try(ParameterizationList[[ParameterizationNameNode]][[PMFLinModStatusNode]] <- "Available")

  Try(ParameterizationList[[ParameterizationNameNode]][["fit"]] <- fit)

  Try(eb <- ebayes(fit))

  Try(tkdelete(ParameterizationTREE,PMFEBayesStatusNode))    
  Try(ParameterizationList[[ParameterizationNameNode]] <- 
            deleteItemFromList(ParameterizationList[[ParameterizationNameNode]],PMFEBayesStatusNode))
  Try(tkinsert(ParameterizationTREE,"0",PMFEBayesNode,PMFEBayesStatusNode,text="Available",font=limmaGUIfontTree))
  Try(ParameterizationList[[ParameterizationNameNode]] <- deleteItemFromList(ParameterizationList[[ParameterizationNameNode]],PMFEBayesStatusNode))
  Try(ParameterizationList[[ParameterizationNameNode]][[PMFEBayesStatusNode]] <- "Available")

  Try(ParameterizationList[[ParameterizationNameNode]][["eb"]] <- eb)
  
  Try(gal <- get("gal",envir=limmaGUIenvironment))
  Try(if (ndups==1) 
  {
   if (nchar(Omit)>0)
     Try(ParameterizationList[[ParameterizationNameNode]][["genelist"]] <- gal[!Omit,])
   else
     Try(ParameterizationList[[ParameterizationNameNode]][["genelist"]] <- gal)
  }
  else  
  {
     if (nchar(Omit)>0)
       Try(ParameterizationList[[ParameterizationNameNode]][["genelist"]] <- uniquegenelist(gal[!Omit,],ndups=ndups,spacing=spacing))
     else
       Try(ParameterizationList[[ParameterizationNameNode]][["genelist"]] <- uniquegenelist(gal,ndups=ndups,spacing=spacing))
  })
  Try(tkconfigure(ttMain,cursor="arrow"))
  Try(LinearModelComputed <- get("LinearModelComputed",envir=limmaGUIenvironment))
  Try(LinearModelComputed[parameterizationIndex] <- TRUE)
  Try(assign("LinearModelComputed",LinearModelComputed,limmaGUIenvironment))

  Try(assign("ParameterizationTreeIndexVec",ParameterizationTreeIndexVec,limmaGUIenvironment))
  Try(assign("ParameterizationNamesVec",ParameterizationNamesVec,limmaGUIenvironment))
  Try(assign("ParameterizationList",ParameterizationList,limmaGUIenvironment))
  Try(assign("NumParameterizations",NumParameterizations,limmaGUIenvironment))

  Try(if (ndups>1)
  {
    Try(msg <- paste("Duplicate Correlation : ",dupcor$cor,sep=""))
    Try(tkmessageBox(title="Duplicate Correlation",message=msg,icon="info",type="ok",default="ok"))
  })  
}

ChooseParameterization <- function()
{
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  
  ttChooseParameterization<-tktoplevel(ttMain)
  tkwm.deiconify(ttChooseParameterization)
  tkgrab.set(ttChooseParameterization)  
  tkfocus(ttChooseParameterization)
  tkwm.title(ttChooseParameterization,"Choose a Parameterization")
  scr <- tkscrollbar(ttChooseParameterization, repeatinterval=5,
                       command=function(...)tkyview(tl,...))
  ## Safest to make sure scr exists before setting yscrollcommand
  tl<-tklistbox(ttChooseParameterization,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white",font=limmaGUIfont2)   
  lbl2<-tklabel(ttChooseParameterization,text="Which parameterization is this for?",font=limmaGUIfont2)
  tkgrid(tklabel(ttChooseParameterization,text="       "),row=0,column=1,columnspan=1)
  tkgrid(tklabel(ttChooseParameterization,text="       "),row=0,column=4,columnspan=1)
  tkgrid(lbl2,row=1,column=2,columnspan=2,rowspan=1);
  tkgrid.configure(lbl2,sticky="w")
  tkgrid(tklabel(ttChooseParameterization,text="         "),row=2,column=1)
  tkgrid(tl,row=2,column=2,columnspan=2,rowspan=4,sticky="ew")
  tkgrid(scr,row=2,column=3,columnspan=1,rowspan=4,sticky="wns")
  if (NumParameterizations>0)
    for (i in (1:NumParameterizations))
       tkinsert(tl,"end",ParameterizationNamesVec[i])
  tkselection.set(tl,0)

  ReturnVal <- 0
  onOK <- function()
  {
      Try(parameterizationIndex <- as.numeric(tclvalue(tkcurselection(tl)))+1)
      Try(tkgrab.release(ttChooseParameterization));Try(tkdestroy(ttChooseParameterization));Try(tkfocus(ttMain))
      ReturnVal <<- parameterizationIndex
  }
  onCancel <- function() {Try(tkgrab.release(ttChooseParameterization));Try(tkdestroy(ttChooseParameterization));Try(tkfocus(ttMain));ReturnVal <<- 0}
  OK.but <-tkbutton(ttChooseParameterization,text="   OK   ",command=onOK,font=limmaGUIfont2)
  Cancel.but <-tkbutton(ttChooseParameterization,text=" Cancel ",command=onCancel,font=limmaGUIfont2)
  tkgrid(tklabel(ttChooseParameterization,text="    "))
  tkgrid(tklabel(ttChooseParameterization,text="    "),tklabel(ttChooseParameterization,text="    "),OK.but,Cancel.but)
  tkgrid.configure(OK.but,    sticky="e")
  tkgrid.configure(Cancel.but,sticky="w")
  Try(tkbind(OK.but, "<Return>",onOK))  
  Try(tkbind(tl, "<Return>",onOK))    
  Try(tkbind(Cancel.but, "<Return>",onCancel))    
  tkgrid(tklabel(ttChooseParameterization,text="    "))
  Try(tkfocus(ttChooseParameterization))
  Try(tkbind(ttChooseParameterization, "<Destroy>", function() {Try(tkgrab.release(ttChooseParameterization));Try(tkfocus(ttMain))}))
  Try(tkwait.window(ttChooseParameterization))

  return (ReturnVal)
}


GetCoef <- function(parameterizationTreeIndex,whichCoef="onlyOne")
{
  # Firstly, NumParameters in main fit:
  Try(NumParameters     <- get("NumParameters",    envir=limmaGUIenvironment))        

  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))    
  Try(NumContrastParameterizations <- ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations)
  Try(ContrastsParameterizationNamesVec <- c() )
  Try(contrastNames <- list())
  Try (if (NumContrastParameterizations>0)
    Try(for (i in (1:NumContrastParameterizations))
    {
      Try(ContrastsParameterizationNamesVec[i] <- ParameterizationList[[ParameterizationNameNode]]$Contrasts[[i]]$contrastsParameterizationNameText)
      Try(contrastsMatrixInList <- ParameterizationList[[ParameterizationNameNode]]$Contrasts[[i]]$contrastsMatrixInList)
      Try(contrastsMatrix <- contrastsMatrixInList$contrasts)
      Try(contrastNames[[i]] <- colnames(contrastsMatrix))
    }))

  Try(ttGetCoef<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttGetCoef))
  Try(tkgrab.set(ttGetCoef)  )
  Try(tkfocus(ttGetCoef))
  Try(if (whichCoef=="onlyOne")    tkwm.title(ttGetCoef,"Choose a parameter"))
  Try(if (whichCoef=="heat")       tkwm.title(ttGetCoef,"Choose a parameter"))  
  Try(if (whichCoef=="first")      tkwm.title(ttGetCoef,"Choose the first parameter"))
  Try(if (whichCoef=="second")     tkwm.title(ttGetCoef,"Choose the second parameter"))            
  Try(scr <- tkscrollbar(ttGetCoef, repeatinterval=5,
                       command=function(...)tkyview(tl,...)))
  Try(xscr <- tkscrollbar(ttGetCoef, repeatinterval=5,
                       command=function(...)tkxview(tl,...) ,orient="horizontal"))                       
  ## Safest to make sure scr exists before setting yscrollcommand
  Try(tl<-tklistbox(ttGetCoef,height=4,selectmode="single",xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(scr,...),background="white",font=limmaGUIfont2)   )
  Try(if (whichCoef=="heat")
      lbl2<-tklabel(ttGetCoef,text="Draw a heat diagram relative to which parameter/contrast?",font=limmaGUIfont2))
  Try(if (whichCoef=="onlyOne")
      lbl2<-tklabel(ttGetCoef,text="Which comparison parameter is this for?",font=limmaGUIfont2))
  Try(tkgrid(tklabel(ttGetCoef,text="       "),row=0,column=1,columnspan=1))
  Try(if (whichCoef=="first")
      lbl2<-tklabel(ttGetCoef,text="Please select the first parameter            ",font=limmaGUIfont2)  )
  Try(if (whichCoef=="second")
      lbl2<-tklabel(ttGetCoef,text="Please select the second parameter            ",font=limmaGUIfont2)        )
  Try(tkgrid(tklabel(ttGetCoef,text="       "),row=0,column=4,columnspan=1))
  Try(tkgrid(lbl2,row=1,column=2,columnspan=2,rowspan=1))
  Try(tkgrid.configure(lbl2,sticky="w"))
  Try(tkgrid(tklabel(ttGetCoef,text="         "),row=2,column=1))
  Try(tkgrid(tl,row=2,column=2,columnspan=2,rowspan=4,sticky="ew"))
  Try(tkgrid(scr,row=2,column=4,columnspan=1,rowspan=4,sticky="wns"))
  Try(tkgrid(xscr,row=6,column=2,columnspan=2,sticky="wne"))
  
  Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))

  coefIndexList <- list()

  if (NumParameters>0)
    Try(for (i in (1:NumParameters))     
    {
      Try(tkinsert(tl,"end",ParameterNamesVec[i]))
      Try(coefIndexList[[i]] <- list(coefOrContrastIndex=i,ContrastParameterizationIndex=0))
    })
  Try(currentIndex <- NumParameters+1)
  Try(if (NumContrastParameterizations>0)
    Try(for (i in (1:NumContrastParameterizations))
    {      
      Try(for (j in (1:length(contrastNames[[i]])))
      {
        Try(tkinsert(tl,"end",paste(contrastNames[[i]][j]," [",ContrastsParameterizationNamesVec[i],"]",sep="")))
        Try(coefIndexList[[currentIndex]] <- list(coefOrContrastIndex=j,ContrastParameterizationIndex=i))        
        Try(currentIndex <- currentIndex + 1)
      })
     }))


  Try(tkselection.set(tl,0))

  Try(ReturnVal <- list(coefIndex=0,parameterIsFromMainFit=TRUE,coefIndexList=coefIndexList))
  onOK <- function()
  {
      Try(parameterNum <- as.numeric(tclvalue(tkcurselection(tl)))+1)
      Try(tkgrab.release(ttGetCoef));Try(tkdestroy(ttGetCoef));Try(tkfocus(ttMain))
      Try(if (parameterNum<=NumParameters)
        Try(ReturnVal <<- list(coefIndex=parameterNum,parameterIsFromMainFit=TRUE,coefIndexList=coefIndexList))
      else
        Try(ReturnVal <<- list(coefIndex=parameterNum,parameterIsFromMainFit=FALSE,coefIndexList=coefIndexList)))
  }
  onCancel <- function() {Try(tkgrab.release(ttGetCoef));Try(tkdestroy(ttGetCoef));Try(tkfocus(ttMain));Try(ReturnVal <<- list(coefIndex=0,parameterIsFromMainFit=TRUE,coefIndexList=coefIndexList))}
  Try(OK.but <-tkbutton(ttGetCoef,text="   OK   ",command=onOK,font=limmaGUIfont2))
  Try(Cancel.but <-tkbutton(ttGetCoef,text=" Cancel ",command=onCancel,font=limmaGUIfont2))
  Try(tkgrid(tklabel(ttGetCoef,text="    ")))
  Try(tkgrid(tklabel(ttGetCoef,text="    "),tklabel(ttGetCoef,text="    "),OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))  
  Try(tkbind(OK.but, "<Return>",onOK))
  Try(tkbind(tl, "<Return>",onOK))    
  Try(tkbind(Cancel.but, "<Return>",onCancel))      
  Try(tkgrid(tklabel(ttGetCoef,text="    ")))
  Try(tkfocus(ttGetCoef))
  Try(tkbind(ttGetCoef, "<Destroy>", function() {Try(tkgrab.release(ttGetCoef));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttGetCoef))

  return (ReturnVal)
}



showTopTable <- function()
{
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))  
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment)) 
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))   
  Try(ndups   <- get("ndups",envir=limmaGUIenvironment))
  Try(spacing   <- get("spacing",envir=limmaGUIenvironment))
  Try(NumSlides <- get("NumSlides",envir=limmaGUIenvironment))
  Try(gal <- get("gal",envir=limmaGUIenvironment))
  Try(SlideNamesVec <- get("SlideNamesVec",envir=limmaGUIenvironment)) # for NumSlides=1
  
  if (ArraysLoaded==FALSE)
  {
      tkmessageBox(title="Top Table",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error")
      Try(tkfocus(ttMain))
      return()
  }
  
  if (NumParameterizations==0 && NumSlides>1)
  {
    Try(tkmessageBox(title="Top Table",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  }  
  
  Try(if (NumSlides>1)
  {
    Try(parameterizationIndex <- ChooseParameterization())
    Try(if (parameterizationIndex==0)    return())
    Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])

    Try(spacing <- GetReducedDuplicateSpacing(parameterizationTreeIndex))
  
    Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))

    if (Try(LinearModelComputed[parameterizationIndex]==FALSE))
    {
      Try(tkmessageBox(title="Top Table",message=paste("No linear model fit is available for ",ParameterizationNamesVec[parameterizationIndex],".  Please select \"Compute Linear Model Fit\" from the Linear Model menu.",sep=""),type="ok",icon="error"))
      Try(tkfocus(ttMain))
      return()
    }
  })

  GALFile <- get("GALFile",envir=limmaGUIenvironment)
  if (!nchar(GALFile))  
  {
    tkmb <- tkmessageBox(title="ERROR",message="Please select a GenePix Array List (GAL) file first, e.g. fish.gal",icon="warning",type="ok",default="ok")
    return()  
  }


  Try(if (NumSlides>1)
  {
    GetCoefReturnVal <- GetCoef(parameterizationTreeIndex)
    if (GetCoefReturnVal$coefIndex==0) return()
    Try(coef <- (GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]])$coefOrContrastIndex)
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
    Try(ContrastParameterizationIndex <- GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]]$ContrastParameterizationIndex)
    Try(if ("genelist" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
      Try(genelist <- (ParameterizationList[[ParameterizationNameNode]])$genelist)
    else
      Try(genelist <- get("genelist",limmaGUIenvironment)))   
    Try(Amatrix <- NULL)
    Try(if ("Amatrix" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
        Amatrix <- (ParameterizationList[[ParameterizationNameNode]])$Amatrix)
    if (GetCoefReturnVal$parameterIsFromMainFit)
    {
      Try(fit <- (ParameterizationList[[ParameterizationNameNode]])$fit)
      Try(eb  <- (ParameterizationList[[ParameterizationNameNode]])$eb)    
    }
    else
    {
      Try(fit <- ((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex]])$fit)
      Try(eb  <- ((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex]])$eb)    
      Try(ParameterNamesVec <- colnames(((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex]])$contrastsMatrixInList$contrasts))
      Try(NumParameters <- length(ParameterNamesVec))    
    }
  })
  
  Try(ttToptableDialog<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttToptableDialog))
  Try(tkgrab.set(ttToptableDialog))
  Try(tkfocus(ttToptableDialog))
  Try(tkwm.title(ttToptableDialog,"Toptable Options"))
  Try(tkgrid(tklabel(ttToptableDialog,text="    ")))
    
  Try(frame1 <- tkframe(ttToptableDialog,relief="groove",borderwidth=2))
  Try(HowManyQuestion1 <- tklabel(frame1,text="Number of genes in table:",font=limmaGUIfont2))
  Try(tkgrid(HowManyQuestion1))
  Try(tkgrid.configure(HowManyQuestion1,columnspan=2,sticky="w"))
  
  Try(numberOfGenesTcl <- tclVar("3"))
  Try(Ten.but      <- tkradiobutton(frame1,text="10",variable=numberOfGenesTcl,value="1",font=limmaGUIfont2))
  Try(Thirty.but   <- tkradiobutton(frame1,text="30",variable=numberOfGenesTcl,value="2",font=limmaGUIfont2))
  Try(Fifty.but    <- tkradiobutton(frame1,text="50",variable=numberOfGenesTcl,value="3",font=limmaGUIfont2))  
  Try(Hundred.but  <- tkradiobutton(frame1,text="100",variable=numberOfGenesTcl,value="4",font=limmaGUIfont2))    
  Try(AllGenes.but <- tkradiobutton(frame1,text="All genes",variable=numberOfGenesTcl,value="5",font=limmaGUIfont2))      
  
  Try(tkgrid(Ten.but,sticky="w"))
  Try(tkgrid(Thirty.but,sticky="w"))
  Try(tkgrid(Fifty.but,sticky="w"))
  Try(tkgrid(Hundred.but,sticky="w"))  
  Try(tkgrid(AllGenes.but,sticky="w"))  
  Try(tkgrid.configure(HowManyQuestion1,Ten.but,Thirty.but,Fifty.but,Hundred.but,AllGenes.but,sticky="w"))
#  Try(tkgrid(frame1))
  

  Try(frame2 <- tkframe(ttToptableDialog,relief="groove",borderwidth=2))
#  Try(tkgrid(tklabel(frame2,text="        ")))
  Try(sortByLabel <- tklabel(frame2,text="Sort by:",font=limmaGUIfont2))
  Try(tkgrid(sortByLabel))
  Try(tkgrid.configure(sortByLabel,sticky="w"))
  Try(if (NumSlides>1)
    Try(sortByTcl <- tclVar("B"))
  else
    Try(sortByTcl <- tclVar("M")))
  Try(M.but <- tkradiobutton(frame2,text="M",variable=sortByTcl,value="M",font=limmaGUIfont2))
  Try(A.but <- tkradiobutton(frame2,text="A",variable=sortByTcl,value="A",font=limmaGUIfont2))
  Try(T.but <- tkradiobutton(frame2,text="t statistic",variable=sortByTcl,value="T",font=limmaGUIfont2))  
  Try(P.but <- tkradiobutton(frame2,text="P value",variable=sortByTcl,value="P",font=limmaGUIfont2))    
  Try(B.but <- tkradiobutton(frame2,text="B statistic",variable=sortByTcl,value="B",font=limmaGUIfont2))      
  
  Try(tkgrid(M.but,sticky="w"))
  Try(tkgrid(A.but,sticky="w"))
  Try(tkgrid(T.but,sticky="w"))
  Try(tkgrid(P.but,sticky="w"))  
  Try(tkgrid(B.but,sticky="w"))
  
  Try(if (NumSlides==1)
  {
    Try(tkconfigure(T.but,state="disabled"))
    Try(tkconfigure(P.but,state="disabled"))    
    Try(tkconfigure(B.but,state="disabled"))        
  })
  
  Try(tkgrid.configure(sortByLabel,M.but,A.but,T.but,P.but,B.but,sticky="w"))
#  Try(tkgrid(frame2))

  Try(frame3 <- tkframe(ttToptableDialog,relief="groove",borderwidth=2))
#  Try(tkgrid(tklabel(frame3,text="        ")))
  Try(adjustMethodLabel <- tklabel(frame3,text="Adjust method:",font=limmaGUIfont2))
  Try(tkgrid(adjustMethodLabel))
  Try(tkgrid.configure(adjustMethodLabel,sticky="w"))  
  Try(if (NumSlides>1)
    Try(adjustMethodTcl <- tclVar("holm"))
  else
    Try(adjustMethodTcl <- tclVar("none")))
  Try(bonferroni.but <- tkradiobutton(frame3,text="Bonferroni",variable=adjustMethodTcl,value="bonferroni",font=limmaGUIfont2))
  Try(holm.but <- tkradiobutton(frame3,text="Holm",variable=adjustMethodTcl,value="holm",font=limmaGUIfont2))
  Try(hochberg.but <- tkradiobutton(frame3,text="Hochberg",variable=adjustMethodTcl,value="hochberg",font=limmaGUIfont2))  
  Try(hommel.but <- tkradiobutton(frame3,text="Hommel",variable=adjustMethodTcl,value="hommel",font=limmaGUIfont2))    
  Try(fdr.but <- tkradiobutton(frame3,text="FDR",variable=adjustMethodTcl,value="fdr",font=limmaGUIfont2))      
  Try(none.but <- tkradiobutton(frame3,text="None",variable=adjustMethodTcl,value="none",font=limmaGUIfont2))      

  Try(tkgrid(bonferroni.but,sticky="w"))
  Try(tkgrid(holm.but,sticky="w"))
  Try(tkgrid(hochberg.but,sticky="w"))
  Try(tkgrid(hommel.but,sticky="w"))  
  Try(tkgrid(fdr.but,sticky="w"))
  Try(tkgrid(none.but,sticky="w"))  
  
  Try(if (NumSlides==1)
  {
		Try(tkconfigure(bonferroni.but,state="disabled"))
		Try(tkconfigure(holm.but,state="disabled"))
		Try(tkconfigure(hochberg.but,state="disabled"))
		Try(tkconfigure(hommel.but,state="disabled"))  
		Try(tkconfigure(fdr.but,state="disabled"))
		Try(tkconfigure(none.but,state="disabled"))    
  })
  
#  Try(tkgrid(frame3))
  
#  tkgrid(tklabel(ttToptableDialog,text="        "))
  Try(if (NumSlides>1)
    Try(totalGenes <- nrow(genelist))
  else
    Try(totalGenes <- nrow(gal)))
  Try(Abort <- 1)
  Try(numberOfGenes <- 0)
  Try(sortBy <- "B")
  Try(adjustMethod <- "holm")
  Try(onOK <- function()
  {     
      Try(NumGenesChoice <- as.numeric(tclvalue(numberOfGenesTcl)))
      Try(tkgrab.release(ttToptableDialog))
      Try(tkdestroy(ttToptableDialog))
      Try(tkfocus(ttMain))
      Try(NumbersOfGenes <- c(10,30,50,100,totalGenes))
      Try(numberOfGenes <<- NumbersOfGenes[NumGenesChoice]      )
      Try(sortBy <<- tclvalue(sortByTcl))
      Try(adjustMethod <<- tclvalue(adjustMethodTcl))
      Try(Abort <<- 0)
  })

  Try(frame4 <- tkframe(ttToptableDialog,borderwidth=2))
  Try(onCancel <- function() {Try(tkgrab.release(ttToptableDialog));Try(tkdestroy(ttToptableDialog));Try(tkfocus(ttMain));Abort <<- 1})
  Try(OK.but <-tkbutton(frame4,text="   OK   ",command=onOK,font=limmaGUIfont2))
  Try(Cancel.but <-tkbutton(frame4,text=" Cancel ",command=onCancel,font=limmaGUIfont2))
  
  Try(tkgrid(tklabel(frame4,text="    "),OK.but,Cancel.but,tklabel(frame4,text="    ")))

  Try(tkgrid(tklabel(ttToptableDialog,text="    "),frame1,frame2,tklabel(ttToptableDialog,text="  ")))
  Try(tkgrid(tklabel(ttToptableDialog,text="    ")))
  Try(tkgrid(tklabel(ttToptableDialog,text="    "),frame3,frame4,tklabel(ttToptableDialog,text="  ")))
  Try(tkgrid(tklabel(ttToptableDialog,text="    ")))  
  Try(tkgrid.configure(frame1,frame3,sticky="w"))
#  Try(tkgrid.configure(frame4,sticky="s"))
  
  Try(tkfocus(ttToptableDialog))
  Try(tkbind(ttToptableDialog, "<Destroy>", function() {Try(tkgrab.release(ttToptableDialog));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttToptableDialog))
    
  Try(if (Abort==1) 
     return())
  
  Try(if (numberOfGenes==totalGenes) 
  {
      tkconfigure(ttMain,cursor="watch")
      Try(tkfocus(ttMain))
  })

  Try(options(digits=3))
  
  Try(if (NumSlides==1)
  {
    Try(NormalizeNow())
    Try(MA <- get("MA",envir=limmaGUIenvironment))
    Try(Amatrix <- MA$A)
  })
  
  
  Try(if (is.null(Amatrix))
  {
    Try(MA <- get("MA",envir=limmaGUIenvironment))  
    Try(A <- MA$A)
    Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
    Try(numSpotTypes <- nrow(SpotTypes))
    Try(SpotTypeStatus <- get("SpotTypeStatus",envir=limmaGUIenvironment))      
    Try(SpotTypesForLinearModel <- ParameterizationList[[ParameterizationNameNode]]$SpotTypesForLinearModel)  
    Omit <- ""
    count <- 0
    Try(for (i in (1:numSpotTypes))
    {
      if (SpotTypesForLinearModel[i]==TRUE)
        next()
      count <- count + 1
      if (count>1)
        Omit <-paste(Omit,"|")
      else
        Omit <- "(" 
      Try(Omit <- paste(Omit," (SpotTypeStatus==\"",SpotTypes[i,"SpotType"],"\")",sep=""))
    })  
    Try(if (nchar(Omit)>0)
    {
      Try(Omit <- paste(Omit,")"))  
      Try(Omit <- eval(parse(text=Omit)))
      Try(A <- A[!Omit,])
    })

    Try(A <- unwrapdups(A,ndups=ndups,spacing=spacing))
  }
  else
    if (NumSlides>1)
      Try(A <- unwrapdups(Amatrix,ndups=ndups,spacing=spacing)))
  
  Try(if (NumSlides>1)
    Try(table1 <- toptable(coef=coef,number=numberOfGenes,genelist=genelist,A=A,fit=fit,eb=eb,adjust.method=adjustMethod,sort.by=sortBy))
  else
  {
    Try(GenesAndLogRatios <- data.frame(gal,M=as.vector(MA$M),A=as.vector(MA$A)))
    Try(if (sortBy=="M")
      Try(ord <- order(abs(MA$M),decreasing=TRUE)))
    Try(if (sortBy=="A")
      Try(ord <- order(abs(MA$A),decreasing=TRUE)))      
    Try(table1 <- GenesAndLogRatios[ord,])
    Try(table1 <- table1[1:numberOfGenes,])
  })
  
#  Try(colnames(table1)[ncol(table1)-1] <- sprintf("%-10s",colnames(table1)[ncol(table1)-1]))

  Try(nrows <- nrow(table1))
  Try(ncols <- ncol(table1))  
  
  if (nrows <=100)
  {
    Try(ttToptableTable <- tktoplevel(ttMain))
    Try(if (NumSlides>1)
      Try(tkwm.title(ttToptableTable,paste("Top",numberOfGenes,"Candidate Genes for Differential Expression for",ParameterNamesVec[coef],".",sep=" ")))
    else
      Try(tkwm.title(ttToptableTable,paste("Top",numberOfGenes,"Candidate Genes for Differential Expression for Slide",SlideNamesVec[1],".",sep=" "))))
    TclRequire("Tktable") 
    Try(toptableTable <- tkwidget(ttToptableTable,"table",
           xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...),
           rows=nrows+1,cols=ncols,titlerows=1,
           width=ncols,selectmode="extended",colwidth="13",background="white",
           rowseparator="\"\n\"",colseparator="\"\t\"",resizeborders="col",multiline="0",state="disabled",
           font=limmaGUIfontTopTable))
    Try(xscr <-tkscrollbar(ttToptableTable,orient="horizontal", command=function(...)tkxview(toptableTable,...)))
    Try(yscr <- tkscrollbar(ttToptableTable,command=function(...)tkyview(toptableTable,...)))
    Try(tclArrayVar1 <- tclArrayVar())
    Try(tclArrayName <- ls(tclArrayVar1$env))
#   Try(n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env))
#   Try(tclArrayName <- paste("::RTcl", n, sep = ""))
#   Try(.Tcl(paste("set ",tclArrayName,"(0,0) \"\"",sep="")))
    Try(tkcmd("set",paste(tclArrayName,"0,0",sep=""),""))
    Try(for (j in (1:ncols))
#       .Tcl(paste("set ",tclArrayName,"(0,",j-1,") \"",colnames(table1)[j],"\"",sep=""))   )
        Try(tkcmd("set",paste(tclArrayName,"(",0,",",j-1,")",sep=""),colnames(table1)[j]))  )
    Try(for (i in (1:nrows))
      for (j in (1:ncols))
      {
        Try(if (is.numeric(table1[i,j]))
          item <- format(table1[i,j],digits=4)
        else
          item <- table1[i,j])
#       .Tcl(paste("set ",tclArrayName,"(",i,",",j-1,") \"",item,"\"",sep=""))
        Try(tkcmd("set",paste(tclArrayName,"(",i,",",j-1,")",sep=""),paste(item)))        
      })
    Try(tkgrid(toptableTable,yscr))
    Try(tkgrid.configure(toptableTable,sticky="news"))
    Try(tkgrid.configure(yscr,sticky="nsw"))
    Try(tkgrid(xscr,sticky="new"))
    Try(tkconfigure(toptableTable,bg="white",variable=tclArrayName))
    Try(for (i in (1:ncols))
    {
      if (tolower(colnames(table1)[i]) %in% c("block","row","column","gridrow","gridcolumn","gridcol","grid.row","grid.col","grid.column"))
      {
        Try(if (limmaGUIpresentation==FALSE)
          Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(max(4,nchar(colnames(table1)[i])+2))))
        else
          Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(max(4,nchar(colnames(table1)[i]))))))        
        next()
      }
      if (colnames(table1)[i] %in% c("M","A","t","B"))
      {
        Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),"6"))
        next()
      }
      if (colnames(table1)[i] %in% c("P.Value"))
      {
        Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),"8"))
        next()
      }     
      if(tolower(colnames(table1)[i]) == "name")
      {
        Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(min(30,max(6,max(nchar(table1[,i]))+2)))))
        next()
      }

      if(tolower(colnames(table1)[i]) == "id")
      {
        Try(if (limmaGUIpresentation==FALSE)
          Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(max(4,max(nchar(table1[,i]))+2))))
        else
          Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(max(4,max(nchar(table1[,i])))))))
        next()
      }

      Try(tkcmd(.Tk.ID(toptableTable),"width",paste(i-1),paste(max(4,max(nchar(table1[,i]))+2))))
    })
    Try(tkfocus(ttToptableTable))
  }
  else
  {
    Try(tkmessageBox(title="Large Toptable",message="Toptable is too big to display in a table widget, so it will be displayed in a text window instead.  You can save it as a tab-delimited text file and then import it into a spreadsheet program.",icon="info",type="ok"))
    Try(tempfile1 <- tempfile())
    Try(write.table(table1,file=tempfile1,quote=FALSE,col.names=NA,sep="\t"))
    Try(ttToptableTable <- tktoplevel(ttMain))
    Try(if (NumSlides>1)
      Try(tkwm.title(ttToptableTable,paste("Top",numberOfGenes,"Candidate Genes for Differential Expression for",ParameterNamesVec[coef],".",sep=" ")))
    else
      Try(tkwm.title(ttToptableTable,paste("Top",numberOfGenes,"Candidate Genes for Differential Expression for Slide",SlideNamesVec[1],".",sep=" "))))
    Try(xscr <-tkscrollbar(ttToptableTable, repeatinterval=5,orient="horizontal",command=function(...)tkxview(txt,...)))
    Try(scr <- tkscrollbar(ttToptableTable, repeatinterval=5,command=function(...)tkyview(txt,...)))
    Try(txt <- tktext(ttToptableTable, bg="white", font="courier",xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(scr,...),wrap="none",width=100))

    Try(copyText2 <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Copy>>"))))

    Try(editPopupMenu2 <- tkmenu(txt, tearoff=FALSE))
    Try(tkadd(editPopupMenu2, "command", label="Copy <Ctrl-C>",command=copyText2)) # ,font=limmaGUIfontMenu)

    RightClick2 <- function(x,y) # x and y are the mouse coordinates
    {
     rootx <- as.integer(tkwinfo("rootx",txt))
     rooty <- as.integer(tkwinfo("rooty",txt))
     xTxt <- as.integer(x)+rootx
     yTxt <- as.integer(y)+rooty
     .Tcl(paste("tk_popup",.Tcl.args(editPopupMenu2,xTxt,yTxt)))
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

    Try(if (numberOfGenes==totalGenes)
        tkconfigure(ttMain,cursor="arrow"))
  }
  
  
  SaveTopTable <- function()
  {
    Try(if (NumSlides>1)
      Try(TopTableFile <- tclvalue(tkgetSaveFile(initialfile=paste("toptable", coef,".txt",sep=""))))
    else
      Try(TopTableFile <- tclvalue(tkgetSaveFile(initialfile=paste("toptable1.txt",sep="")))))
    Try(if (!nchar(TopTableFile))
      return())
    Try(write.table(table1,file=TopTableFile,quote=FALSE,col.names=NA,sep="\t"))
  } 
  
  Try(copyFcn <-      function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(toptableTable),"<<Copy>>"))))
  
  topMenu2 <- tkmenu(ttToptableTable)
  tkconfigure(ttToptableTable, menu=topMenu2)
  fileMenu2 <- tkmenu(topMenu2, tearoff=FALSE)
  tkadd(fileMenu2, "command", label="Save As",
  command=SaveTopTable) # ) # ,font=limmaGUIfontMenu)
  tkadd(fileMenu2, "command", label="Close",
  command=function() tkdestroy(ttToptableTable)) # ) # ,font=limmaGUIfontMenu)
  tkadd(topMenu2, "cascade", label="File",
  menu=fileMenu2) # ,font=limmaGUIfontMenu)
  editMenu2 <- tkmenu(topMenu2, tearoff=FALSE)
  tkadd(editMenu2, "command", label="Copy <Ctrl-C>",
  command=copyFcn) # ,font=limmaGUIfontMenu)
  tkadd(topMenu2, "cascade", label="Edit",
  menu=editMenu2) # ,font=limmaGUIfontMenu)
}

GetSpotTypesForLinearModel <- function()
{
  Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
  Try(numSpotTypes <- nrow(SpotTypes))

  if (numSpotTypes==0)
  {
      tkmessageBox(message="Error: No spot types have been registered.",title="Spot Types For Linear Model",type="ok",icon="error");
      return()
  }

  ttGetSpotTypesForLinearModel<-tktoplevel(ttMain)
  tkwm.deiconify(ttGetSpotTypesForLinearModel)
  tkgrab.set(ttGetSpotTypesForLinearModel)
  tkfocus(ttGetSpotTypesForLinearModel)
  tkwm.title(ttGetSpotTypesForLinearModel,"Spot Types to be Included in the Linear Model Fit")   
  
  Try(if (numSpotTypes>10)
  {
    TclRequire("BWidget")
    Try(sw <- tkwidget(ttGetSpotTypesForLinearModel,"ScrolledWindow",relief="sunken",borderwidth=2))
    Try(sf <- tkwidget(sw,"ScrollableFrame"))
    Try(tkcmd(sw,"setwidget",sf))
    Try(subfID <- tclvalue(tkcmd(sf,"getframe")))
  }
  else
  {
    Try(sw <- tkframe(ttGetSpotTypesForLinearModel,borderwidth=2))
    Try(subfID <- .Tk.ID(sw))
  })
  Try(tkgrid(tklabel(ttGetSpotTypesForLinearModel,text="    "),tklabel(ttGetSpotTypesForLinearModel,text="    ")))

  IncludeSpotTypeTcl <- list()
  for (i in (1:numSpotTypes))
      Try(IncludeSpotTypeTcl[[i]] <- tclVar("1"))  # Initially, all checkboxes are checked.

  SpotTypeCheckbox <- list()
  for (i in (1:numSpotTypes))
      Try(SpotTypeCheckbox[[i]] <- tkcmd("checkbutton",paste(subfID,".cb",i,sep=""),variable=IncludeSpotTypeTcl[[i]]))
       
  Try(lbl2 <- tklabel(ttGetSpotTypesForLinearModel,text="Please Choose Spot Types to be Included in the Linear Model Fit",font=limmaGUIfont2))
  tkgrid(lbl2)
  Try(tkgrid.configure(lbl2,columnspan=2,sticky="w"))
  tkgrid(tklabel(ttGetSpotTypesForLinearModel,text="    "))
  
  for (i in (1:numSpotTypes))
  {
      Try(blankLabel <- tkcmd("label",paste(subfID,".blank",i,sep=""),text="    ",font=limmaGUIfont2))
      Try(currentLabel <- tkcmd("label",paste(subfID,".lab",i,sep=""),text=SpotTypes[i,"SpotType"],font=limmaGUIfont2))
      Try(tkgrid(blankLabel,SpotTypeCheckbox[[i]],currentLabel))
      Try(tkgrid.configure(SpotTypeCheckbox[[i]],sticky="e"))
      Try(tkgrid.configure(currentLabel,sticky="w"))
  }

  Try(tkgrid(sw,columnspan=2,sticky="nsew"))
  
  tkgrid(tklabel(ttGetSpotTypesForLinearModel,text="    "))
  tkgrid(tklabel(ttGetSpotTypesForLinearModel,text="    "))
  ReturnVal <- c()
  onOK <- function()
  {
      ReturnVal <- rep(TRUE,numSpotTypes)
      for (i in (1:numSpotTypes))
        if (tclvalue(IncludeSpotTypeTcl[[i]])!="1")
            ReturnVal[i] <- FALSE
      Try(tkgrab.release(ttGetSpotTypesForLinearModel));Try(tkdestroy(ttGetSpotTypesForLinearModel));Try(tkfocus(ttMain))
      ReturnVal <<- ReturnVal
  }
  onCancel <- function() {Try(tkgrab.release(ttGetSpotTypesForLinearModel));Try(tkdestroy(ttGetSpotTypesForLinearModel));Try(tkfocus(ttMain)); ReturnVal <<- c()}      
  OK.but <-tkbutton(ttGetSpotTypesForLinearModel,text="   OK   ",command=onOK,font=limmaGUIfont2)
  Cancel.but <-tkbutton(ttGetSpotTypesForLinearModel,text=" Cancel ",command=onCancel,font=limmaGUIfont2)
  tkgrid(OK.but,Cancel.but,tklabel(ttGetSpotTypesForLinearModel,text="    "),tklabel(ttGetSpotTypesForLinearModel,text="    "))
  tkgrid.configure(OK.but,    sticky="e")
  tkgrid.configure(Cancel.but,sticky="w")
  tkgrid(tklabel(ttGetSpotTypesForLinearModel,text="    "),tklabel(ttGetSpotTypesForLinearModel,text="    "),tklabel(ttGetSpotTypesForLinearModel,text="    "),
       tklabel(ttGetSpotTypesForLinearModel,text="    "),tklabel(ttGetSpotTypesForLinearModel,text="    "))
  Try(tkfocus(ttGetSpotTypesForLinearModel))
  Try(tkbind(ttGetSpotTypesForLinearModel, "<Destroy>", function() {Try(tkgrab.release(ttGetSpotTypesForLinearModel));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttGetSpotTypesForLinearModel))

  return (ReturnVal)
}



GetSlideNum <- function()
{
  Try(SlideNamesVec <- get("SlideNamesVec",envir=limmaGUIenvironment))  
  Try(if (min(nchar(gsub("[^0-9]","",SlideNamesVec))==nchar(SlideNamesVec))==TRUE)
    SlideNamesVec <- paste("Slide",SlideNamesVec))  
  Try(NumSlides <- get("NumSlides",envir=limmaGUIenvironment))
  ttGetSlideNum<-tktoplevel(ttMain)
  tkwm.deiconify(ttGetSlideNum)
  tkgrab.set(ttGetSlideNum)
  tkfocus(ttGetSlideNum)
  tkwm.title(ttGetSlideNum,"Please Specify Slide")
  scr <- tkscrollbar(ttGetSlideNum, repeatinterval=5,
                       command=function(...)tkyview(tl,...))
  ## Safest to make sure scr exists before setting yscrollcommand
  tl<-tklistbox(ttGetSlideNum,height=4,selectmode="browse",yscrollcommand=function(...)tkset(scr,...),background="white",font=limmaGUIfont2) 
  tkgrid(tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),
       tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
  lbl2<-tklabel(ttGetSlideNum,text="Choose a slide",font=limmaGUIfont2)
  tkgrid(tklabel(ttGetSlideNum,text="    "),lbl2,tklabel(ttGetSlideNum,text="    "),
     tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
  tkgrid.configure(lbl2,sticky="w")
  tkgrid(tklabel(ttGetSlideNum,text="    "),row=2,column=0)
  tkgrid(tl,row=2,column=1,columnspan=2,rowspan=4,sticky="ew")
  tkgrid(scr,row=2,column=3,rowspan=4,sticky="wns")
  tkgrid(tklabel(ttGetSlideNum,text="    "),row=2,column=4)
  for (i in (1:NumSlides))
    tkinsert(tl,"end",SlideNamesVec[i])
  tkselection.set(tl,0)

  tkgrid(tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),
     tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
  ReturnVal <- 0
  onOK <- function()
  {
      slidenum <- as.numeric(tclvalue(tkcurselection(tl)))+1
      Try(tkgrab.release(ttGetSlideNum));Try(tkdestroy(ttGetSlideNum));Try(tkfocus(ttMain))
      ReturnVal <<- slidenum
  }
  onCancel <- function() {Try(tkgrab.release(ttGetSlideNum));Try(tkdestroy(ttGetSlideNum));Try(tkfocus(ttMain)); ReturnVal <<- 0}      
  OK.but <-tkbutton(ttGetSlideNum,text="   OK   ",command=onOK,font=limmaGUIfont2)
  Cancel.but <-tkbutton(ttGetSlideNum,text=" Cancel ",command=onCancel,font=limmaGUIfont2)
  tkgrid(tklabel(ttGetSlideNum,text="    "),OK.but,Cancel.but,tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
  tkgrid.configure(OK.but,Cancel.but,sticky="w")
  tkgrid(tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "),
       tklabel(ttGetSlideNum,text="    "),tklabel(ttGetSlideNum,text="    "))
  Try(tkbind(OK.but, "<Return>",onOK))
  Try(tkbind(tl, "<Return>",onOK))    
  Try(tkbind(Cancel.but, "<Return>",onCancel))      
  Try(tkfocus(tl))
  Try(tkbind(ttGetSlideNum, "<Destroy>", function() {Try(tkgrab.release(ttGetSlideNum));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttGetSlideNum))

  return (ReturnVal)
}


GetDEcutoff <- function()
{
  ttGetDEcutoff<-tktoplevel(ttMain)
  tkwm.deiconify(ttGetDEcutoff)
  tkgrab.set(ttGetDEcutoff)  
  Try(tkwm.title(ttGetDEcutoff,"Cutoff for Differentially Expressed Genes"))
  Try(cutoffStatisticTcl <- tclVar("abs(t)"))
  Try(tkframe1 <- tkframe(ttGetDEcutoff,borderwidth=2))
  Try(tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2))
  Try(tkframe4<-tkframe(tkframe1,borderwidth=2))

  Try(tkgrid(tklabel(tkframe1,text="    ")))

  Try(tkgrid(tklabel(tkframe2,text="Choose a cutoff for differentially expressed genes.",font=limmaGUIfont2),rowspan=1,columnspan=2,sticky="w"))

  Try(tStatistic.but <- tkradiobutton(tkframe2,text="Abs(t)",variable=cutoffStatisticTcl,value="abs(t)",font=limmaGUIfont2))
  Try(BStatistic.but <- tkradiobutton(tkframe2,text="B",variable=cutoffStatisticTcl,value="B",font=limmaGUIfont2))

  Try(tkgrid(tStatistic.but))
  Try(tkgrid(BStatistic.but))
  Try(tkgrid.configure(tStatistic.but,BStatistic.but,sticky="w"))
  Try(tkgrid(tklabel(tkframe2,text="    ")))
  Try(cutoffValueTcl <- tclVar("0"))
  Try(entry.cutoffValue<-tkentry(tkframe2,width="20",font=limmaGUIfont2,textvariable=cutoffValueTcl,bg="white"))
  Try(tkgrid(tklabel(tkframe2,text="Cutoff value ",font=limmaGUIfont2),entry.cutoffValue,sticky="w"))
  
  Try(tkgrid(tkframe2))
  Try(ReturnVal <- list())
  onOK <- function()
  {
      Try(cutoffStatisticVal <- as.character(tclvalue(cutoffStatisticTcl)))
      Try(cutoffValue <- as.numeric(tclvalue(cutoffValueTcl)))
      Try(tkgrab.release(ttGetDEcutoff));Try(tkdestroy(ttGetDEcutoff));Try(tkfocus(ttMain))
      Try(ReturnVal <<- list(cutoffStatistic=cutoffStatisticVal,cutoff=cutoffValue))
  }
  onCancel <- function(){tkgrab.release(ttGetDEcutoff);tkdestroy(ttGetDEcutoff);tkfocus(ttMain);ReturnVal <<- list()} 
  Try(OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=limmaGUIfont2))
  Try(Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=limmaGUIfont2))
  Try(tkgrid(tklabel(tkframe4,text="                    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="e"))
  Try(tkgrid(tklabel(tkframe4,text="       ")))
  Try(tkgrid(tkframe4))
  Try(tkgrid(tkframe1))
  Try(tkfocus(ttGetDEcutoff))
  Try(tkbind(ttGetDEcutoff, "<Destroy>", function(){tkgrab.release(ttGetDEcutoff);tkfocus(ttMain);} )) 
  Try(tkwait.window(ttGetDEcutoff))

  return (ReturnVal)
}

UpdateSpotTypesStatus <- function()
{
  Try(SpotTypes <- get("SpotTypes", envir=limmaGUIenvironment))
  Try(gal <- get("gal", envir=limmaGUIenvironment))
  Try(Status <- controlStatus(SpotTypes,gal))
  Try(assign("SpotTypeStatus", Status, limmaGUIenvironment))
}


ChooseEbayesStatistic <- function()
{
  ttChooseEbayesStatistic<-tktoplevel(ttMain)
  tkwm.deiconify(ttChooseEbayesStatistic)
  tkgrab.set(ttChooseEbayesStatistic)
  tkfocus(ttChooseEbayesStatistic)
  tkwm.title(ttChooseEbayesStatistic,"Empirical Bayes Statistic")   
  Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    ")))

  Try(EbayesStatisticTcl <- tclVar("t"))

  Try(tStatisticRadioButton <- tkradiobutton(ttChooseEbayesStatistic,variable=EbayesStatisticTcl,value="t"))
  Try(BStatisticRadioButton <- tkradiobutton(ttChooseEbayesStatistic,variable=EbayesStatisticTcl,value="lods"))  
  Try(PValueRadioButton     <- tkradiobutton(ttChooseEbayesStatistic,variable=EbayesStatisticTcl,value="p.value"))    
       
  Try(lbl2 <- tklabel(ttChooseEbayesStatistic,text="Please Choose an Empirical Bayes Statistic",font=limmaGUIfont2))
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),lbl2)
  Try(tkgrid.configure(lbl2,columnspan=2,sticky="w"))
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "))
  
  Try(currentLabel <- tklabel(ttChooseEbayesStatistic,text="t Statistic",font=limmaGUIfont2))
  Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),tStatisticRadioButton,currentLabel))
  Try(tkgrid.configure(tStatisticRadioButton,sticky="e"))
  Try(tkgrid.configure(currentLabel,sticky="w"))
  Try(currentLabel <- tklabel(ttChooseEbayesStatistic,text="B Statistic",font=limmaGUIfont2))
  Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),BStatisticRadioButton,currentLabel))
  Try(tkgrid.configure(BStatisticRadioButton,sticky="e"))
  Try(tkgrid.configure(currentLabel,sticky="w"))
  Try(currentLabel <- tklabel(ttChooseEbayesStatistic,text="P Value",font=limmaGUIfont2))
  Try(tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),PValueRadioButton,currentLabel))
  Try(tkgrid.configure(PValueRadioButton,sticky="e"))
  Try(tkgrid.configure(currentLabel,sticky="w"))
  
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "))
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "))
  ReturnVal <- ""
  onOK <- function()
  {      
      Try(ReturnVal <- tclvalue(EbayesStatisticTcl))
      Try(tkgrab.release(ttChooseEbayesStatistic));Try(tkdestroy(ttChooseEbayesStatistic));Try(tkfocus(ttMain))
      ReturnVal <<- ReturnVal
  }
  onCancel <- function() {Try(tkgrab.release(ttChooseEbayesStatistic));Try(tkdestroy(ttChooseEbayesStatistic));Try(tkfocus(ttMain)); ReturnVal <<- ""}      
  OK.but <-tkbutton(ttChooseEbayesStatistic,text="   OK   ",command=onOK,font=limmaGUIfont2)
  Cancel.but <-tkbutton(ttChooseEbayesStatistic,text=" Cancel ",command=onCancel,font=limmaGUIfont2)
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),OK.but,Cancel.but,tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    "))
  tkgrid.configure(OK.but,    sticky="e")
  tkgrid.configure(Cancel.but,sticky="w")
  tkgrid(tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    "),
       tklabel(ttChooseEbayesStatistic,text="    "),tklabel(ttChooseEbayesStatistic,text="    "))
  Try(tkfocus(ttChooseEbayesStatistic))
  Try(tkbind(ttChooseEbayesStatistic, "<Destroy>", function() {Try(tkgrab.release(ttChooseEbayesStatistic));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttChooseEbayesStatistic))

  return (ReturnVal)

}

ChooseSpotType <- function(parameterizationTreeIndex)
{
  # This function is designed to be used for selecting a spot type for which t Statistics box plots will be shown.
  # This means that the only available spot types should be those included in the linear model.
  
  
  Try(SpotTypesIncludedNamesVec <- GetSpotTypesIncludedNames(parameterizationTreeIndex)) 
  Try(numSpotTypes <- length(SpotTypesIncludedNamesVec))

  if (numSpotTypes==0)
  {
      tkmessageBox(message="Error: No spot types have been registered.",title="Choose Spot Type",type="ok",icon="error");
      return()
  }

  ttChooseSpotType<-tktoplevel(ttMain)
  tkwm.deiconify(ttChooseSpotType)
  tkgrab.set(ttChooseSpotType)
  tkfocus(ttChooseSpotType)
  tkwm.title(ttChooseSpotType,"Spot Type for Empirical Bayes Statistics Box Plots")   
  Try(tkgrid(tklabel(ttChooseSpotType,text="    "),tklabel(ttChooseSpotType,text="    ")))

  Try(WhichSpotTypeTcl <- tclVar("1"))

  Try(if (numSpotTypes>10)
  {
    TclRequire("BWidget")
    Try(sw <- tkwidget(ttChooseSpotType,"ScrolledWindow",relief="sunken",borderwidth=2))
    Try(sf <- tkwidget(sw,"ScrollableFrame"))
    Try(tkcmd(sw,"setwidget",sf))
    Try(subfID <- tclvalue(tkcmd(sf,"getframe")))
  }
  else
  {
    Try(sw <- tkframe(ttChooseSpotType,borderwidth=2))
    Try(subfID <- .Tk.ID(sw))
  })

  SpotTypeRadioButton <- list()
  for (i in (1:numSpotTypes))
      Try(SpotTypeRadioButton[[i]] <- tkcmd("radiobutton",paste(subfID,".rb",i,sep=""),variable=WhichSpotTypeTcl,value=paste(i)))
       
  Try(lbl2 <- tklabel(ttChooseSpotType,text="Please Choose a Spot Type for Empirical Bayes Statistics Box Plots",font=limmaGUIfont2))
  tkgrid(tklabel(ttChooseSpotType,text="    "),lbl2)
  Try(tkgrid.configure(lbl2,columnspan=2,sticky="w"))
  tkgrid(tklabel(ttChooseSpotType,text="    "))
  
  for (i in (1:numSpotTypes))
  {
      Try(currentLabel <- tkcmd("label",paste(subfID,".lab1_",i,sep=""),text=SpotTypesIncludedNamesVec[i],font=limmaGUIfont2))
      Try(tkgrid(tkcmd("label",paste(subfID,".lab2_",i,sep=""),text="    "),SpotTypeRadioButton[[i]],currentLabel))
      Try(tkgrid.configure(SpotTypeRadioButton[[i]],sticky="e"))
      Try(tkgrid.configure(currentLabel,sticky="w"))
  }
  
  Try(tkgrid(sw,columnspan=2,sticky="nsew"))  
  
  tkgrid(tklabel(ttChooseSpotType,text="    "))
  tkgrid(tklabel(ttChooseSpotType,text="    "))
  
  Try(OKCancelFrame <- tkframe(ttChooseSpotType,borderwidth=2))
  
  ReturnVal <- ""
  onOK <- function()
  {
      Try(radioButtonSelected <- as.integer(tclvalue(WhichSpotTypeTcl)))      
      Try(ReturnVal <- SpotTypesIncludedNamesVec[radioButtonSelected])
      Try(tkgrab.release(ttChooseSpotType));Try(tkdestroy(ttChooseSpotType));Try(tkfocus(ttMain))
      ReturnVal <<- ReturnVal
  }
  onCancel <- function() {Try(tkgrab.release(ttChooseSpotType));Try(tkdestroy(ttChooseSpotType));Try(tkfocus(ttMain)); ReturnVal <<- ""}      
  OK.but <-tkbutton(OKCancelFrame,text="   OK   ",command=onOK,font=limmaGUIfont2)
  Cancel.but <-tkbutton(OKCancelFrame,text=" Cancel ",command=onCancel,font=limmaGUIfont2)
  tkgrid(OK.but,Cancel.but)
  tkgrid.configure(OK.but,    sticky="e")
  tkgrid.configure(Cancel.but,sticky="w")
  tkgrid(OKCancelFrame,columnspan=2)  
  tkgrid(tklabel(ttChooseSpotType,text="    "),tklabel(ttChooseSpotType,text="    "))
  Try(tkfocus(ttChooseSpotType))
  Try(tkbind(ttChooseSpotType, "<Destroy>", function() {Try(tkgrab.release(ttChooseSpotType));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttChooseSpotType))

  return (ReturnVal)

}

GetWtAreaParams <- function()
{
    ttWeightingwtArea <- tktoplevel(ttMain)
    tkwm.deiconify(ttWeightingwtArea)
    tkgrab.set(ttWeightingwtArea)
    tkfocus(ttWeightingwtArea)
    tkwm.title(ttWeightingwtArea,"Good Spot Size")
    tkframe1 <- tkframe(ttWeightingwtArea)
    tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2)
    tkframe4 <- tkframe(tkframe1)
    tkgrid(tklabel(tkframe1,text="    "))
    tkgrid(tklabel(tkframe1,text="Please enter the area range for good spots",font=limmaGUIfont2),columnspan=2)
    tkgrid(tklabel(tkframe1,text="    "))
    tkgrid(tklabel(tkframe2,text="Area Range in Pixels",font=limmaGUIfont2),columnspan=2)
    AreaLowerLimitTcl <- tclVar(paste(160))
    AreaUpperLimitTcl <- tclVar(paste(170))
    tkgrid(tklabel(tkframe2,text="    "))
    entry.AreaLowerLimit <-tkentry(tkframe2,width="12",font=limmaGUIfont2,textvariable=AreaLowerLimitTcl,bg="white")
    entry.AreaUpperLimit <-tkentry(tkframe2,width="12",font=limmaGUIfont2,textvariable=AreaUpperLimitTcl,bg="white")
    tkgrid(tklabel(tkframe2,text="Lower area limit in pixels",font=limmaGUIfont2),entry.AreaLowerLimit,sticky="w")
    tkgrid(tklabel(tkframe2,text="Upper area limit in pixels",font=limmaGUIfont2),entry.AreaUpperLimit,sticky="w")
    tkgrid(tkframe2)
    tkgrid(tklabel(tkframe1,text="    "))
    ReturnVal <- 0
    AreaLowerLimitVal <- 0
    AreaUpperLimitVal <- 0
    onOK <- function()
    {
        Try(AreaLowerLimitVal <<- as.integer(tclvalue(AreaLowerLimitTcl)))
        Try(AreaUpperLimitVal <<- as.integer(tclvalue(AreaUpperLimitTcl)))
        Try(assign("AreaLowerLimit",AreaLowerLimitVal,limmaGUIenvironment))
        Try(assign("AreaUpperLimit",AreaUpperLimitVal,limmaGUIenvironment))
        Try(assign("WeightingType",paste("wtarea, Ideal=(",AreaLowerLimitVal,",",AreaUpperLimitVal,")",sep=""),limmaGUIenvironment))        
        Try(tkgrab.release(ttWeightingwtArea));Try(tkdestroy(ttWeightingwtArea));Try(tkfocus(ttMain))
        ReturnVal <<- 1
    }
    onCancel <- function() {Try(tkgrab.release(ttWeightingwtArea));Try(tkdestroy(ttWeightingwtArea));Try(tkfocus(ttMain));ReturnVal<<-0}   
    OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=limmaGUIfont2)   
    Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=limmaGUIfont2)
    tkgrid(OK.but,Cancel.but)
    tkgrid(tklabel(tkframe4,text="    "))
    tkgrid(tkframe4)
    tkgrid(tkframe1)
    Try(tkfocus(ttWeightingwtArea))
    Try(tkbind(ttWeightingwtArea, "<Destroy>", function() {Try(tkgrab.release(ttWeightingwtArea));Try(tkfocus(ttMain));}))
    Try(tkwait.window(ttWeightingwtArea))

    return (ReturnVal)
}

#GetWtFlagParams <- function()
#{
#    ttWeightingwtFlag <- tktoplevel(ttMain)
#    tkwm.deiconify(ttWeightingwtFlag)
#    tkgrab.set(ttWeightingwtFlag)
#    tkfocus(ttWeightingwtFlag)  
#    tkwm.title(ttWeightingwtFlag,"Weighting for Spots with Flag Values Less Than Zero")
#    tkframe1 <- tkframe(ttWeightingwtFlag)
#    tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2)
#    tkframe4 <- tkframe(tkframe1)
#    tkgrid(tklabel(tkframe1,text="    "))
#    tkgrid(tklabel(tkframe1,text="Please enter the weighting for spots with flag values less than zero",font=limmaGUIfont2),columnspan=2)
#    tkgrid(tklabel(tkframe1,text="    "))
#    tkgrid(tklabel(tkframe2,text="Spot Weighting",font=limmaGUIfont2),columnspan=2)
#    FlagSpotWeightingTcl <- tclVar(paste(0.1))
#    tkgrid(tklabel(tkframe2,text="    "))
#    entry.FlagSpotWeighting<-tkentry(tkframe2,width="12",font=limmaGUIfont2,textvariable=FlagSpotWeightingTcl,bg="white")
#    tkgrid(tklabel(tkframe2,text="Weighting (relative to 1 for all other spots)",font=limmaGUIfont2),entry.FlagSpotWeighting,sticky="w")
#    tkgrid(tkframe2)
#    tkgrid(tklabel(tkframe1,text="    "))
#    ReturnVal <- 0
#    FlagSpotWeightingVal <- 0
#    onOK <- function()
#    {
#        Try(FlagSpotWeightingVal <- as.numeric(tclvalue(FlagSpotWeightingTcl)))
#        Try(tkgrab.release(ttWeightingwtFlag));Try(tkdestroy(ttWeightingwtFlag));Try(tkfocus(ttMain))
#        ReturnVal <<- 1
#    }
#    onCancel <- function() {Try(tkgrab.release(ttWeightingwtFlag));Try(tkdestroy(ttWeightingwtFlag));Try(tkfocus(ttMain));ReturnVal<<-0}   
#    OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=limmaGUIfont2)
#    Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=limmaGUIfont2)
#    tkgrid(OK.but,Cancel.but)
#    tkgrid(tklabel(tkframe4,text="    "))
#    tkgrid(tkframe4)
#    tkgrid(tkframe1)
#    Try(tkfocus(ttWeightingwtFlag))
#    Try(tkbind(ttWeightingwtFlag, "<Destroy>", function() {Try(tkgrab.release(ttWeightingwtFlag));Try(tkfocus(ttMain));}))
#    Try(tkwait.window(ttWeightingwtFlag))
#    
#    Try(FlagSpotWeighting <- FlagSpotWeightingVal)
#    Try(assign("FlagSpotWeighting", FlagSpotWeighting,limmaGUIenvironment))
#
#    Try(assign("WeightingType",paste("wtflag, FlagSpotWeighting = ",FlagSpotWeighting,sep=""),limmaGUIenvironment))
#
#    return (ReturnVal)
#}

evalRcode <- function()
{
  Try(wfile <- "")
  Try(ttEvalRcode <- tktoplevel(ttMain))
  Try(tkwm.title(ttEvalRcode ,"Enter R code in this window and then click on Run"))
  Try(scrCode <- tkscrollbar(ttEvalRcode , repeatinterval=5,
                         command=function(...)tkyview(txt,...)))
  Try(xscrCode <- tkscrollbar(ttEvalRcode , repeatinterval=5,orient="horizontal",
                         command=function(...)tkxview(txt,...)))
  Try(txt <- tktext(ttEvalRcode , height=20,
            yscrollcommand=function(...)tkset(scrCode,...),
            xscrollcommand=function(...)tkset(xscrCode,...),
            wrap="none",width=100,bg="white",
              font=limmaGUIfontCourier))
  Try(cutText <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Cut>>")))    )
  Try(copyText <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Copy>>"))))
  Try(pasteText <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt),"<<Paste>>")))  )

  Try(editPopupMenu <- tkmenu(txt, tearoff=FALSE))
  Try(tkadd(editPopupMenu, "command", label="Cut <Ctrl-X>",
  command=cutText)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(editPopupMenu, "command", label="Copy <Ctrl-C>",
  command=copyText)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(editPopupMenu, "command", label="Paste <Ctrl-V>",
  command=pasteText)) # ) # ,font=limmaGUIfontMenu))

  RightClick <- function(x,y) # x and y are the mouse coordinates
  {
   Try(rootx <- as.integer(tkwinfo("rootx",txt)))
   Try(rooty <- as.integer(tkwinfo("rooty",txt)))
   Try(xTxt <- as.integer(x)+rootx)
   Try(yTxt <- as.integer(y)+rooty)
   Try(.Tcl(paste("tk_popup",.Tcl.args(editPopupMenu,xTxt,yTxt))))
  }
  Try(tkbind(txt, "<Button-3>",RightClick))
  
  Try(tkpack(scrCode, side="right", fill="y"))
  Try(tkpack(xscrCode, side="bottom", fill="x"))
  Try(tkpack(txt, side="left", fill="both", expand="yes"))
  Try(tkfocus(txt))

  SaveRSourceFile <- function() 
  {
    Try(fileName <- tclvalue(tkgetSaveFile(initialfile=tclvalue(tkfile.tail(wfile)),initialdir=tclvalue(tkfile.dir(wfile)),
           filetypes="{{R Source Files} {.R}} {{All files} *}")))
    if (nchar(fileName)==0) return()
    Try(len <- nchar(fileName))
    if (len<=2)
      Try( fileName <- paste(fileName,".R",sep=""))
    else if (substring(fileName,len-1,len)!=".R")
      Try(fileName <- paste(fileName,".R",sep=""))
    Try(chn <- tkopen(fileName, "w"))
    Try(tkputs(chn, tclvalue(tkget(txt,"0.0","end"))))
    Try(tkclose(chn))
    Try(wfile <<- fileName)
    Try(tkfocus(txt))
  }

  OpenRSourceFile <- function() 
  {
    Try(fileName <- tclvalue(tkgetOpenFile(filetypes="{{R Source Files} {.R}} {{All files} *}")))
    if (nchar(fileName)==0) return()
    Try(chn <- tkopen(fileName, "r"))
    Try(tkinsert(txt, "0.0", tclvalue(tkread(chn))))
    Try(tkclose(chn))
    Try(wfile <<- fileName)
    Try(tkfocus(txt))
  }

  runOverall <- function(runType) {
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(tkconfigure(ttEvalRcode,cursor="watch"))
  Try(tkfocus(ttEvalRcode))
  Try(code <- tclvalue(tkget(txt,"0.0","end")))
  if (runType!="runTextOnly")
  {
     Try(LocalHScale <- get("Myhscale",envir=.GlobalEnv))
     Try(LocalVScale <- get("Myvscale",envir=.GlobalEnv))   

    Try(ttGraph<-tktoplevel(ttMain))
    Try(tkwm.withdraw(ttGraph))
    Try(tkwm.title(ttGraph,"Graphical Results from R Code Evaluation"))
    Try(codeGraph <- paste("assign(\"plotFunction\",function () {\nopar<-par(bg=\"white\")\nTry({\n",code,"\n})\n\ntempGraphPar <- par(opar)\n},limmaGUIenvironment)\n",sep=""))
  }

  if (runType!="runGraphicsOnly")
  {
    Try(RoutFileObject <- file("tmpEvalRcodeResults", open="wt"))
    Try(sink(RoutFileObject))
    Try(sink(RoutFileObject,type="message"))
    Try(e <- try(parse(text=code)))
    if (inherits(e, "try-error")) 
    {
      Try(tkmessageBox(message="Syntax error",icon="error"))
      Try(tkconfigure(ttMain,cursor="arrow"))
      Try(sink(type="message"))
      Try(sink())
      Try(try(close(RoutFileObject),TRUE))
      return()
    }
    e2 <- try(print(eval(e,envir=limmaGUIenvironment)))
    if (inherits(e2, "try-error"))
    {
      Try(tkmessageBox(title="An error occured while trying to evaluate your R code",message=as.character(e2),icon="error"))
      Try(tkconfigure(ttMain,cursor="arrow"))
      Try(sink(type="message"))
      Try(sink())
      Try(try(close(RoutFileObject),TRUE))
      return()      
    }
    Try(sink(type="message"))
    Try(sink())
    Try(try(close(RoutFileObject),TRUE))
  }

  if (runType!="runTextOnly")
  {
    Try(RoutFileObjectGraph <- file("tmpEvalRcodeResultsGraph",open="wt"))
    Try(sink(RoutFileObjectGraph))
    Try(sink(RoutFileObjectGraph,type="message"))
    Try(e3 <- try(parse(text=codeGraph)))
    if (inherits(e3, "try-error")) 
    {
      Try(tkmessageBox(message="Syntax error",icon="error"))
      Try(tkconfigure(ttMain,cursor="arrow"))
      Try(sink(type="message"))
      Try(sink())
      Try(close(RoutFileObjectGraph))
      return()
    }
    e4 <- try(print(eval(e3,envir=limmaGUIenvironment)))
    if (inherits(e4, "try-error"))
    {
      Try(tkmessageBox(message="An error occured while trying to plot the graph(s) for your R code",icon="error"))
      Try(tkconfigure(ttMain,cursor="arrow"))
      Try(sink(type="message"))
      Try(sink())
      Try(close(RoutFileObjectGraph))
      return()            
    }
    Try(sink(type="message"))
    Try(sink())
    Try(try(close(RoutFileObjectGraph),TRUE))
    Require("tkrplot")

    Try(plotFunction <- get("plotFunction",envir=limmaGUIenvironment))
    Try(imgLimmaGUI<-tkrplot(ttGraph,plotFunction,hscale=LocalHScale,vscale=LocalVScale))
    SetupPlotKeyBindings(tt=ttGraph,img=imgLimmaGUI)
    SetupPlotMenus(tt=ttGraph,initialfile="",plotFunction,img=imgLimmaGUI)  

    Try(tkgrid(imgLimmaGUI))
    Try(if (as.numeric(tclvalue(tkwinfo("reqheight",imgLimmaGUI)))<10)  # Nothing plotted.
      Try(tkdestroy(ttGraph))
    else
    {
      Try(tkwm.deiconify(ttGraph))
      Try(tkfocus(imgLimmaGUI))   
    })
    
    CopyToClip <- function()
    {
      Try(tkrreplot(imgLimmaGUI))
    }
  }

  if (runType!="runGraphicsOnly") 
  {
    Try(tt2 <-tktoplevel(ttMain))
    Try(tkwm.title(tt2,"Text Results of R Code Evaluation"))
    Try(scr <- tkscrollbar(tt2, repeatinterval=5,
                         command=function(...)tkyview(txt2,...)))
    Try(xscr <- tkscrollbar(tt2, repeatinterval=5,orient="horizontal",
                           command=function(...)tkxview(txt2,...)))
    Try(txt2 <- tktext(tt2,height=20,bg="white",
    yscrollcommand=function(...)tkset(scr,...),
    xscrollcommand=function(...)tkset(xscr,...),
    wrap="none",width=100,font=limmaGUIfontCourier))

    Try(copyText2 <- function() .Tcl(paste("event","generate",.Tcl.args(.Tk.ID(txt2),"<<Copy>>"))))

    Try(editPopupMenu2 <- tkmenu(txt2, tearoff=FALSE))
    Try(tkadd(editPopupMenu2, "command", label="Copy <Ctrl-C>",
    command=copyText2)) # ) # ,font=limmaGUIfontMenu))

    RightClick2 <- function(x,y) # x and y are the mouse coordinates
    {
     Try(rootx <- as.integer(tkwinfo("rootx",txt2)))
     Try(rooty <- as.integer(tkwinfo("rooty",txt2)))
     Try(xTxt <- as.integer(x)+rootx)
     Try(yTxt <- as.integer(y)+rooty)
     Try(.Tcl(paste("tk_popup",.Tcl.args(editPopupMenu2,xTxt,yTxt))))
    }
    Try(tkbind(txt2, "<Button-3>",RightClick2))

    Try(tkpack(scr, side="right", fill="y"))
    Try(tkpack(xscr, side="bottom", fill="x"))
    Try(tkpack(txt2, side="left", fill="both", expand="yes"))

    Try(chn <- tkopen("tmpEvalRcodeResults", "r"))
    Try(tkinsert(txt2, "0.0", tclvalue(tkread(chn))))
    Try(tkclose(chn))
    Try(tkfocus(tt2))
    SaveTextResults <- function()
    {
      Try(fileName<- tclvalue(tkgetSaveFile(initialfile="RcodeResults.txt",filetypes="{{Text Files} {.txt}} {{All files} *}")))
      Try(if (!nchar(fileName))        return())

      if (nchar(fileName)==0) return()
      Try(len <- nchar(fileName))
      if (len<=4)
        Try( fileName <- paste(fileName,".txt",sep=""))
      else if (substring(fileName,len-3,len)!=".txt")
      Try(fileName <- paste(fileName,".txt",sep=""))    
      Try(chn <- tkopen(fileName,"w"))
      Try(tkputs(chn, tclvalue(tkget(txt2,"0.0","end"))))
      Try(tkclose(chn))
    }

    Try(topMenu2 <- tkmenu(tt2))
    Try(tkconfigure(tt2, menu=topMenu2))
    Try(fileMenu2 <- tkmenu(topMenu2, tearoff=FALSE))
    Try(editMenu2 <- tkmenu(topMenu2, tearoff=FALSE))

    Try(tkadd(fileMenu2, "command", label="Save As",
    command=SaveTextResults)) # ) # ,font=limmaGUIfontMenu))
    Try(tkadd(fileMenu2, "command", label="Close",
    command=function() tkdestroy(tt2))) # ) # ,font=limmaGUIfontMenu))
    Try(tkadd(topMenu2, "cascade", label="File",
    menu=fileMenu2)) # ) # ,font=limmaGUIfontMenu))

    Try(tkadd(editMenu2, "command", label="Copy <Ctrl-C>",
    command=copyText2)) # ) # ,font=limmaGUIfontMenu))
    Try(tkadd(topMenu2, "cascade", label="Edit",
    menu=editMenu2)) # ) # ,font=limmaGUIfontMenu))

  }
  Try(tkconfigure(ttMain,cursor="arrow"))
  Try(tkconfigure(ttEvalRcode,cursor="arrow"))
  }

  Try(runTextOnly <- function() runOverall("runTextOnly"))
  Try(runGraphicsOnly <- function() runOverall("runGraphicsOnly"))
  Try(runTextAndGraphics <- function() runOverall("runTextAndGraphics"))

  MakeLimmaGUIMenu <- function()
  {
    Try(code <- tclvalue(tkget(txt,"0.0","end")))
    Try(codeGraph <- paste("assign(\"plotFunction\",function () {\nopar<-par(bg=\"white\")\nTry({\n",code,"\n})\n\ntempGraphPar <- par(opar)\n},limmaGUIenvironment)\n",sep=""))
    Try(menuNameObject <- GetMenuName())
    Try(if (length(menuNameObject)==0) return())    
    Try(addMenuItem(codeGraph,menuNameObject$MenuName,newMenu=TRUE,menuPosition="end",
      menuNameObject$MenuItemName,newMenuItem=TRUE,menuItemPosition="end",
            outputHasGraphics=TRUE))  
  }

  Try(HTMLhelp <- function() help.start())

  Try(topMenu <- tkmenu(ttEvalRcode ))
  Try(tkconfigure(ttEvalRcode , menu=topMenu))
  Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(runMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(helpMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(tkadd(fileMenu, "command", label="Open",
  command=OpenRSourceFile)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(fileMenu, "command", label="Save As",
  command=SaveRSourceFile)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(fileMenu, "command", label="Close",
  command=function() tkdestroy(ttEvalRcode ))) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(topMenu, "cascade", label="File",
  menu=fileMenu)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(editMenu, "command", label="Cut <Ctrl-X>",
  command=cutText)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",
  command=copyText)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(editMenu, "command", label="Paste <Ctrl-V>",
  command=pasteText)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(topMenu, "cascade", label="Edit",
  menu=editMenu)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(runMenu,"command",label="Show Text Results only",
  command=runTextOnly)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(runMenu,"command",label="Show Graphical Results only",
  command=runGraphicsOnly)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(runMenu,"command",label="Show Text and Graphics",
  command=runTextAndGraphics)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(topMenu, "cascade", label="Run",
  menu=runMenu)) # ) # ,font=limmaGUIfontMenu))
  
#  Try(menuMenu <- tkmenu(topMenu,tearoff=FALSE))
#  Try(tkadd(menuMenu,"command",label="Make limmaGUI Custom Menu Item",command=MakeLimmaGUIMenu))
#  Try(tkadd(topMenu,"cascade",label="Make limmaGUI Menu Item",menu=menuMenu))
  
  Try(tkadd(helpMenu,"command",label="HTML Help",
  command=HTMLhelp)) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(topMenu,"cascade",label="Help",
  menu=helpMenu)) # ) # ,font=limmaGUIfontMenu))
}

OpenGALandTargetsandSpotTypesfiles <- function()
{
  Try(ttGALandTargetsandSpotTypes<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttGALandTargetsandSpotTypes))
  Try(tkgrab.set(ttGALandTargetsandSpotTypes))
  Try(tkfocus(ttGALandTargetsandSpotTypes))
  Try(tkwm.title(ttGALandTargetsandSpotTypes,"Open GAL and Targets and Spot Types Files"))
  Try(tkgrid(tklabel(ttGALandTargetsandSpotTypes,text="    ")))

  OpenTargetsFileAndSetCursor <- function()
  {
      Try(tkconfigure(ttGALandTargetsandSpotTypes,cursor="watch"))
      Try(tkfocus(ttGALandTargetsandSpotTypes))
      Try(OpenTargetsFile())
      Try(tkconfigure(ttGALandTargetsandSpotTypes,cursor="arrow"))  
      Try(tkfocus(ttGALandTargetsandSpotTypes))
  }

  OpenGALFileAndSetCursor <- function()
  {
      Try(tkconfigure(ttGALandTargetsandSpotTypes,cursor="watch"))
      Try(tkfocus(ttGALandTargetsandSpotTypes))
      Try(OpenGALFile())
      Try(tkconfigure(ttGALandTargetsandSpotTypes,cursor="arrow"))  
      Try(tkfocus(ttGALandTargetsandSpotTypes))
  }

  OpenSpotTypesFileAndSetCursor <- function()
  {
      Try(tkconfigure(ttGALandTargetsandSpotTypes,cursor="watch"))
      Try(tkfocus(ttGALandTargetsandSpotTypes))
      Try(OpenSpotTypesFile())
      Try(tkconfigure(ttGALandTargetsandSpotTypes,cursor="arrow"))  
      Try(tkfocus(ttGALandTargetsandSpotTypes))
  }

  Try(OpenGALFile.but <- tkbutton(ttGALandTargetsandSpotTypes, text="Select GAL File",command=OpenGALFile,font=limmaGUIfont2))
  Try(OpenTargetsFile.but <- tkbutton(ttGALandTargetsandSpotTypes, text="Select Targets File",command=OpenTargetsFile,font=limmaGUIfont2))
  Try(OpenSpotTypesFile.but <- tkbutton(ttGALandTargetsandSpotTypes, text="Select Spot-Types File",command=OpenSpotTypesFileAndSetCursor,font=limmaGUIfont2))

  Try(GALfileBoxTitleLabel<-tklabel(ttGALandTargetsandSpotTypes,text=as.character(tclvalue(GALfileBoxTitle)),font=limmaGUIfont2))
  Try(GALfileNameLabel<-tklabel(ttGALandTargetsandSpotTypes,text=as.character(tclvalue(GALfileName)),background="white",font=limmaGUIfont2))
  Try(tkconfigure(GALfileBoxTitleLabel, textvariable=GALfileBoxTitle))
  Try(tkconfigure(GALfileNameLabel, textvariable=GALfileName))

  Try(tkgrid(tklabel(ttGALandTargetsandSpotTypes,text="    ")))
  Try(tkgrid(GALfileBoxTitleLabel,columnspan=4))
  Try(tkgrid(GALfileNameLabel,columnspan=4))

  Try(TargetsfileBoxTitleLabel <- tklabel(ttGALandTargetsandSpotTypes,text=as.character(tclvalue(TargetsfileBoxTitle)),font=limmaGUIfont2))
  Try(TargetsfileNameLabel <- tklabel(ttGALandTargetsandSpotTypes,text=as.character(tclvalue(TargetsfileName)),background="white",font=limmaGUIfont2))
  Try(tkconfigure(TargetsfileBoxTitleLabel, textvariable=TargetsfileBoxTitle))
  Try(tkconfigure(TargetsfileNameLabel, textvariable=TargetsfileName))

  Try(tkgrid(tklabel(ttGALandTargetsandSpotTypes,text="    ")))
  Try(tkgrid(TargetsfileBoxTitleLabel,columnspan=4))
  Try(tkgrid(TargetsfileNameLabel,columnspan=4))

  Try(SpotTypesfileBoxTitleLabel <- tklabel(ttGALandTargetsandSpotTypes,text=as.character(tclvalue(SpotTypesfileBoxTitle)),font=limmaGUIfont2))
  Try(SpotTypesfileNameLabel <- tklabel(ttGALandTargetsandSpotTypes,text=as.character(tclvalue(SpotTypesfileName)),background="white",font=limmaGUIfont2))
  Try(tkconfigure(SpotTypesfileBoxTitleLabel, textvariable=SpotTypesfileBoxTitle))
  Try(tkconfigure(SpotTypesfileNameLabel, textvariable=SpotTypesfileName))

  Try(tkgrid(tklabel(ttGALandTargetsandSpotTypes,text="    ")))
  Try(tkgrid(SpotTypesfileBoxTitleLabel,columnspan=4))
  Try(tkgrid(SpotTypesfileNameLabel,columnspan=4))
  
  Try(tkgrid(tklabel(ttGALandTargetsandSpotTypes,text="    ")))
  Try(tkgrid(OpenGALFile.but, OpenTargetsFile.but,OpenSpotTypesFile.but))
  Try(Abort <- 1)
  onOK <- function()
  {
      Try(gal     <- get("gal",envir=limmaGUIenvironment))
      Try(Targets <- get("Targets",envir=limmaGUIenvironment))  
      Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))    
      Try(if (length(gal)==0)
      {
        Try(tkmessageBox(title="GAL (GenePix Array List) File",message=paste("Either you did not specify a valid GAL (GenePix Array List) File",
          "or an error occurred while reading in the GAL file.  It should be in tab-delimited text format and it should include the column headings \"Block\", \"Column\", \"Row\", \"Name\" and \"ID\"."),icon="error"))        
        onCancel()
        return()
      })
      Try(if (length(Targets)==0)
      {
        Try(tkmessageBox(title="RNA Targets (Hybridizations) File",message=paste("Either you did not specify a valid RNA Targets (Hybridizations) File",
          "or an error occurred while reading in the Targets file.  It should be in tab-delimited text format and it should include the column headings \"SlideNumber\", \"FileName\", \"Cy3\" and \"Cy5\"",
          "(except for the case of ImaGene data which requires \"FileNameCy3\" and \"FileNameCy5\" column headings instead of \"FileName\".)"),icon="error"))
        onCancel()
        return()
      })
      Try(if (length(SpotTypes)==0)
      {
        Try(tkmessageBox(title="Spot Types",message=paste("Warning: You did not specify a valid spot types file, so limmaGUI will assume that all spots are genes."),icon="warning"))
        Try(assign("SpotTypes" , data.frame(SpotType=I("gene"),ID=I("*"),Name=I("*"),Color=I("black")),limmaGUIenvironment))
        Try(UpdateSpotTypesStatus())        
      })
      Try(tkgrab.release(ttGALandTargetsandSpotTypes));
      Try(tkdestroy(ttGALandTargetsandSpotTypes));
      Try(tkfocus(ttMain))
      Try(Abort <<- 0)
  }
  onCancel <- function() {Try(tkgrab.release(ttGALandTargetsandSpotTypes));Try(tkdestroy(ttGALandTargetsandSpotTypes));Try(tkfocus(ttMain));Try(Abort<<-1)}
  Try(OK.but <-tkbutton(ttGALandTargetsandSpotTypes,text="   OK   ",command=onOK,font=limmaGUIfont2))
  Try(Cancel.but <-tkbutton(ttGALandTargetsandSpotTypes,text=" Cancel ",command=onCancel,font=limmaGUIfont2))
  Try(tkgrid(tklabel(ttGALandTargetsandSpotTypes,text="    ")))
  Try(tkgrid(tklabel(ttGALandTargetsandSpotTypes,text="    "),OK.but,Cancel.but))
  Try(tkgrid(tklabel(ttGALandTargetsandSpotTypes,text="       ")))
  Try(tkfocus(ttGALandTargetsandSpotTypes))
  Try(tkbind(ttGALandTargetsandSpotTypes, "<Destroy>", function() {Try(tkgrab.release(ttGALandTargetsandSpotTypes));Try(tkfocus(ttMain));}))
  Try(tkwait.window(ttGALandTargetsandSpotTypes))
   
  if (Abort==1)
        return(0)

  #OK
  Try(ReturnVal<- ReadImageProcessingFiles())
  if (ReturnVal==0) return(0)
  Try(ReturnVal <- GetlimmaDataSetName())
  if (ReturnVal==0) return(0)        
  return(1)
 
}

GetlimmaDataSetName <- function()
{
  Try(limmaDataSetName <- get("limmaDataSetName",envir=.GlobalEnv))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
  Try(ttGetlimmaDataSetName<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttGetlimmaDataSetName))
  Try(tkgrab.set(ttGetlimmaDataSetName))
  Try(tkfocus(ttGetlimmaDataSetName))
  Try(tkwm.title(ttGetlimmaDataSetName,"Data Set Name"))
  Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="    ")))
  if (limmaDataSetNameText=="Untitled")
      Try(limmaDataSetNameText <- "")
  Try(Local.limmaDataSetName <- tclVar(init=limmaDataSetNameText))
  Try(entry.limmaDataSetName <-tkentry(ttGetlimmaDataSetName,width="20",font=limmaGUIfont2,textvariable=Local.limmaDataSetName,bg="white"))
  Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="Please enter a name for this data set.",font=limmaGUIfont2)))
  Try(tkgrid(entry.limmaDataSetName))
  onOK <- function()
  {    
      Try(limmaDataSetNameText <- tclvalue(Local.limmaDataSetName))
      if (nchar(limmaDataSetNameText)==0)
        limmaDataSetNameText <- "Untitled"
#      Try(tkwm.title(ttMain,paste("LimmaGUI -",limmaDataSetNameText)))
      Try(assign("limmaDataSetNameText",limmaDataSetNameText,limmaGUIenvironment))
      Try(tclvalue(limmaDataSetName) <- limmaDataSetNameText)
      Try(tkgrab.release(ttGetlimmaDataSetName));Try(tkdestroy(ttGetlimmaDataSetName));Try(tkfocus(ttMain))
  }
  Try(OK.but <-tkbutton(ttGetlimmaDataSetName,text="   OK   ",command=onOK,font=limmaGUIfont2))
  Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="    ")))
  Try(tkgrid(OK.but))
  Try(tkgrid.configure(OK.but))
  Try(tkgrid(tklabel(ttGetlimmaDataSetName,text="       ")))
  Try(tkfocus(entry.limmaDataSetName))
  Try(tkbind(entry.limmaDataSetName, "<Return>",onOK))
  Try(tkbind(ttGetlimmaDataSetName, "<Destroy>", function(){Try(tkgrab.release(ttGetlimmaDataSetName));Try(tkfocus(ttMain));return(0)}))
  Try(tkwait.window(ttGetlimmaDataSetName))
  Try(tkfocus(ttMain))
  return (1)
}

GetParameterizationName <- function()
{
  Try(ttGetParameterizationName<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttGetParameterizationName))
  Try(tkgrab.set(ttGetParameterizationName))
  Try(tkwm.title(ttGetParameterizationName,"Parameterization Name"))
  Try(tkgrid(tklabel(ttGetParameterizationName,text="    ")))
  Try(Local.ParameterizationName <- tclVar(init=""))
  Try(entry.ParameterizationName <-tkentry(ttGetParameterizationName,width="20",font=limmaGUIfont2,textvariable=Local.ParameterizationName,bg="white"))
  Try(tkgrid(tklabel(ttGetParameterizationName,text="Please enter a name for this parameterization.",font=limmaGUIfont2),columnspan=2))
  Try(tkgrid(entry.ParameterizationName,columnspan=2))

  ReturnVal <- "GetParameterizationName.CANCEL"
  onOK <- function()
  {
      Try(ParameterizationNameText <- tclvalue(Local.ParameterizationName))
      Try(tkgrab.release(ttGetParameterizationName));Try(tkdestroy(ttGetParameterizationName));Try(tkfocus(ttMain))
      ReturnVal <<- ParameterizationNameText
  }
  onCancel <- function()
  {
      Try(tkgrab.release(ttGetParameterizationName));Try(tkdestroy(ttGetParameterizationName));Try(tkfocus(ttMain))
      ReturnVal <<- "GetParameterizationName.CANCEL"
  }
  Try(OK.but <-tkbutton(ttGetParameterizationName,text="   OK   ",command=onOK,font=limmaGUIfont2))
  Try(Cancel.but <-tkbutton(ttGetParameterizationName,text=" Cancel ",command=onCancel,font=limmaGUIfont2))
  Try(tkgrid(tklabel(ttGetParameterizationName,text="    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(ttGetParameterizationName,text="       ")))
  Try(tkfocus(entry.ParameterizationName))
  Try(tkbind(entry.ParameterizationName, "<Return>",onOK))
  Try(tkbind(ttGetParameterizationName, "<Destroy>", function(){Try(tkgrab.release(ttGetParameterizationName));Try(tkfocus(ttMain));Try(return("GetParameterizationName.CANCEL"))}))
  Try(tkwait.window(ttGetParameterizationName))
  Try(tkfocus(ttMain))
  Try(return (ReturnVal))
}

GetContrastsParameterizationName <- function()
{
  Try(ttGetContrastsParameterizationName<-tktoplevel(ttMain))
  Try(tkwm.deiconify(ttGetContrastsParameterizationName))
  Try(tkgrab.set(ttGetContrastsParameterizationName))
  Try(tkwm.title(ttGetContrastsParameterizationName,"Contrasts Name"))
  Try(tkgrid(tklabel(ttGetContrastsParameterizationName,text="    ")))
  Try(Local.ContrastsParameterizationName <- tclVar(init=""))
  Try(entry.ContrastsParameterizationName <-tkentry(ttGetContrastsParameterizationName,width="20",font=limmaGUIfont2,textvariable=Local.ContrastsParameterizationName,bg="white"))
  Try(tkgrid(tklabel(ttGetContrastsParameterizationName,text="Please enter a name for this set of contrasts.",font=limmaGUIfont2),columnspan=2))
  Try(tkgrid(entry.ContrastsParameterizationName,columnspan=2))

  ReturnVal <- "GetContrastsParameterizationName.CANCEL"
  onOK <- function()
  {
      Try(contrastsParameterizationNameText <- tclvalue(Local.ContrastsParameterizationName))
      Try(tkgrab.release(ttGetContrastsParameterizationName));Try(tkdestroy(ttGetContrastsParameterizationName));Try(tkfocus(ttMain))
      ReturnVal <<- contrastsParameterizationNameText
  }
  onCancel <- function()
  {
      Try(tkgrab.release(ttGetContrastsParameterizationName));Try(tkdestroy(ttGetContrastsParameterizationName));Try(tkfocus(ttMain))
      ReturnVal <<- "GetContrastsParameterizationName.CANCEL"
  }
  Try(OK.but <-tkbutton(ttGetContrastsParameterizationName,text="   OK   ",command=onOK,font=limmaGUIfont2))
  Try(Cancel.but <-tkbutton(ttGetContrastsParameterizationName,text=" Cancel ",command=onCancel,font=limmaGUIfont2))
  Try(tkgrid(tklabel(ttGetContrastsParameterizationName,text="    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(ttGetContrastsParameterizationName,text="       ")))
  Try(tkfocus(entry.ContrastsParameterizationName))
  Try(tkbind(entry.ContrastsParameterizationName, "<Return>",onOK))
  Try(tkbind(ttGetContrastsParameterizationName, "<Destroy>", function(){Try(tkgrab.release(ttGetContrastsParameterizationName));Try(tkfocus(ttMain));Try(return("GetContrastsParameterizationName.CANCEL"))}))
  Try(tkwait.window(ttGetContrastsParameterizationName))
  Try(tkfocus(ttMain))
  Try(return (ReturnVal))
}



NewLimmaFile <- function()
{
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(LimmaFileName <- get("LimmaFileName",envir=limmaGUIenvironment))
  
  if (limmaDataSetNameText!="Untitled")
  {
      Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled")  LimmaFileName <- limmaDataSetNameText)  # Local assignment only
      Try(mbVal <- tkmessageBox(title="Start New Analysis",
            message=paste("Save changes to ",LimmaFileName,"?",sep=""),
            icon="question",type="yesnocancel",default="yes"))
      if (tclvalue(mbVal)=="yes")
          Try(SaveLimmaFile())
      if (tclvalue(mbVal)=="cancel")
      {
          Try(tkfocus(ttMain))
          return()
      }
      Try(limmaDataSetNameText <- "Untitled")
  }
#  Try(tkmessageBox(title="Working Directory",message="After clicking OK, please select a working directory.",type="ok"))
  Try(WD <- SetWD())
  if (WD=="") return()

  Try(tkdelete(mainTree,"RG"))
  Try(tkdelete(mainTree,"WeightingType"))
  Try(tkdelete(mainTree,"MA"))  
  Try(tkdelete(mainTree,"Layout"))    
  Try(tkdelete(mainTree,"Parameterizations"))      
  Try(tkinsert(mainTree,"end","root","RG" ,text="R and G",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","RG","RG.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","root","WeightingType" ,text="Spot Quality Weighting",font=limmaGUIfontTree)) 
  Try(tkinsert(mainTree,"end","WeightingType","WeightingType.Status" ,text="none",font=limmaGUIfontTree))  
  Try(tkinsert(mainTree,"end","root","MA" ,text="M and A",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","MA","Raw" ,text="Raw",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","Raw","Raw.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","MA","WithinOnly" ,text="Within-Array Normalized",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","WithinOnly","WithinOnly.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","MA","BetweenOnly" ,text="Between-Array Normalized",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","BetweenOnly","BetweenOnly.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","MA","WithinAndBetween",text="Within and Between-Array Normalized",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","WithinAndBetween","WithinAndBetween.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","root","Layout", text="Layout",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","Layout","Layout.Status" ,text="Not Available",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","root","Parameterizations" ,text="Parameterizations",font=limmaGUIfontTree))
  Try(tkinsert(mainTree,"end","Parameterizations","Parameterizations.Status" ,text="None",font=limmaGUIfontTree))


  if (NumParameterizations>0)
    for (parameterizationIndex in (1:NumParameterizations))
    {
      Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
      Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
      Try(tkdelete(ParameterizationTREE,ParameterizationNameNode))
      Try(assign("ParameterizationList", deleteItemFromList(ParameterizationList,ParameterizationNameNode),limmaGUIenvironment))
    }
  Try(initGlobals())
  Try(LimmaFileName <- get("LimmaFileName",limmaGUIenvironment))
  Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only  
  Try(tkwm.title(ttMain,paste("LimmaGUI -",LimmaFileName)))
  Try(tclvalue(GALfileBoxTitle)     <- "Please select a GenePix Array List (GAL) file.")
  Try(tclvalue(GALfileName)         <- "No filename is selected at the moment.  Press the Select GAL File Button.")
  Try(tclvalue(TargetsfileBoxTitle) <- "Please select a tab-delimited file listing the microarray hybridizations.")
  Try(tclvalue(TargetsfileName)     <- "No filename is selected at the moment.  Press the Select Targets File Button.")
  Try(tclvalue(SpotTypesfileBoxTitle) <- "Please select a tab-delimited file listing the spot types.")
  Try(tclvalue(SpotTypesfileName) <- "No filename is selected at the moment.  Press the Select Spot-Types File Button.")
  Try(OpenGALandTargetsandSpotTypesfiles())
  Try(tkfocus(ttMain))
}

chooseDir <- function()
{
	Try(wd <- tclVar(getwd()))
	Try(ttChooseDir <- tktoplevel(ttMain))
	Try(tkwm.title(ttChooseDir,"Choose working directory"))
	Try(tkwm.deiconify(ttChooseDir))
	Try(tkgrab.set(ttChooseDir))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	Try(label1 <- tklabel(ttChooseDir,text="Choose working directory (containing input files):",font=limmaGUIfont2))
	Try(tkgrid(tklabel(ttChooseDir,text="    "),label1,sticky="w"))
	Try(tkgrid.configure(label1,columnspan=3))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	Try(onBrowse <- function() 
	{
	  Try(if (file.exists(tclvalue(wd))) initialdir<-gsub("/","\\\\",tclvalue(wd)) else initialdir<-gsub("/","\\\\",getwd()))
	  Try(dir1 <- tclvalue(tkchooseDirectory(title="Please choose a working directory for the Limma Analysis",initialdir=initialdir)))
		Try(if (nchar(dir1)>0) tclvalue(wd) <- dir1)
	})
	Try(ReturnVal <- "")
	Try(onOK <- function() {Try(DirChosen <- tclvalue(wd));Try(tkgrab.release(ttChooseDir));Try(tkdestroy(ttChooseDir)); Try(ReturnVal <<- DirChosen)})
	Try(onCancel <- function() {Try(tkgrab.release(ttChooseDir));Try(tkdestroy(ttChooseDir))})
	Try(Browse.but <- tkbutton(ttChooseDir,text="Browse",command=onBrowse,font=limmaGUIfont2))
	Try(OK.but <- tkbutton(ttChooseDir,text="    OK    ",command=onOK,font=limmaGUIfont2))
	Try(Cancel.but <- tkbutton(ttChooseDir,text=" Cancel ",command=onCancel,font=limmaGUIfont2))
	Try(entry1 <- tkentry(ttChooseDir,textvariable=wd,width=40))
	Try(tkgrid(tklabel(ttChooseDir,text="    "),entry1))
	Try(tkgrid.configure(entry1,columnspan=3))
  Try(tkgrid(tklabel(ttChooseDir,text="    "),row=3,column=4))
	Try(tkgrid(Browse.but,row=3,column=5))
  Try(tkgrid(tklabel(ttChooseDir,text="    "),row=3,column=6))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	Try(tkgrid(tklabel(ttChooseDir,text="    "),tklabel(ttChooseDir,text="    "),OK.but,Cancel.but))
	Try(tkgrid.configure(Cancel.but,sticky="w"))
	Try(tkgrid(tklabel(ttChooseDir,text="    ")))
	Try(tkfocus(entry1))
	Try(tkbind(ttChooseDir,"<Destroy>",function()tkgrab.release(ttChooseDir)))
	Try(tkbind(entry1,"<Return>",onOK))
	Try(tkwait.window(ttChooseDir))
	return(ReturnVal)	
}


SetWD <- function()
{
  WD <- chooseDir()
  if (!nchar(WD)) 
  {
      tkfocus(ttMain)
      return("")
  }
  Try(setwd(WD))
  tkfocus(ttMain)
  return(WD)
}

onExit <- function()
{
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
  Try(LimmaFileName <- get("LimmaFileName",envir=limmaGUIenvironment))
  if (limmaDataSetNameText!="Untitled")
  {
      Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled")  LimmaFileName <- limmaDataSetNameText)  # Local assignment only
      Try(mbVal <- tkmessageBox(title="Exit limmaGUI",
            message=paste("Save changes to ",LimmaFileName,"?",sep=""),
            icon="question",type="yesnocancel",default="yes"))
      if (tclvalue(mbVal)=="yes")
          Try(SaveLimmaFile())
      if (tclvalue(mbVal)=="cancel")
          return()
  }
  Try(assign(".JustAskedWhetherToSave",TRUE,.GlobalEnv))
  try(tkdestroy(ttMain),silent=TRUE)
}

DeleteParameterization <- function()
{
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))  
  
  if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="Delete Parameterization",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  }  
  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return()    )
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
  Try(LinearModelComputed[parameterizationIndex]<-FALSE)
  Try(assign("LinearModelComputed",LinearModelComputed,limmaGUIenvironment))
  
  ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep="")
  Try(tkdelete(ParameterizationTREE,ParameterizationNameNode))
  Try(ParameterizationList <- deleteItemFromList(ParameterizationList,ParameterizationNameNode))

  tempVec <- c()
  if (NumParameterizations>0)
    for (i in (1:NumParameterizations))
    {
      if (i!=parameterizationIndex)
          Try(tempVec <- c(tempVec,ParameterizationNamesVec[i]))
    }
  Try(ParameterizationNamesVec <- tempVec)

  tempVec2 <- c()
  if (NumParameterizations>0)
    for (i in (1:NumParameterizations))
    {
      if (i!=parameterizationTreeIndex)
          Try(tempVec2 <- c(tempVec2,ParameterizationTreeIndexVec[i]))
    }
  Try(ParameterizationTreeIndexVec <- tempVec2)

  Try(NumParameterizations <- NumParameterizations - 1)        
  
  Try(assign("ParameterizationTreeIndexVec",ParameterizationTreeIndexVec,limmaGUIenvironment))
  Try(assign("ParameterizationNamesVec",ParameterizationNamesVec,limmaGUIenvironment))
  Try(assign("ParameterizationList",ParameterizationList,limmaGUIenvironment))
  Try(assign("NumParameterizations",NumParameterizations,limmaGUIenvironment))  

  Try(tkdelete(mainTree,"Parameterizations"))
  Try(tkinsert(mainTree,"end","root","Parameterizations" ,text="Parameterizations",font=limmaGUIfontTree))
  if (NumParameterizations>0)
  {
    for (i in (1:NumParameterizations))
    {
      Try(ParameterizationsStatusNameNode <- paste("Parameterizations.Status.",i,sep=""))
      Try(tkinsert(mainTree,"end","Parameterizations",ParameterizationsStatusNameNode ,text=ParameterizationNamesVec[i],font=limmaGUIfontTree))
    }
  } else
    Try(tkinsert(mainTree,"end","Parameterizations","Parameterizations.Status.1" ,text="None",font=limmaGUIfontTree)) 
}

ChooseContrastsParameterization <- function(parameterizationTreeIndex)
{
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(NumContrastParameterizations <- ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations)
  Try(if (NumContrastParameterizations==0)
  {
    Try(tkmessageBox(title="Choose Contrasts Parameterization",message=paste("There are no contrasts parameterizations available for parameterization ",ParameterizationNamesVec[parameterizationIndex],".",sep=""),type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  })      
  Try(ContrastsParameterizationNamesVec <- c() )
  Try(for (i in(1:NumContrastParameterizations))
    ContrastsParameterizationNamesVec[i] <- ParameterizationList[[ParameterizationNameNode]]$Contrasts[[i]]$contrastsParameterizationNameText)
  
  ttChooseContrastsParameterization<-tktoplevel(ttMain)
  tkwm.deiconify(ttChooseContrastsParameterization)
  tkgrab.set(ttChooseContrastsParameterization)  
  tkfocus(ttChooseContrastsParameterization)
  tkwm.title(ttChooseContrastsParameterization,"Choose a Contrasts Parameterization")
  scr <- tkscrollbar(ttChooseContrastsParameterization, repeatinterval=5,
                       command=function(...)tkyview(tl,...))
  ## Safest to make sure scr exists before setting yscrollcommand
  tl<-tklistbox(ttChooseContrastsParameterization,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white",font=limmaGUIfont2)   
  lbl2<-tklabel(ttChooseContrastsParameterization,text="Which contrasts parameterization is this for?",font=limmaGUIfont2)
  tkgrid(tklabel(ttChooseContrastsParameterization,text="       "),row=0,column=1,columnspan=1)
  tkgrid(tklabel(ttChooseContrastsParameterization,text="       "),row=0,column=4,columnspan=1)
  tkgrid(lbl2,row=1,column=2,columnspan=2,rowspan=1);
  tkgrid.configure(lbl2,sticky="w")
  tkgrid(tklabel(ttChooseContrastsParameterization,text="         "),row=2,column=1)
  tkgrid(tl,row=2,column=2,columnspan=2,rowspan=4,sticky="ew")
  tkgrid(scr,row=2,column=3,columnspan=1,rowspan=4,sticky="wns")
  if (NumContrastParameterizations>0)
    for (i in (1:NumContrastParameterizations))
       tkinsert(tl,"end",ContrastsParameterizationNamesVec[i])
  tkselection.set(tl,0)

  ReturnVal <- 0
  onOK <- function()
  {
      Try(contrastsParameterizationIndex <- as.numeric(tclvalue(tkcurselection(tl)))+1)
      Try(tkgrab.release(ttChooseContrastsParameterization));Try(tkdestroy(ttChooseContrastsParameterization));Try(tkfocus(ttMain))
      ReturnVal <<- contrastsParameterizationIndex
  }
  onCancel <- function() {Try(tkgrab.release(ttChooseContrastsParameterization));Try(tkdestroy(ttChooseContrastsParameterization));Try(tkfocus(ttMain));ReturnVal <<- 0}
  OK.but <-tkbutton(ttChooseContrastsParameterization,text="   OK   ",command=onOK,font=limmaGUIfont2)
  Cancel.but <-tkbutton(ttChooseContrastsParameterization,text=" Cancel ",command=onCancel,font=limmaGUIfont2)
  tkgrid(tklabel(ttChooseContrastsParameterization,text="    "))
  tkgrid(tklabel(ttChooseContrastsParameterization,text="    "),tklabel(ttChooseContrastsParameterization,text="    "),OK.but,Cancel.but)
  tkgrid.configure(OK.but,    sticky="e")
  tkgrid.configure(Cancel.but,sticky="w")
  
  tkgrid(tklabel(ttChooseContrastsParameterization,text="    "))
  Try(tkfocus(ttChooseContrastsParameterization))
  Try(tkbind(ttChooseContrastsParameterization, "<Destroy>", function() {Try(tkgrab.release(ttChooseContrastsParameterization));Try(tkfocus(ttMain))}))
  Try(tkwait.window(ttChooseContrastsParameterization))

  return (ReturnVal)
}

DeleteContrastsParameterization <- function()
{
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  
  Try(if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="Delete Contrasts Parameterization",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  })  
  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return()    )
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(NumContrastParameterizations <- ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations)

  Try(if (NumContrastParameterizations==0)
  {
    Try(tkmessageBox(title="Delete Contrasts Parameterization",message=paste("There are no contrasts parameterizations available for parameterization ",ParameterizationNamesVec[parameterizationIndex],".",sep=""),type="ok",icon="error"))
    Try(tkfocus(ttMain))
    return()  
  })  
  Try(contrastsParameterizationIndex <- ChooseContrastsParameterization(parameterizationTreeIndex))
  Try(if (contrastsParameterizationIndex==0)    return()    )
  Try(ContrastsParameterizationTreeIndexVec <- ParameterizationList[[ParameterizationNameNode]]$ContrastsParameterizationTreeIndexVec)
  Try(contrastsParameterizationTreeIndex <- ContrastsParameterizationTreeIndexVec[contrastsParameterizationIndex])          
  Try(ContrastsParameterizationTreeNode <- paste("ContrastsParameterizationNames.",parameterizationTreeIndex,".",contrastsParameterizationTreeIndex,sep=""))
  
  Try(ContrastsParameterizationListNode <- paste("ContrastsParameterization.",parameterizationTreeIndex,".",contrastsParameterizationTreeIndex, sep=""))  
  
  Try(tkdelete(ParameterizationTREE,ContrastsParameterizationTreeNode))
  Try(ParameterizationList[[ParameterizationNameNode]]$Contrasts <- deleteItemFromList(ParameterizationList[[ParameterizationNameNode]]$Contrasts,
                            ContrastsParameterizationListNode))
  Try(tempVec <- c())
  Try(if (NumContrastParameterizations>0)
    Try(for (i in (1:NumContrastParameterizations))
    {
      Try(if (i!=contrastsParameterizationTreeIndex)
          Try(tempVec <- c(tempVec,ContrastsParameterizationTreeIndexVec[i])))
    }))
  Try(ContrastsParameterizationTreeIndexVec <- tempVec)

  Try(ParameterizationList[[ParameterizationNameNode]]$ContrastsParameterizationTreeIndexVec <- ContrastsParameterizationTreeIndexVec)

  Try(NumContrastParameterizations <- NumContrastParameterizations - 1)        
  Try(ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations <- NumContrastParameterizations)  
  Try(ParamContrastsNode <- paste("ParamContrasts.",parameterizationTreeIndex,sep=""))
  Try(if (ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations==0)
    Try(tkinsert(ParameterizationTREE,"0",ParamContrastsNode,paste("ContrastsParameterizationNames.",parameterizationTreeIndex,".1",sep=""),text="none",font=limmaGUIfontTree)))
  Try(assign("ParameterizationList",ParameterizationList,limmaGUIenvironment))
}


OpenLimmaFile <- function()
{
  Try(limmaDataSetName <- get("limmaDataSetName",envir=.GlobalEnv))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
  Try(LimmaFileName <- get("LimmaFileName",envir=limmaGUIenvironment))
  Try(tempLimmaFileName <- tclvalue(tkgetOpenFile(filetypes="{{Limma Files} {.lma}} {{All files} *}")))
  if (!nchar(tempLimmaFileName)) 
  {
    tkfocus(ttMain)
    return()
  }
  limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment)
  if (limmaDataSetNameText!="Untitled")
  {
      Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled")  LimmaFileName <- limmaDataSetNameText)  # Local assignment only
      mbVal <- tkmessageBox(title="Open File",
            message=paste("Save changes to ",LimmaFileName,"?",sep=""),
            icon="question",type="yesnocancel",default="yes")
      if (tclvalue(mbVal)=="yes")
          SaveLimmaFile()
      if (tclvalue(mbVal)=="cancel")
          return()
  }
  Try(LimmaFileName <- tempLimmaFileName)
  Try(assign("LimmaFileName",LimmaFileName,limmaGUIenvironment))
  
  Try(tkconfigure(ttMain,cursor="watch"))
  Try(tkfocus(ttMain))

  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))        
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))

# Using existing NumParameterizations, NOT the one loaded from the .lma file.
# (We haven't loaded it yet.)
  if (NumParameterizations>0)
    for (parameterizationIndex in (1:NumParameterizations))
    {
      Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
      Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
      Try(tkdelete(ParameterizationTREE,ParameterizationNameNode))
      Try(assign("ParameterizationList",deleteItemFromList(ParameterizationList,ParameterizationNameNode),limmaGUIenvironment))         
    }

  # Load the RData File whose name is "LimmaFileName"
  load(LimmaFileName,envir=limmaGUIenvironment)

  # The user may have changed the filename in the operating system since the last save.
  Try(LimmaFileName <- tempLimmaFileName)
  Try(assign("LimmaFileName",LimmaFileName,limmaGUIenvironment))
  
  Try(limmaDataSetNameText <- get("limmaDataSetNameText" , envir=limmaGUIenvironment))
  Try(NumParameterizations <- get("NumParameterizations", envir=limmaGUIenvironment))  
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec", envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(NumParameters <- get("NumParameters" , envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
  Try(numSpotTypes <- nrow(SpotTypes))

  Try(LimmaFileName <- get("LimmaFileName",envir=limmaGUIenvironment))
  Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only 
  Try(tkwm.title(ttMain,paste("LimmaGUI -",LimmaFileName)))
  Try(assign("limmaDataSetNameText",limmaDataSetNameText,limmaGUIenvironment))
  Try(tclvalue(limmaDataSetName) <- limmaDataSetNameText)  

  if (NumParameterizations>0)
    for (parameterizationIndex in (1:NumParameterizations))
    {
      Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
      Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
      Try(ParamMainFitNode   <- paste("ParamMainFit.",parameterizationTreeIndex,sep=""))
      Try(ParamContrastsNode <- paste("ParamContrasts.",parameterizationTreeIndex,sep=""))
      Try(PMFNoneNode <- paste("PMFNone.",parameterizationTreeIndex,sep=""))
      Try(PMFSpotTypesNode <- paste("PMFSpotTypes.",parameterizationTreeIndex,sep=""))        
      Try(PMFMANormMethodNode <- paste("PMFMANormMethod.",parameterizationTreeIndex,sep=""))    
      Try(PMFParamsNode <- paste("PMFParams.",parameterizationTreeIndex,sep=""))
      Try(PMFLinModNode <- paste("PMFLinMod.",parameterizationTreeIndex,sep=""))
      Try(PMFLinModStatusNode <- paste("PMFLinModStatus.",parameterizationTreeIndex,sep=""))
      Try(PMFEBayesNode <- paste("PMFEBayes.",parameterizationTreeIndex,sep=""))
      Try(PMFEBayesStatusNode <- paste("PMFEBayesStatus.",parameterizationTreeIndex,sep=""))
      Try(PMFDupCorNode <- paste("PMFDupCor.",parameterizationTreeIndex,sep=""))
      Try(PMFDupCorStatusNode <- paste("PMFDupCorStatus.",parameterizationTreeIndex,sep=""))
      Try(tkinsert(ParameterizationTREE,"end","root",ParameterizationNameNode ,text=ParameterizationNamesVec[parameterizationIndex],font=limmaGUIfontTree))
      Try(tkinsert(ParameterizationTREE,"0",ParameterizationNameNode,ParamMainFitNode,text="Main fit",font=limmaGUIfontTree))
      Try(tkinsert(ParameterizationTREE,"1",ParameterizationNameNode,ParamContrastsNode,text="Contrasts",font=limmaGUIfontTree))      
      Try(if (ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations==0)
        Try(tkinsert(ParameterizationTREE,"0",ParamContrastsNode,paste("ContrastsParameterizationNames.",parameterizationTreeIndex,".1",sep=""),text="none",font=limmaGUIfontTree)))
          Try(ContrastsParameterizationTreeIndexVec <- ParameterizationList[[ParameterizationNameNode]]$ContrastsParameterizationTreeIndexVec)

      Try(if (ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations>0)
        for (contrastIndex in (1:ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations))        
        {
          Try(contrastsParameterizationTreeIndex <- ContrastsParameterizationTreeIndexVec[contrastIndex])
          Try(ContrastsParameterizationNamesNode <- paste("ContrastsParameterizationNames.",parameterizationTreeIndex,".",contrastsParameterizationTreeIndex,sep=""))
          Try(contrastsParameterizationNameText <- ParameterizationList[[ParameterizationNameNode]]$Contrasts[[contrastIndex]]$contrastsParameterizationNameText)
          Try(tkinsert(ParameterizationTREE,"end",ParamContrastsNode,ContrastsParameterizationNamesNode,text=contrastsParameterizationNameText,font=limmaGUIfontTree))
          Try(contrastsMatrixInList <- ParameterizationList[[ParameterizationNameNode]]$Contrasts[[contrastIndex]]$contrastsMatrixInList)
          Try(contrastsMatrix <- contrastsMatrixInList$contrasts)          
          Try(ContrastsNames <- colnames(contrastsMatrix))          
          Try(NumContrasts <- ncol(contrastsMatrix))
          Try(for (contrast in (1:NumContrasts))
          {
            Try(tkinsert(ParameterizationTREE,"end",ContrastsParameterizationNamesNode,paste("Contrasts.",parameterizationIndex,".",contrastsParameterizationTreeIndex,".",contrast,sep=""),text=ContrastsNames[contrast],font=limmaGUIfontTree))
          })
        })
      Try(tkinsert(ParameterizationTREE,"0",ParamMainFitNode,PMFSpotTypesNode,text="Spot Type(s) Included",font=limmaGUIfontTree))  
      Try(SpotTypesForLinearModel <- ParameterizationList[[ParameterizationNameNode]]$SpotTypesForLinearModel)
      Try(if (numSpotTypes>0)
        for (i in (1:numSpotTypes))
        {
          if (SpotTypesForLinearModel[i]==FALSE)
              next()
          Try(NewNode <- paste("PMFSpotTypes.",parameterizationTreeIndex,".",i,sep=""))
          Try(tkinsert(ParameterizationTREE,"0",PMFSpotTypesNode, NewNode,text=SpotTypes[i,"SpotType"],font=limmaGUIfontTree))
        })
          
      Try(tkinsert(ParameterizationTREE,"end",ParamMainFitNode,PMFMANormMethodNode,text="M A Normalization Method",font=limmaGUIfontTree))
      Try(if (!exists("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
      {
        Try(WithinArrayNormalizationMethod <- "printtiploess")
        Try(assign("WithinArrayNormalizationMethod",WithinArrayNormalizationMethod,limmaGUIenvironment))
      })
      Try(WithinArrayNormalizationMethod <- get("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))      
      Try(if ("WithinArrayNormalizationMethod" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
        Try(WithinArrayNormalizationMethod <- (ParameterizationList[[ParameterizationNameNode]])$WithinArrayNormalizationMethod)
      else
      {
        Try(ParameterizationList[[ParameterizationNameNode]][["WithinArrayNormalizationMethod"]] <- WithinArrayNormalizationMethod)
        Try(assign("ParameterizationList",ParameterizationList,limmaGUIenvironment))
      })      
      Try(MANormMethodValueNode <- paste("PMFMANormMethod.",parameterizationTreeIndex,".",1,sep=""))
      Try(if (ParameterizationList[[ParameterizationNameNode]][[MANormMethodValueNode]]=="Within arrays only")
        ParameterizationList[[ParameterizationNameNode]][[MANormMethodValueNode]] <- paste("Within arrays (",WithinArrayNormalizationMethod,") only",sep=""))
      Try(if (ParameterizationList[[ParameterizationNameNode]][[MANormMethodValueNode]]=="Within and between arrays")
        ParameterizationList[[ParameterizationNameNode]][[MANormMethodValueNode]] <- paste("Within arrays (",WithinArrayNormalizationMethod,") and between arrays",sep=""))
      Try(assign("ParameterizationList",ParameterizationList,limmaGUIenvironment))
      Try(tkinsert(ParameterizationTREE,"end",PMFMANormMethodNode, MANormMethodValueNode,
        text=(ParameterizationList[[ParameterizationNameNode]])[[MANormMethodValueNode]],
      font=limmaGUIfontTree))        
      Try(tkinsert(ParameterizationTREE,"end",ParamMainFitNode,PMFParamsNode,text="Parameters",font=limmaGUIfontTree))
      Try(tkinsert(ParameterizationTREE,"end",ParamMainFitNode,PMFLinModNode,text="Linear Model Fit",font=limmaGUIfontTree))
      Try(tkinsert(ParameterizationTREE,"0",PMFLinModNode,PMFLinModStatusNode,text=
          (ParameterizationList[[ParameterizationNameNode]])[[PMFLinModStatusNode]],font=limmaGUIfontTree))
      Try(tkinsert(ParameterizationTREE,"end",ParamMainFitNode,PMFEBayesNode,text="Empirical Bayes Statistics",font=limmaGUIfontTree))
      Try(tkinsert(ParameterizationTREE,"0",PMFEBayesNode,PMFEBayesStatusNode,text=
          (ParameterizationList[[ParameterizationNameNode]])[[PMFEBayesStatusNode]],font=limmaGUIfontTree)) 
      Try(tkinsert(ParameterizationTREE,"end",ParamMainFitNode,PMFDupCorNode,text="Duplicate Correlation",font=limmaGUIfontTree))
      Try(tkinsert(ParameterizationTREE,"0",PMFDupCorNode,PMFDupCorStatusNode,text=
          (ParameterizationList[[ParameterizationNameNode]])[[PMFDupCorStatusNode]],font=limmaGUIfontTree)) 
      Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))      
      Try(if (NumParameters>0)
        for (i in (1:NumParameters))
        {
          Try(NewNode <- paste("PMFParams.",parameterizationIndex,".",i,sep=""))
          Try(tkinsert(ParameterizationTREE,"end",PMFParamsNode, NewNode,text=ParameterNamesVec[i],font=limmaGUIfontTree))
        })
    } 
      
  Try(tkdelete(mainTree,"RG.Status"))
  Try(tkdelete(mainTree,"WeightingType.Status"))
  Try(tkdelete(mainTree,"Raw.Status"))  
  Try(tkdelete(mainTree,"WithinOnly.Status"))    
  Try(tkdelete(mainTree,"BetweenOnly.Status"))      
  Try(tkdelete(mainTree,"WithinAndBetween.Status"))        
  Try(tkdelete(mainTree,"Layout.Status"))          
  Try(tkdelete(mainTree,"Parameterizations"))
  Try(tkinsert(mainTree,"end","root","Parameterizations" ,text="Parameterizations",font=limmaGUIfontTree))  
  
  Try(RG.Available     <- get("RG.Available" , envir=limmaGUIenvironment))  
  Try(MA.Available     <- get("MA.Available" , envir=limmaGUIenvironment))    
  Try(Layout.Available <- get("Layout.Available" , envir=limmaGUIenvironment))      
  Try(WeightingType    <- get("WeightingType", envir=limmaGUIenvironment))
  
  Try(tkinsert(mainTree,"end","WeightingType","WeightingType.Status",text=WeightingType,font=limmaGUIfontTree))  
  Try(if (RG.Available)  
    Try(tkinsert(mainTree,"end","RG","RG.Status" ,text="Available",font=limmaGUIfontTree))
  else
    Try(tkinsert(mainTree,"end","RG","RG.Status" ,text="Not Available",font=limmaGUIfontTree))    )
  Try(if (MA.Available$Raw)
    Try(tkinsert(mainTree,"end","Raw","Raw.Status" ,text="Available",font=limmaGUIfontTree))
  else  
    Try(tkinsert(mainTree,"end","Raw","Raw.Status" ,text="Not Available",font=limmaGUIfontTree))  )
  Try(if (MA.Available$WithinArrays)  
  {
    Try(if (!exists("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
    {
      Try(WithinArrayNormalizationMethod <- "printtiploess")
      Try(assign("WithinArrayNormalizationMethod",WithinArrayNormalizationMethod,limmaGUIenvironment))
    })
    Try(WithinArrayNormalizationMethod <- get("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
    Try(tkinsert(mainTree,"end","WithinOnly","WithinOnly.Status" ,text=paste("Available (using ",WithinArrayNormalizationMethod,")",sep=""),font=limmaGUIfontTree))
  }
  else
    Try(tkinsert(mainTree,"end","WithinOnly","WithinOnly.Status" ,text="Not Available",font=limmaGUIfontTree))  )
  Try(if (MA.Available$BetweenArrays)  
    Try(tkinsert(mainTree,"end","BetweenOnly","BetweenOnly.Status" ,text="Available",font=limmaGUIfontTree))
  else
    Try(tkinsert(mainTree,"end","BetweenOnly","BetweenOnly.Status" ,text="Not Available",font=limmaGUIfontTree))  )
  Try(if (MA.Available$Both)  
    Try(tkinsert(mainTree,"end","WithinAndBetween","WithinAndBetween.Status" ,text="Available",font=limmaGUIfontTree))
  else
    Try(tkinsert(mainTree,"end","WithinAndBetween","WithinAndBetween.Status" ,text="Not Available",font=limmaGUIfontTree))  )
  Try(if (Layout.Available)  
    Try(tkinsert(mainTree,"end","Layout","Layout.Status" ,text="Available",font=limmaGUIfontTree))
  else
    Try(tkinsert(mainTree,"end","Layout","Layout.Status" ,text="Not Available",font=limmaGUIfontTree))  )
  Try(if (NumParameterizations>0)
  {
    for (i in (1:NumParameterizations))
    {
      Try(ParameterizationsStatusNameNode <- paste("Parameterizations.Status.",i,sep=""))
      Try(tkinsert(mainTree,"end","Parameterizations",ParameterizationsStatusNameNode ,text=ParameterizationNamesVec[i],font=limmaGUIfontTree))
    }
  }
  else
    Try(tkinsert(mainTree,"end","Parameterizations","Parameterizations.Status.1" ,text="None",font=limmaGUIfontTree)))

  Try(tkconfigure(ttMain,cursor="arrow"))
  Try(tkfocus(ttMain))

}

SaveLimmaFile <- function()
{
  LimmaFileName <- get("LimmaFileName",envir=limmaGUIenvironment)
  if (LimmaFileName=="Untitled")
  {
    SaveAsLimmaFile()
    try(tkfocus(ttMain),silent=TRUE)
    return()
  }
  # Don't give an error if main window has been destroyed.
  try(tkconfigure(ttMain,cursor="watch"),silent=TRUE)  
#  tkmessageBox(message="About to save Limma File! (0)")  
  Try(save(list = ls(envir=limmaGUIenvironment), file=LimmaFileName, envir=limmaGUIenvironment))  
#  tkmessageBox(message="Limma File Saved! (1)")  
  try(tkconfigure(ttMain,cursor="arrow"),silent=TRUE)
  try(tkfocus(ttMain),silent=TRUE)
#  tkmessageBox(message="Limma File Saved! (2)")    
}    

SaveAsLimmaFile <- function()
{
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
  Try(LimmaFileName <- get("LimmaFileName",envir=limmaGUIenvironment))
  Try(if (LimmaFileName=="Untitled" && limmaDataSetNameText!="Untitled") LimmaFileName <- limmaDataSetNameText) # Local assignment only
  Try(tempLimmaFileName <- tclvalue(tkgetSaveFile(initialfile=LimmaFileName,filetypes="{{Limma Files} {.lma}} {{All files} *}")))
  if (!nchar(tempLimmaFileName)) 
  {
    try(tkfocus(ttMain),silent=TRUE)
    return()
  }
  len <- nchar(tempLimmaFileName)
  if (len<=4)
      tempLimmaFileName <- paste(tempLimmaFileName,".lma",sep="")
  else if (substring(tempLimmaFileName,len-3,len)!=".lma")
        tempLimmaFileName <- paste(tempLimmaFileName,".lma",sep="")
  Try(LimmaFileName <- tempLimmaFileName)
  Try(assign("LimmaFileName",LimmaFileName,limmaGUIenvironment))
  # ttMain may have been destroyed
  e <- try(tkfocus(ttMain),silent=TRUE)
  if (!inherits(e, "try-error"))
    Try(tkwm.title(ttMain,paste("LimmaGUI -",LimmaFileName)))
  try(tkconfigure(ttMain,cursor="watch"),silent=TRUE)    
  Try(save(list = ls(envir=limmaGUIenvironment), file=LimmaFileName, envir=limmaGUIenvironment))    
  try(tkconfigure(ttMain,cursor="arrow"),silent=TRUE)
  try(tkfocus(ttMain),silent=TRUE)
}

AboutLimmaGUI <- function()
{
    Try(tkmessageBox(title="About limmaGUI",message=paste("This is limmaGUI Version ",getPackageVersion("limmaGUI"),
                              ", using limma Version ",getPackageVersion("limma"),".  The limma package was developed by Gordon Smyth and the Graphical User Interface (GUI) was developed by James Wettenhall.",sep=""),type="ok",icon="info"))
}
