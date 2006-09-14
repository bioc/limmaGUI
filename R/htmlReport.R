






GetComponentsToExportInHTMLreport <- function(parameterizationIndex=NULL)
{

  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(LinearModelComputed  <- get("LinearModelComputed", envir=limmaGUIenvironment))
  Try(ndups   <- get("ndups",envir=limmaGUIenvironment))

  Try(ttHTMLreportDialog<-tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.deiconify(ttHTMLreportDialog))
  Try(tkgrab.set(ttHTMLreportDialog))
  Try(tkfocus(ttHTMLreportDialog))
  Try(tkwm.title(ttHTMLreportDialog,"HTML Report"))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    ")))


  Try(TargetsTcl           <- tclVar("1"))
  Try(SpotTypesTcl         <- tclVar("1"))
  Try(LayoutTcl            <- tclVar("1"))
  Try(BackgroundCorrectionTcl <- tclVar("1"))
  Try(SpotWeightingTcl     <- tclVar("1"))
  Try(RawMATcl             <- tclVar("1"))
  Try(RawPrintTipGroupTcl  <- tclVar("1"))
  Try(ScaleBoxPlotTcl      <- tclVar("1"))
  Try(if (NumParameterizations>0)
    Try(SpotTypesInLinearModelTcl <- tclVar("1"))
  else
    Try(SpotTypesInLinearModelTcl <- tclVar("0")))
  Try(if (NumParameterizations>0)
    Try(NormalizationInLinearModelTcl <- tclVar("1"))
  else
    Try(NormalizationInLinearModelTcl <- tclVar("0")))
  Try(if (NumParameterizations>0)
    Try(DesignMatrixTcl      <- tclVar("1"))
  else
    Try(DesignMatrixTcl      <- tclVar("0")))
  Try(if (NumParameterizations>0 && ndups>1)
    Try(DupCorTcl            <- tclVar("1"))
  else
    Try(DupCorTcl            <- tclVar("0")))
  Try(if (NumParameterizations>0)
    Try(Top50ToptablesTcl    <- tclVar("1"))
  else
    Try(Top50ToptablesTcl    <- tclVar("0")))
  Try(CompleteToptablesTcl <- tclVar("0"))
  Try(if (NumParameterizations>0)
    Try(AvgMAPlotTcl <- tclVar("1"))
  else
    Try(AvgMAPlotTcl    <- tclVar("0")))
  Try(tStatisticBoxPlotsTcl <- tclVar("0"))

  Try(TargetsCheckbox                     <- tkcheckbutton(ttHTMLreportDialog,variable=TargetsTcl))
  Try(SpotTypesCheckbox                   <- tkcheckbutton(ttHTMLreportDialog,variable=SpotTypesTcl))
  Try(LayoutCheckbox                      <- tkcheckbutton(ttHTMLreportDialog,variable=LayoutTcl))
  Try(BackgroundCorrectionCheckbox        <- tkcheckbutton(ttHTMLreportDialog,variable=BackgroundCorrectionTcl))
  Try(SpotWeightingCheckbox               <- tkcheckbutton(ttHTMLreportDialog,variable=SpotWeightingTcl))
  Try(RawMACheckbox                       <- tkcheckbutton(ttHTMLreportDialog,variable=RawMATcl))
  Try(RawPrintTipGroupCheckbox            <- tkcheckbutton(ttHTMLreportDialog,variable=RawPrintTipGroupTcl))
  Try(ScaleBoxPlotCheckbox                <- tkcheckbutton(ttHTMLreportDialog,variable=ScaleBoxPlotTcl))
  Try(SpotTypesInLinearModelCheckbox      <- tkcheckbutton(ttHTMLreportDialog,variable=SpotTypesInLinearModelTcl))
  Try(NormalizationInLinearModelCheckbox  <- tkcheckbutton(ttHTMLreportDialog,variable=NormalizationInLinearModelTcl))
  Try(DesignMatrixCheckbox                <- tkcheckbutton(ttHTMLreportDialog,variable=DesignMatrixTcl))
  Try(DupCorCheckbox                      <- tkcheckbutton(ttHTMLreportDialog,variable=DupCorTcl))
  Try(Top50ToptablesCheckbox              <- tkcheckbutton(ttHTMLreportDialog,variable=Top50ToptablesTcl))
  Try(CompleteToptablesCheckbox           <- tkcheckbutton(ttHTMLreportDialog,variable=CompleteToptablesTcl))
  Try(AvgMAPlotCheckbox                   <- tkcheckbutton(ttHTMLreportDialog,variable=AvgMAPlotTcl))
  Try(tStatisticBoxPlotsCheckbox          <- tkcheckbutton(ttHTMLreportDialog,variable=tStatisticBoxPlotsTcl))

  Try(lbl2 <- tklabel(ttHTMLreportDialog,text="Components to be Included in the HTML Report",font=.limmaGUIglobals$limmaGUIfont2))
  tkgrid(tklabel(ttHTMLreportDialog,text="    "),lbl2)
  Try(tkgrid.configure(lbl2,columnspan=3,sticky="w"))
  tkgrid(tklabel(ttHTMLreportDialog,text="    "))

  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="RNA Targets",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),TargetsCheckbox,currentLabel))
  Try(tkgrid.configure(TargetsCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Spot Types",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),SpotTypesCheckbox,currentLabel))
  Try(tkgrid.configure(SpotTypesCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Layout",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),LayoutCheckbox,currentLabel))
  Try(tkgrid.configure(LayoutCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Background Correction Method",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),BackgroundCorrectionCheckbox,currentLabel))
  Try(tkgrid.configure(BackgroundCorrectionCheckbox,sticky="e"));Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Spot Quality Weighting",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),SpotWeightingCheckbox,currentLabel))
  Try(tkgrid.configure(SpotWeightingCheckbox,sticky="e"));Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Color-Coded Raw M A Plots",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),RawMACheckbox,currentLabel))
  Try(tkgrid.configure(RawMACheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Print-Tip Group Loess Raw M A Plots",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),RawPrintTipGroupCheckbox,currentLabel))
  Try(tkgrid.configure(RawPrintTipGroupCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Within-Array Normalized M Box Plots",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),ScaleBoxPlotCheckbox,currentLabel))
  Try(tkgrid.configure(ScaleBoxPlotCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Spot Types Included In Linear Model",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),SpotTypesInLinearModelCheckbox,currentLabel))
  Try(tkgrid.configure(SpotTypesInLinearModelCheckbox,sticky="e"));Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Normalization Used In Linear Model",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),NormalizationInLinearModelCheckbox,currentLabel))
  Try(tkgrid.configure(NormalizationInLinearModelCheckbox,sticky="e"));Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Design Matrix (Parameterization)",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),DesignMatrixCheckbox,currentLabel))
  Try(tkgrid.configure(DesignMatrixCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Duplicate Correlation",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),DupCorCheckbox,currentLabel))
  Try(tkgrid.configure(DupCorCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Top 50 DE Genes",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),Top50ToptablesCheckbox,currentLabel))
  Try(tkgrid.configure(Top50ToptablesCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="Complete Lists of DE-Ranked Genes",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),CompleteToptablesCheckbox,currentLabel))
  Try(tkgrid.configure(CompleteToptablesCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="M A Plots (with fitted M values)",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),AvgMAPlotCheckbox,currentLabel))
  Try(tkgrid.configure(AvgMAPlotCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))
  Try(currentLabel <- tklabel(ttHTMLreportDialog,text="t Statistic Box Plots",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttHTMLreportDialog,text="    "),tStatisticBoxPlotsCheckbox,currentLabel))
  Try(tkgrid.configure(tStatisticBoxPlotsCheckbox,sticky="e"));  Try(tkgrid.configure(currentLabel,sticky="w",columnspan=2))

# Need to test whether arrays have been loaded and whether a linear model has been fit.

  if (NumParameterizations==0 || LinearModelComputed[parameterizationIndex]==FALSE)
  {
    Try(tkconfigure(SpotTypesInLinearModelCheckbox,state="disabled"))
    Try(tkconfigure(NormalizationInLinearModelCheckbox,state="disabled"))
    Try(tkconfigure(DesignMatrixCheckbox,state="disabled"))
    Try(tkconfigure(DupCorCheckbox,state="disabled"))
    Try(tkconfigure(Top50ToptablesCheckbox,state="disabled"))
    Try(tkconfigure(CompleteToptablesCheckbox,state="disabled"))
    Try(tkconfigure(tStatisticBoxPlotsCheckbox,state="disabled"))
    Try(tkconfigure(AvgMAPlotCheckbox,state="disabled"))
  }

  if (ndups<=1)
    Try(tkconfigure(DupCorCheckbox,state="disabled"))

  tkgrid(tklabel(ttHTMLreportDialog,text="    "))
  tkgrid(tklabel(ttHTMLreportDialog,text="    "))
  ReturnVal <- list()
  onOK <- function()
  {
      if (tclvalue(TargetsTcl)=="1") ReturnVal[[1]] <- TRUE else ReturnVal[[1]] <- FALSE; attributes(ReturnVal)$names[1] <- "Targets"
      if (tclvalue(SpotTypesTcl)=="1") ReturnVal[[2]] <- TRUE else ReturnVal[[2]] <- FALSE; attributes(ReturnVal)$names[2] <- "SpotTypes"
      if (tclvalue(LayoutTcl)=="1") ReturnVal[[3]] <- TRUE else ReturnVal[[3]] <- FALSE; attributes(ReturnVal)$names[3] <- "Layout"
      if (tclvalue(BackgroundCorrectionTcl)=="1") ReturnVal[[4]] <- TRUE else ReturnVal[[4]] <- FALSE; attributes(ReturnVal)$names[4] <- "BackgroundCorrection"
      if (tclvalue(SpotWeightingTcl)=="1") ReturnVal[[5]] <- TRUE else ReturnVal[[5]] <- FALSE; attributes(ReturnVal)$names[5] <- "SpotWeighting"
      if (tclvalue(RawMATcl)=="1") ReturnVal[[6]] <- TRUE else ReturnVal[[6]] <- FALSE; attributes(ReturnVal)$names[6] <- "RawMA"
      if (tclvalue(RawPrintTipGroupTcl)=="1") ReturnVal[[7]] <- TRUE else ReturnVal[[7]] <- FALSE; attributes(ReturnVal)$names[7] <- "RawPrintTipGroup"
      if (tclvalue(ScaleBoxPlotTcl)=="1") ReturnVal[[8]] <- TRUE else ReturnVal[[8]] <- FALSE; attributes(ReturnVal)$names[8] <- "ScaleBoxPlot"
      if (tclvalue(SpotTypesInLinearModelTcl)=="1") ReturnVal[[9]] <- TRUE else ReturnVal[[9]] <- FALSE; attributes(ReturnVal)$names[9] <- "SpotTypesInLinearModel"
      if (tclvalue(NormalizationInLinearModelTcl)=="1") ReturnVal[[10]] <- TRUE else ReturnVal[[10]] <- FALSE; attributes(ReturnVal)$names[10] <- "NormalizationInLinearModel"
      if (tclvalue(DesignMatrixTcl)=="1") ReturnVal[[11]] <- TRUE else ReturnVal[[11]] <- FALSE; attributes(ReturnVal)$names[11] <- "DesignMatrix"
      if (tclvalue(DupCorTcl)=="1") ReturnVal[[12]] <- TRUE else ReturnVal[[12]] <- FALSE; attributes(ReturnVal)$names[12] <- "DupCor"
      if (tclvalue(Top50ToptablesTcl)=="1") ReturnVal[[13]] <- TRUE else ReturnVal[[13]] <- FALSE; attributes(ReturnVal)$names[13] <- "Top50Toptables"
      if (tclvalue(CompleteToptablesTcl)=="1") ReturnVal[[14]] <- TRUE else ReturnVal[[14]] <- FALSE; attributes(ReturnVal)$names[14] <- "CompleteToptables"
      if (tclvalue(AvgMAPlotTcl)=="1") ReturnVal[[15]] <- TRUE else ReturnVal[[15]] <- FALSE; attributes(ReturnVal)$names[15] <- "AvgMAPlot"
      if (tclvalue(tStatisticBoxPlotsTcl)=="1") ReturnVal[[16]] <- TRUE else ReturnVal[[16]] <- FALSE; attributes(ReturnVal)$names[16] <- "tStatisticBoxPlots"

      Try(tkgrab.release(ttHTMLreportDialog));Try(tkdestroy(ttHTMLreportDialog));Try(tkfocus(.limmaGUIglobals$ttMain))
      ReturnVal <<- ReturnVal
  }
  onCancel <- function() {Try(tkgrab.release(ttHTMLreportDialog));Try(tkdestroy(ttHTMLreportDialog));Try(tkfocus(.limmaGUIglobals$ttMain)); ReturnVal <<- list()}
  OK.but <-tkbutton(ttHTMLreportDialog,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2)
  Cancel.but <-tkbutton(ttHTMLreportDialog,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2)
  tkgrid(tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "),OK.but,Cancel.but,tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "))
  tkgrid.configure(OK.but,    sticky="e")
  tkgrid.configure(Cancel.but,sticky="w")
  tkgrid(tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "),
       tklabel(ttHTMLreportDialog,text="    "),tklabel(ttHTMLreportDialog,text="    "))
  Try(tkfocus(ttHTMLreportDialog))
  Try(tkbind(ttHTMLreportDialog, "<Destroy>", function() {Try(tkgrab.release(ttHTMLreportDialog));Try(tkfocus(.limmaGUIglobals$ttMain));}))
  Try(tkwait.window(ttHTMLreportDialog))

  return (ReturnVal)
}

###########################################################################################################################
# R2HTML Plot Function (Modified to accept a plotFunction argument, rather than using the main R Graphics Device)

"HTMLplotUsingFunction" <- function (Caption = "", File = .HTML.file, GraphRelativeDirectory = ".", GraphAbsoluteDirectory = NULL, GraphFileName = "", GraphSaveAs = "png", GraphBorder = 1,  Align = "center", plotFunction = NULL,Width=600,Height=600,PointSize=12,BG="white",res=72,...)
{
    if (is.null(GraphAbsoluteDirectory))
      GraphAbsoluteDirectory <- getwd()
    if (GraphFileName == "") {
        nowd <- date()
        GraphFileName <- paste("GRAPH_", substring(nowd, 5, 7), substring(nowd, 9, 10), "_", substring(nowd, 12, 13), substring(nowd, 15,  16), substring(nowd, 18, 19), sep = "")
    }
    GraphFileName <- paste(GraphFileName, ".", GraphSaveAs, sep = "")

#    AbsGraphFileName <- paste(GraphRelativeDirectory,.Platform$file.sep,GraphFileName,sep="")
    AbsGraphFileName <- file.path(GraphAbsoluteDirectory, GraphFileName)
    if (GraphSaveAs=="png")
    {
      if (is.null(plotFunction))
        dev.print(png, file = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)
      else
      {
        Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
          Try(bitmap(file = AbsGraphFileName,bg=BG,res=res))
        else
          Try(png(filename = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)))
        plotFunction()
        dev.off()
      }
    }
    else if (GraphSaveAs=="jpg")
    {
      if (is.null(plotFunction))
        dev.print(jpeg, file = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)
      else
      {
        Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
          Try(bitmap(filename = AbsGraphFileName,bg=BG,res=res,type="jpeg"))
        else
          Try(jpeg(filename = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)))
        plotFunction()
        dev.off()
      }
    }
    else if (GraphSaveAs=="gif")
    {
      if (is.null(plotFunction))
        dev.print(gif, file = AbsGraphFileName, width=Width,height=Height,pointsize=PointSize,bg=BG)
      else
      {
        stop("When passing a plot function to HTMLplot, device must be jpg or png.")
      }
    }
    else stop("GraphSaveAs must be either jpg, png or gif")
    cat(paste("<p align=", Align, "><img src='", paste(GraphRelativeDirectory,"/",GraphFileName,sep=""), "' border=", GraphBorder, ">", sep = "", collapse = ""), file = File, append = TRUE, sep = "")
    if (Caption != "") {
        cat(paste("<br><i>", Caption, "</i>"), file = File, append = TRUE, sep = "")
    }
    cat("</P>", file = File, append = TRUE, sep = "\n")
    try(assign(".HTML.graph", TRUE, env = get("HTMLenv", envir = .GlobalEnv)))
    invisible(return())
}

###########################################################################################################################

ExportHTMLreport <- function()
{
# We will use the R2HTML package, but with my own HTMLplot function.
# Will we need xtable or does R2HTML have its own HTMLtable function?
  Require("xtable")
  Require("R2HTML")

  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(RG <- get("RG",envir=limmaGUIenvironment))

  if (ArraysLoaded==FALSE)
  {
      Try(tkmessageBox(title="Export HTML Report",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  if (NumParameterizations>0)
  {
    Try(parameterizationIndex <- ChooseParameterization())
    Try(if (parameterizationIndex==0)    return()    )
    Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
    Try(ComponentsToExport <- GetComponentsToExportInHTMLreport(parameterizationIndex))
 }
 else
     Try(ComponentsToExport <- GetComponentsToExportInHTMLreport())

  if (length(ComponentsToExport)==0) return()

  Try(fileNameWithPath<- tkgetSaveFile(initialfile=limmaDataSetNameText,filetypes="{{HTML Files} {.html .htm}} {{All files} *}"))
  Try(if (nchar(tclvalue(fileNameWithPath))==0)
    return())
  Try(path     <- tclvalue(tkfile.dir (tclvalue(fileNameWithPath))))
  Try(fileName <- tclvalue(tkfile.tail(tclvalue(fileNameWithPath))))
  ###Try(path     <- tclvalue(tcl( (tclvalue(fileNameWithPath)),"dir" )))###not working yet - no timee to fix now - do it later
  ###Try(fileName <- tclvalue(tcl( (tclvalue(fileNameWithPath)),"tail")))###not working yet - no timee to fix now - do it later

  Try(len <- nchar(fileName))
  if (len<4)
      Try(fileName <- paste(fileName,".html",sep=""))
  else if   ((tolower(substring(fileName,len-4,len))!=".html") &&
  (len<5 ||  (tolower(substring(fileName,len-4,len))!=".html")))
          Try(fileName <- paste(fileName,".html",sep=""))

  Try(fileNameWithoutExtension <- substring(fileName,1,nchar(fileName)-5))

  Try(HTMLfilePath <- paste(path,.Platform$file.sep,fileNameWithoutExtension,"_files",sep=""))
  Try(HTMLfileRelativePath <- paste(fileNameWithoutExtension,"_files",sep=""))
  Try(dir.create(HTMLfilePath))

  Try(fileNameWithPath <- paste(path,"/",fileName,sep=""))

  Try(R2HTMLpath <- system.file(package="R2HTML","output"))
  Try(cssFileSource <- paste(R2HTMLpath,"/","R2HTML.css",sep=""))
  Try(cssFileDestination <- paste(path,"/","R2HTML.css",sep=""))
  Try(R2HTMLlogoSource <- paste(R2HTMLpath,"/","R2HTMLlogo.gif",sep=""))
  Try(R2HTMLlogoDestination <- paste(path,"/","R2HTMLlogo.gif",sep=""))
  Try(file.copy(cssFileSource,cssFileDestination,overwrite=TRUE))
  Try(file.copy(R2HTMLlogoSource,R2HTMLlogoDestination,overwrite=TRUE))

  Try(HTMLtarget <- HTMLInitFile(path,filename=fileNameWithoutExtension,Title=paste(limmaDataSetNameText,"- Statistical Microarray Analysis using LimmaGUI"), HTMLframe=FALSE,BackGroundColor="#FFFFFF"))

  Try(HTML.title(paste(limmaDataSetNameText,"- Statistical Microarray Analysis using LimmaGUI"),HR=1))

  Try(ExportTargets                    <- ComponentsToExport$Targets)
  Try(ExportSpotTypes                  <- ComponentsToExport$SpotTypes)
  Try(ExportLayout                     <- ComponentsToExport$Layout)
  Try(ExportBackgroundCorrection       <- ComponentsToExport$BackgroundCorrection)
  Try(ExportSpotWeighting              <- ComponentsToExport$SpotWeighting)
  Try(ExportRawMA                      <- ComponentsToExport$RawMA)
  Try(ExportRawPrintTipGroup           <- ComponentsToExport$RawPrintTipGroup)
  Try(ExportScaleBoxPlot               <- ComponentsToExport$ScaleBoxPlot)
  Try(ExportSpotTypesInLinearModel     <- ComponentsToExport$SpotTypesInLinearModel)
  Try(ExportNormalizationInLinearModel <- ComponentsToExport$NormalizationInLinearModel)
  Try(ExportDesignMatrix               <- ComponentsToExport$DesignMatrix)
  Try(ExportDupCor                     <- ComponentsToExport$DupCor)
  Try(ExportTop50Toptables             <- ComponentsToExport$Top50Toptables)
  Try(ExportCompleteToptables          <- ComponentsToExport$CompleteToptables)
  Try(ExportAvgMAPlot                  <- ComponentsToExport$AvgMAPlot)
  Try(ExporttStatisticBoxPlots         <- ComponentsToExport$tStatisticBoxPlots)

  if (ExportRawMA || ExportRawPrintTipGroup || ExportScaleBoxPlot || ExportAvgMAPlot || ExporttStatisticBoxPlots)
  {
    Try(if (capabilities("png")==FALSE)
        Try(tkmessageBox(title="PNG unavailable",message="Your R installation is unable to save PNG images of plots.",icon="warning"))
    else
      Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
      {
        Try(pngParams <- GetJpegOrPngX11Params(graphFileType="PNG"))
        Try(if (length(pngParams)==0) return())
        Try(plotBG        <- pngParams$bg)
        Try(plotRes       <- pngParams$res)
      }
      else
      {
        Try(pngParams <- GetJpegOrPngParams(graphFileType="PNG"))
        Try(if (length(pngParams)==0) return())
        Try(plotWidth     <- pngParams$width)
        Try(plotHeight    <- pngParams$height)
        Try(plotPointSize <- pngParams$pointsize)
        Try(plotBG        <- pngParams$bg)
      }))
  }

  Try(HTML.title("Contents",HR=2))
  if (ExportTargets) Try(HTMLli(txt="<a href=\"#Targets\"><b>Targets</b></a>"))
  if (ExportSpotTypes) Try(HTMLli(txt="<a href=\"#SpotTypes\"><b>Spot Types</b></a>"))
  if (ExportLayout) Try(HTMLli(txt="<a href=\"#Layout\"><b>Layout</b></a>"))
  if (ExportBackgroundCorrection) Try(HTMLli(txt="<a href=\"#BackgroundCorrection\"><b>Background Correction</b></a>"))
  if (ExportSpotWeighting) Try(HTMLli(txt="<a href=\"#SpotWeighting\"><b>Spot Quality Weighting</b></a>"))
  if (ExportRawMA) Try(HTMLli(txt="<a href=\"#RawMA\"><b>Raw M A Plots</b></a>"))
  if (ExportRawPrintTipGroup) Try(HTMLli(txt="<a href=\"#RawPrintTipGroup\"><b>Raw Print-Tip Group Loess M A Plots</b></a>"))
  if (ExportScaleBoxPlot) Try(HTMLli(txt="<a href=\"#ScaleBoxPlot\"><b>M Box Plot for each Slide</b></a>"))
  if (ExportSpotTypesInLinearModel) Try(HTMLli(txt="<a href=\"#SpotTypesInLinearModel\"><b>Spot Types Included In Linear Model</b></a>"))
  if (ExportNormalizationInLinearModel) Try(HTMLli(txt="<a href=\"#NormalizationInLinearModel\"><b>Normalization Used In Linear Model</b></a>"))
  if (ExportDesignMatrix) Try(HTMLli(txt="<a href=\"#DesignMatrix\"><b>Design Matrix</b></a>"))
  if (ExportDupCor) Try(HTMLli(txt="<a href=\"#DupCor\"><b>Duplicate Correlation</b></a>"))
  if (ExportTop50Toptables) Try(HTMLli(txt="<a href=\"#Top50Toptables\"><b>Tables of Top 50 Differentially Expressed Genes</b></a>"))
  if (ExportCompleteToptables) Try(HTMLli(txt="<a href=\"#CompleteToptables\"><b>Complete Tables of Genes Ranked in order of Evidence for Differential Expression</b></a>"))
  if (ExportAvgMAPlot) Try(HTMLli(txt="<a href=\"#AvgMAPlot\"><b>M A Plots (with fitted M values)</b></a>"))
  if (ExporttStatisticBoxPlots) Try(HTMLli(txt="<a href=\"#tStatisticBoxPlots\"><b>Box Plots showing the Range of t Statistics for each Spot Type</b></a>"))

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

  if (ExportTargets)
  {
    Try(Targets <- get("Targets",envir=limmaGUIenvironment))
    Try(ncolTargets <- ncol(Targets))
    Try(colnamesTargets <- colnames(Targets))
    Try(displayVector <- rep("s",ncolTargets+1))
    Try(for (i in (0:ncolTargets))
      Try(if (i==0 || colnamesTargets[i]=="SlideNumber")
        Try(displayVector[i] <- "d")))
    Try(TargetsXtable <- xtable(Targets,display=displayVector))
    Try(HTML.title("<a name=\"Targets\">RNA Targets</a>",HR=2))
    Try(print(TargetsXtable,type="html",file=fileNameWithPath,append=TRUE))
  }
  if (ExportSpotTypes)
  {
    Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
    Try(SpotTypesXtable <- xtable(SpotTypes))
    Try(HTML.title("<a name=\"SpotTypes\">Spot Types</a>",HR=2))
    Try(print(SpotTypesXtable,type="html",file=fileNameWithPath,append=TRUE))
  }
  if (ExportLayout)
  {
    Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
    Try(layoutDataFrame <- data.frame(NumBlockRows=maLayout$ngrid.r,NumBlockColumns=maLayout$ngrid.c,
                                  NumRowsPerBlock=maLayout$nspot.r,NumColsPerBlock=maLayout$nspot.c))
    Try(layoutXtable <- xtable(layoutDataFrame,display=c("d","d","d","d","d")))
    Try(HTML.title("<a name=\"Layout\">Layout</a>",HR=2))
    Try(print(layoutXtable,type="html",file=fileNameWithPath,append=TRUE))
  }
  if (ExportBackgroundCorrection)
  {
  	Try(if (!exists("BCMethod",envir=limmaGUIenvironment))
		{
			Try(BCMethod <- "normexp")
			Try(assign("BCMethod",BCMethod,limmaGUIenvironment))
		})
		Try(BCMethod <- get("BCMethod",envir=limmaGUIenvironment))
    Try(HTML.title("<a name=\"BackgroundCorrection\">Background Correction Method</a>",HR=2))
    Try(HTMLli(txt=paste("<b>Background Correction Method : </b> ",BCMethod)))
  }
  if (ExportSpotWeighting)
  {
    Try(WeightingType     <- get("WeightingType",envir=limmaGUIenvironment))
    Try(HTML.title("<a name=\"SpotWeighting\">Spot Quality Weighting</a>",HR=2))
    Try(HTMLli(txt=paste("<b>Weighting Type : </b> ",WeightingType)))
  }
  if (ExportRawMA && capabilities("png"))
  {
    Try(HTML.title("<a name=\"RawMA\">M A Plots Using Raw (Unnormalized) Data</a>",HR=2))
    Try(NumSlides      <- get("NumSlides",envir=limmaGUIenvironment))
    Try(SlideNamesVec  <- get("SlideNamesVec",envir=limmaGUIenvironment))
    Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
    Try(SpotTypeStatus <- get("SpotTypeStatus",envir=limmaGUIenvironment))
    Try(MAraw <- get("MAraw",envir=limmaGUIenvironment))

    Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
    if (MA.Available$Raw)
      Try(MAraw <- get("MAraw",envir=limmaGUIenvironment))
    else
    {
      Try (MAraw <- MA.RG(RG))
      Try(assign("MAraw",MAraw,limmaGUIenvironment))
      Try(MA.Available$Raw <- TRUE)
      Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
      Try(tkdelete(.limmaGUIglobals$mainTree,"Raw.Status"))
      Try(tkinsert(.limmaGUIglobals$mainTree,"end","Raw","Raw.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
    }

    Try(PointTypes    <- SpotTypes$SpotType)
    Try(PointColors   <- SpotTypes$Color)
    Try(numPointTypes <- length(PointTypes))
    Try(cex           <- rep(0.6,numPointTypes))
    Try(for (i in (1:numPointTypes))
      if (tolower(PointTypes[i])=="gene"||PointTypes[i]=="cDNA")
      {
        Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
          cex[i] <- 0.3
        else
          cex[i] <- 0.1)
      })
    Try(cex    <- cex)
    Try(values <- PointTypes)
    Try(colVec <- PointColors)


    Try(for (slidenum in (1:NumSlides))
    {
      Try(plotTitle <- paste("M A Scatter Plot for ",SlideNamesVec[slidenum],sep=""))
      Try(plotFunction <- function() plotMA(MAraw,pch=16,cex=cex,array=slidenum,
          status=SpotTypeStatus,values=values,col=colVec,main=plotTitle,xlab="A",ylab="M"))
      Try(HTMLplotUsingFunction(Caption = plotTitle, File=fileNameWithPath, GraphRelativeDirectory = HTMLfileRelativePath ,
        GraphAbsoluteDirectory = HTMLfilePath, GraphFileName = paste("plotMArawSlide",slidenum,sep=""),
        GraphSaveAs = "png", GraphBorder = 1,  Align = "left", plotFunction=plotFunction,
        Width=plotWidth,Height=plotHeight,PointSize=plotPointSize,BG=plotBG,res=plotRes))
    })
  }

  if (ExportRawPrintTipGroup && capabilities("png"))
  {
    Try(HTML.title("<a name=\"RawPrintTipGroup\">Print-Tip Group M A Plots (With Loess Curves) Using Raw (Unnormalized Data)</a>",HR=2))
    Try(maLayout       <- get("maLayout",     envir=limmaGUIenvironment))
    Try(NumSlides      <- get("NumSlides",    envir=limmaGUIenvironment))
    Try(SlideNamesVec  <- get("SlideNamesVec",envir=limmaGUIenvironment))
    Try(MAraw          <- get("MAraw",        envir=limmaGUIenvironment))

    Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
    if (MA.Available$Raw)
      Try(MAraw <- get("MAraw",envir=limmaGUIenvironment))
    else
    {
      Try (MAraw <- MA.RG(RG))
      Try(assign("MAraw",MAraw,limmaGUIenvironment))
      Try(MA.Available$Raw <- TRUE)
      Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
      Try(tkdelete(.limmaGUIglobals$mainTree,"Raw.Status"))
      Try(tkinsert(.limmaGUIglobals$mainTree,"end","Raw","Raw.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
    }

    Try(for (slidenum in (1:NumSlides))
    {
      Try(plotTitle <- paste("Print-Tip Group Loess M A Plots for ",SlideNamesVec[slidenum],sep=""))
      Try(plotFunction <- function() plotPrintTipLoess(MAraw,layout=maLayout,array=slidenum))
      Try(HTMLplotUsingFunction(Caption = plotTitle, File=fileNameWithPath, GraphRelativeDirectory = HTMLfileRelativePath ,
        GraphAbsoluteDirectory = HTMLfilePath, GraphFileName = paste("PrintTipLoessPlotSlide",slidenum,sep=""),
        GraphSaveAs = "png", GraphBorder = 1,  Align = "left", plotFunction=plotFunction,
        Width=plotWidth,Height=plotHeight,PointSize=plotPointSize,BG=plotBG,res=plotRes))
    })
  }

  if (ExportScaleBoxPlot && capabilities("png"))
  {
    Try(HTML.title("<a name=\"ScaleBoxPlot\">M Box Plot Showing the Range of M Values for Each Slide</a>",HR=2))
    Try(Require("sma"))
    Try(RG       <- get("RG",envir=limmaGUIenvironment))
    Try(maLayout <- get("maLayout",     envir=limmaGUIenvironment))
    Try(SlideNamesVec  <- get("SlideNamesVec",envir=limmaGUIenvironment))
    Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
    if (MA.Available$WithinArrays)
      Try(MA <- get("MAwithinArrays",envir=limmaGUIenvironment))
    else
    {
      Try(if (!exists("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
      {
        Try(WithinArrayNormalizationMethod <- "printtiploess")
        Try(assign("WithinArrayNormalizationMethod",WithinArrayNormalizationMethod,limmaGUIenvironment))
      })
      Try(WithinArrayNormalizationMethod <- get("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
      if (WeightingType == "none")
        Try (MA <- normalizeWithinArrays(RG,maLayout,method=WithinArrayNormalizationMethod))
      else
        Try(MA <- normalizeWithinArrays(RG,weights=RG$weights,maLayout,method=WithinArrayNormalizationMethod))
      Try(assign("MAwithinArrays",MA,limmaGUIenvironment))
      Try(MA.Available$WithinArrays <- TRUE)
      Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
      Try(tkdelete(.limmaGUIglobals$mainTree,"WithinOnly.Status"))
      Try(WithinArrayNormalizationMethod <- get("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
      Try(tkinsert(.limmaGUIglobals$mainTree,"end","WithinOnly","WithinOnly.Status" ,text=paste("Available (using ",WithinArrayNormalizationMethod,")",sep=""),font=.limmaGUIglobals$limmaGUIfontTree))
    }
    Try(plotTitle <- "M Box Plot for all slides with normalization within arrays only")
    Try(plotFunction <- function() plot.scale.box(MA$M,x.names=SlideNamesVec,xlab="Slide",ylab="M",main=plotTitle))
    Try(HTMLplotUsingFunction(Caption = plotTitle, File=fileNameWithPath, GraphRelativeDirectory = HTMLfileRelativePath ,
      GraphAbsoluteDirectory = HTMLfilePath, GraphFileName = "MScaleBoxPlot",
      GraphSaveAs = "png", GraphBorder = 1,  Align = "left", plotFunction=plotFunction,
      Width=plotWidth,Height=plotHeight,PointSize=plotPointSize,BG=plotBG,res=plotRes))
  }
  if (ExportSpotTypesInLinearModel)
  {
    Try(SpotTypesIncludedNamesVec <- GetSpotTypesIncludedNames(parameterizationTreeIndex))
    Try(SpotTypesInLinearModel <- data.frame(SpotTypes=SpotTypesIncludedNamesVec))
    Try(colnames(SpotTypesInLinearModel)[1] <- "Spot Types Included In Linear Model")
    Try(SpotTypesInLinearModelXtable <- xtable(SpotTypesInLinearModel))
    Try(HTML.title("<a name=\"SpotTypesInLinearModel\">Spot Types Included In Linear Model</a>",HR=2))
    Try(print(SpotTypesInLinearModelXtable,type="html",file=fileNameWithPath,append=TRUE))
  }
  if (ExportNormalizationInLinearModel)
  {
    if (NumParameterizations==0) break()
    Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
    Try(WhetherToNormalizeWithinArrays  <- (ParameterizationList[[ParameterizationNameNode]])$WhetherToNormalizeWithinArrays)
    Try(WhetherToNormalizeBetweenArrays <- (ParameterizationList[[ParameterizationNameNode]])$WhetherToNormalizeBetweenArrays)
    Try(HTML.title("<a name=\"NormalizationInLinearModel\">Normalization Used In Linear Model</a>",HR=2))
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
      Try(ParameterizationList[[ParameterizationNameNode]][[WithinArrayNormalizationMethod]] <- WithinArrayNormalizationMethod)
      Try(assign("ParameterizationList",ParameterizationList,limmaGUIenvironment))
    })

    Try(HTMLli(txt=paste("Within Arrays : ", WhetherToNormalizeWithinArrays," (",WithinArrayNormalizationMethod,")",sep="")))
    Try(HTMLli(txt=paste("Between Arrays : ",WhetherToNormalizeBetweenArrays,sep="")))
  }
  if (ExportDesignMatrix)
  {
    if (NumParameterizations==0) break()
    Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
    Try(designList <- (ParameterizationList[[ParameterizationNameNode]])$designList)
    Try(design <- designList$design)
    Try(display <- rep("g",ncol(design)))
    Try(display <- c("d",display))
    Try(designXtable <- xtable(design,display=display))
    Try(HTML.title("<a name=\"DesignMatrix\">Design Matrix</a>",HR=2))
    Try(print(designXtable,type="html",file=fileNameWithPath,append=TRUE))
  }
  if (ExportDupCor)
  {
    Try(ndups <- get("ndups",envir=limmaGUIenvironment))
    Try(if (ndups<=1) break())
    Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
    Try(dupcor <- (ParameterizationList[[ParameterizationNameNode]])$dupcor)
    Try(HTML.title("<a name=\"DupCor\">Duplicate Correlation</a>",HR=2))
    Try(HTMLli(txt=paste("Duplicate Correlation :",dupcor$cor)))
  }

  if (ExportTop50Toptables)
  {
    Try(ndups <- get("ndups",envir=limmaGUIenvironment))
    Try(spacing <- GetReducedDuplicateSpacing(parameterizationTreeIndex))
    Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
    Try(if (NumParameterizations==0) break())
    Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
    Try(designList <- (ParameterizationList[[ParameterizationNameNode]])$designList)
    Try(design <- designList$design)
    Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
    Try(HTML.title(paste("<a name=\"Top50Toptables\">Top 50 Differentially Expressed Genes for each Parameter in Parameterization ",ParameterizationNamesVec[parameterizationIndex],"</a>",sep=""),HR=2))
    Try(NumParameters         <- get("NumParameters",envir=limmaGUIenvironment))
    Try(if (nrow(design)==0)
    {
        Try(ParameterNamesVec <- c())
        if (NumParameters>0)
          for (i in (1:NumParameters))
            Try(ParameterNamesVec <- c(ParameterNamesVec,paste("Param",i)))
    }
    else
        Try(ParameterNamesVec <- colnames(design)))
    Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))

    Try(Amatrix <- NULL)

    Try(fit <- (ParameterizationList[[ParameterizationNameNode]])$fit)
    Try(eb  <- (ParameterizationList[[ParameterizationNameNode]])$eb)
    Try(if ("Amatrix" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
      Amatrix <- (ParameterizationList[[ParameterizationNameNode]])$Amatrix)

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
      Try(A <- unwrapdups(Amatrix,ndups=ndups,spacing=spacing)))

    Try(if ("genelist" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
      Try(genelist <- (ParameterizationList[[ParameterizationNameNode]])$genelist)
    else
      Try(genelist <- get("genelist",limmaGUIenvironment)))

    for (coef in (1:NumParameters))
    {
      Try(options(digits=3))
      Try(table1 <- toptable(coef=coef,number=50,genelist=genelist,A=A,fit=fit,eb=eb))
      Try(toptableDisplay <- rep("s",ncol(table1)+1))
      Try(toptableDisplay[1] <- "d")
      Try(for (i in (2:(ncol(table1)+1)))
      {
        Try(if (tolower(colnames(table1)[i-1])=="block") toptableDisplay[i] <- "d")
        Try(if (tolower(colnames(table1)[i-1])=="column") toptableDisplay[i] <- "d")
        Try(if (tolower(colnames(table1)[i-1])=="col") toptableDisplay[i] <- "d"   )
        Try(if (tolower(colnames(table1)[i-1])=="row") toptableDisplay[i] <- "d"   )
        Try(if (tolower(colnames(table1)[i-1])=="gridrow") toptableDisplay[i] <- "d")
        Try(if (tolower(colnames(table1)[i-1])=="gridcol") toptableDisplay[i] <- "d")
        Try(if (tolower(colnames(table1)[i-1])=="gridcolumn") toptableDisplay[i] <- "d")
        Try(if (colnames(table1)[i-1]=="M")       toptableDisplay[i] <- "f")
        Try(if (colnames(table1)[i-1]=="A")       toptableDisplay[i] <- "f")
        Try(if (colnames(table1)[i-1]=="t")       toptableDisplay[i] <- "f")
        Try(if (colnames(table1)[i-1]=="P.Value") toptableDisplay[i] <- "e")
        Try(if (colnames(table1)[i-1]=="B") toptableDisplay[i] <- "f")
      })
#      Try(colnames(table1)[ncol(table1)-1] <- sprintf("%-10s",colnames(table1)[ncol(table1)-1]))
      Try(toptableXtable <- xtable(table1,display=toptableDisplay))
      Try(HTML.title(paste("Top 50 Differentially Expressed Genes for",ParameterNamesVec[coef]),HR=3))
      Try(print(toptableXtable,type="html",file=fileNameWithPath,append=TRUE))
    }

    # Now the Contrasts
    Try(NumContrastParameterizations <- ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations)
    Try(ContrastsParameterizationNamesVec <- c() )
    Try(contrastNames <- list())
    Try (if (NumContrastParameterizations>0)
      Try(for (cp in (1:NumContrastParameterizations))
      {
          Try(ContrastsParameterizationNamesVec[cp] <- ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]]$contrastsParameterizationNameText)
          Try(contrastsMatrixInList <- ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]]$contrastsMatrixInList)
          Try(contrastsMatrix <- contrastsMatrixInList$contrasts)
          Try(contrastNames[[cp]] <- colnames(contrastsMatrix))
          Try(fit <- (ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]])$fit)
          Try(eb  <- (ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]])$eb)

          for (coef in (1:ncol(contrastsMatrix)))
          {
            Try(options(digits=3))
            Try(table1 <- toptable(coef=coef,number=50,genelist=genelist,A=A,fit=fit,eb=eb))
            Try(toptableDisplay <- rep("s",ncol(table1)+1))
            Try(toptableDisplay[1] <- "d")
            Try(for (i in (2:(ncol(table1)+1)))
            {
              Try(if (tolower(colnames(table1)[i-1])=="block") toptableDisplay[i] <- "d")
              Try(if (tolower(colnames(table1)[i-1])=="column") toptableDisplay[i] <- "d")
              Try(if (tolower(colnames(table1)[i-1])=="col") toptableDisplay[i] <- "d"   )
              Try(if (tolower(colnames(table1)[i-1])=="row") toptableDisplay[i] <- "d"   )
              Try(if (tolower(colnames(table1)[i-1])=="gridrow") toptableDisplay[i] <- "d")
              Try(if (tolower(colnames(table1)[i-1])=="gridcol") toptableDisplay[i] <- "d")
              Try(if (tolower(colnames(table1)[i-1])=="gridcolumn") toptableDisplay[i] <- "d")
              Try(if (colnames(table1)[i-1]=="M")       toptableDisplay[i] <- "f")
              Try(if (colnames(table1)[i-1]=="A")       toptableDisplay[i] <- "f")
              Try(if (colnames(table1)[i-1]=="t")       toptableDisplay[i] <- "f")
              Try(if (colnames(table1)[i-1]=="P.Value") toptableDisplay[i] <- "e")
              Try(if (colnames(table1)[i-1]=="B") toptableDisplay[i] <- "f")
            })
      #      Try(colnames(table1)[ncol(table1)-1] <- sprintf("%-10s",colnames(table1)[ncol(table1)-1]))
            Try(toptableXtable <- xtable(table1,display=toptableDisplay))
            Try(HTML.title(paste("Top 50 Differentially Expressed Genes for ",colnames(contrastsMatrix)[coef]," [",ContrastsParameterizationNamesVec[cp],"]",sep=""),HR=3))
            Try(print(toptableXtable,type="html",file=fileNameWithPath,append=TRUE))
        }
      }))


  }
  if (ExportCompleteToptables)
  {
    Try(ndups <- get("ndups",envir=limmaGUIenvironment))
    Try(spacing <- GetReducedDuplicateSpacing(parameterizationTreeIndex))
    if (NumParameterizations==0) break()
    Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
    Try(designList <- (ParameterizationList[[ParameterizationNameNode]])$designList)
    Try(design <- designList$design)
    Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
    Try(HTML.title(paste("<a name=\"CompleteToptables\">Complete Tables of Genes Ranked in order of Evidence for Differential Expression for each Parameter in Parameterization ",ParameterizationNamesVec[parameterizationIndex],"</a>",sep=""),HR=2))
    Try(NumParameters         <- get("NumParameters",envir=limmaGUIenvironment))
    Try(if (nrow(design)==0)
    {
        Try(ParameterNamesVec <- c())
        if (NumParameters>0)
          for (i in (1:NumParameters))
            Try(ParameterNamesVec <- c(ParameterNamesVec,paste("Param",i)))
    }
    else
        Try(ParameterNamesVec <- colnames(design)))
    Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))

    Try(Amatrix <- NULL)
    Try(if ("Amatrix" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
      Amatrix <- (ParameterizationList[[ParameterizationNameNode]])$Amatrix)

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
      Try(A <- unwrapdups(Amatrix,ndups=ndups,spacing=spacing)))

    Try(fit <- (ParameterizationList[[ParameterizationNameNode]])$fit)
    Try(eb  <- (ParameterizationList[[ParameterizationNameNode]])$eb)
    Try(if ("genelist" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
      Try(genelist <- (ParameterizationList[[ParameterizationNameNode]])$genelist)
    else
      Try(genelist <- get("genelist",limmaGUIenvironment)))

    for (coef in (1:NumParameters))
    {
      Try(options(digits=3))
      Try(table1 <- toptable(coef=coef,number=nrow(genelist),genelist=genelist,A=A,fit=fit,eb=eb))
#      Try(colnames(table1)[ncol(table1)-1] <- sprintf("%-10s",colnames(table1)[ncol(table1)-1]))
      Try(ToptableAbsoluteFilename <- paste(HTMLfilePath ,.Platform$file.sep,"CompleteToptable_Param",coef,".xls",sep=""))
      Try(ToptableRelativeFilename <- paste(HTMLfileRelativePath ,.Platform$file.sep,"CompleteToptable_Param",coef,".xls",sep=""))
      Try(write.table(table1,file=ToptableAbsoluteFilename,quote=FALSE,col.names=NA,sep="\t"))
      Try(HTML.title(paste("Complete Table of Genes Ranked in order of Evidence for Differential Expression for ",ParameterNamesVec[coef]),HR=3))
      Try(HTMLli(txt=paste("<a href=\"",ToptableRelativeFilename,"\"><b>",paste("CompleteToptable_Param",coef,".xls",sep=""),"</b></a>",sep="")))
    }
    # Now the Contrasts
    Try(NumContrastParameterizations <- ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations)
    Try(ContrastsParameterizationNamesVec <- c() )
    Try(contrastNames <- list())
    Try (if (NumContrastParameterizations>0)
      Try(for (cp in (1:NumContrastParameterizations))
      {
          Try(ContrastsParameterizationNamesVec[cp] <- ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]]$contrastsParameterizationNameText)
          Try(contrastsMatrixInList <- ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]]$contrastsMatrixInList)
          Try(contrastsMatrix <- contrastsMatrixInList$contrasts)
          Try(contrastNames[[cp]] <- colnames(contrastsMatrix))
          Try(fit <- (ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]])$fit)
          Try(eb  <- (ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]])$eb)
          for (coef in (1:ncol(contrastsMatrix)))
          {
            Try(options(digits=3))
            Try(table1 <- toptable(coef=coef,number=nrow(genelist),genelist=genelist,A=A,fit=fit,eb=eb))
      #      Try(colnames(table1)[ncol(table1)-1] <- sprintf("%-10s",colnames(table1)[ncol(table1)-1]))
            Try(ToptableAbsoluteFilename <- paste(HTMLfilePath ,.Platform$file.sep,"CompleteToptable_CP_",cp,"Param",coef,".xls",sep=""))
            Try(ToptableRelativeFilename <- paste(HTMLfileRelativePath ,.Platform$file.sep,"CompleteToptable_CP_",cp,"Param",coef,".xls",sep=""))
            Try(write.table(table1,file=ToptableAbsoluteFilename,quote=FALSE,col.names=NA,sep="\t"))
            Try(HTML.title(paste("Complete Table of Genes Ranked in order of Evidence for Differential Expression for ",colnames(contrastsMatrix)[coef]," [",ContrastsParameterizationNamesVec[cp],"]",sep=""),HR=3))
            Try(HTMLli(txt=paste("<a href=\"",ToptableRelativeFilename,"\"><b>",paste("CompleteToptable_CP_",cp,"Param",coef,".xls",sep=""),"</b></a>",sep="")))

        }
      }))



  }

  if (ExportAvgMAPlot && capabilities("png"))
  {
    Try(HTML.title(paste("<a name=\"AvgMAPlot\">M A Plots (with fitted M values) in Parameterization ",ParameterizationNamesVec[parameterizationIndex],"</a>",sep=""),HR=2))
    if (NumParameterizations==0) break()
    Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
    Try(designList <- (ParameterizationList[[ParameterizationNameNode]])$designList)
    Try(design <- designList$design)
    Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
    Try(NumParameters         <- get("NumParameters",envir=limmaGUIenvironment))
    Try(if (nrow(design)==0)
    {
        Try(ParameterNamesVec <- c())
        if (NumParameters>0)
          for (i in (1:NumParameters))
            Try(ParameterNamesVec <- c(ParameterNamesVec,paste("Param",i)))
    }
    else
        Try(ParameterNamesVec <- colnames(design)))
    Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))

    Try(Amatrix <- NULL)
    Try(if ("Amatrix" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
      Amatrix <- (ParameterizationList[[ParameterizationNameNode]])$Amatrix)

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
      Try(A <- unwrapdups(Amatrix,ndups=ndups,spacing=spacing)))

    Try(meanA <- c())
    Try(for (i in (1:nrow(A)))
    {
      Try(meanA[i] <- mean(A[i,]))
    })

    Try(A <- meanA)

    Try(fit <- (ParameterizationList[[ParameterizationNameNode]])$fit)
    Try(eb  <- (ParameterizationList[[ParameterizationNameNode]])$eb)

    Try(if ("genelist" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
      Try(genelist <- (ParameterizationList[[ParameterizationNameNode]])$genelist)
    else
      Try(genelist <- get("genelist",limmaGUIenvironment)))

    for (coef in (1:NumParameters))
    {
      Try(HTML.title(paste("Average M A Plot showing DE genes for",ParameterNamesVec[coef]),HR=3))
      Try(plotTitle <- paste("Average M A Plot showing DE genes for",ParameterNamesVec[coef]))
      Try(if (NumParameters>1)
        Try(M <- fit$coefficients[,coef])
      else
        Try(M <- fit$coefficients))
      Try(numDEgenesLabeled   <- 10)
      Try(GeneLabelsMaxLength <- 10)
      Try(if (NumParameters>1)
        Try(ord <- order(eb$lods[,coef],decreasing=TRUE))
      else
        Try(ord <- order(eb$lods,decreasing=TRUE)))
      Try(topGenes <- ord[1:numDEgenesLabeled])

      Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
        Try(cex <- 0.3)
      else
        Try(cex <- 0.1))
      AvgMAPlotFunction <- function()
      {
        Try(plot(A,M,pch=16,cex=cex,xlab="mean(A)",ylab=paste("M (",ParameterNamesVec[coef],")",sep=""),main=plotTitle))
        Try(text(A[topGenes],M[topGenes],labels=substr(genelist[topGenes,"Name"],1,GeneLabelsMaxLength),cex=0.8,col="blue"))
      }
      Try(HTMLplotUsingFunction(Caption = plotTitle, File=fileNameWithPath, GraphRelativeDirectory = HTMLfileRelativePath ,
        GraphAbsoluteDirectory = HTMLfilePath, GraphFileName = paste("AvgMAPlot",coef,sep=""),
        GraphSaveAs = "png", GraphBorder = 1,  Align = "left", plotFunction=AvgMAPlotFunction,
        Width=plotWidth,Height=plotHeight,PointSize=plotPointSize,BG=plotBG,res=plotRes))

    }

    # Now the Contrasts.

    Try(NumContrastParameterizations <- ParameterizationList[[ParameterizationNameNode]]$NumContrastParameterizations)
    Try(ContrastsParameterizationNamesVec <- c() )
    Try(contrastNames <- list())
    Try (if (NumContrastParameterizations>0)
      Try(for (cp in (1:NumContrastParameterizations))
      {
          Try(ContrastsParameterizationNamesVec[cp] <- ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]]$contrastsParameterizationNameText)
          Try(contrastsMatrixInList <- ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]]$contrastsMatrixInList)
          Try(contrastsMatrix <- contrastsMatrixInList$contrasts)
          Try(contrastNames[[cp]] <- colnames(contrastsMatrix))
          Try(fit <- (ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]])$fit)
          Try(eb  <- (ParameterizationList[[ParameterizationNameNode]]$Contrasts[[cp]])$eb)
          Try(NumContrasts <- ncol(contrastsMatrix))
          for (coef in (1:NumContrasts))
          {
            Try(HTML.title(paste("Average M A Plot showing DE genes for",  contrastNames[coef]),HR=3))
            Try(plotTitle <- paste("Average M A Plot showing DE genes for",contrastNames[coef]))
            Try(if (NumContrasts>1)
              Try(M <- fit$coefficients[,coef])
            else
              Try(M <- fit$coefficients))
            Try(numDEgenesLabeled   <- 10)
            Try(GeneLabelsMaxLength <- 10)
            Try(if (NumContrasts>1)
              Try(ord <- order(eb$lods[,coef],decreasing=TRUE))
            else
              Try(ord <- order(eb$lods,decreasing=TRUE)))
            Try(topGenes <- ord[1:numDEgenesLabeled])

            Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
              Try(cex <- 0.3)
            else
              Try(cex <- 0.1))
            AvgMAPlotFunction2 <- function()
            {
              Try(plot(A,M,pch=16,cex=cex,xlab="mean(A)",ylab=paste("M (",contrastNames[coef],")",sep=""),main=plotTitle))
              Try(text(A[topGenes],M[topGenes],labels=substr(genelist[topGenes,"Name"],1,GeneLabelsMaxLength),cex=0.8,col="blue"))
            }
            Try(HTMLplotUsingFunction(Caption = plotTitle, File=fileNameWithPath, GraphRelativeDirectory = HTMLfileRelativePath ,
              GraphAbsoluteDirectory = HTMLfilePath, GraphFileName = paste("AvgMAPlot_CP_",cp,"_",coef,sep=""),
              GraphSaveAs = "png", GraphBorder = 1,  Align = "left", plotFunction=AvgMAPlotFunction2,
              Width=plotWidth,Height=plotHeight,PointSize=plotPointSize,BG=plotBG,res=plotRes))

          }
      }))

  }

  if (ExporttStatisticBoxPlots && capabilities("png"))
  {
    if (NumParameterizations==0) break()
    Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
    Try(designList <- (ParameterizationList[[ParameterizationNameNode]])$designList)
    Try(NumParameters         <- get("NumParameters",envir=limmaGUIenvironment))
    Try(design <- designList$design)
    Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
    Try(HTML.title(paste("<a name=\"tStatisticBoxPlots\">Box Plots showing the Range of t Statistics for each Spot Type, for each Parameter in Parameterization ",ParameterizationNamesVec[parameterizationIndex],"</a>",sep=""),HR=2))
    Try(if (nrow(design)==0)
    {
        Try(ParameterNamesVec <- c())
        if (NumParameters>0)
          for (i in (1:NumParameters))
            Try(ParameterNamesVec <- c(ParameterNamesVec,paste("Param",i)))
    }
    else
        Try(ParameterNamesVec <- colnames(design)))
    Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
    Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
    Try(eb  <- (ParameterizationList[[ParameterizationNameNode]])$eb)
    Try(ndups   <- get("ndups",envir=limmaGUIenvironment))
    Try(spacing   <- get("spacing",envir=limmaGUIenvironment))       # Global version
    Try(spacing <- GetReducedDuplicateSpacing(parameterizationTreeIndex))

    Try(if ("genelist" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
      Try(genelist <- (ParameterizationList[[ParameterizationNameNode]])$genelist)
    else
      Try(genelist <- get("genelist",limmaGUIenvironment)))
    Try(gal <- get("gal",envir=limmaGUIenvironment))
    Try(SpotTypeStatus <- get("SpotTypeStatus",envir=limmaGUIenvironment))

    Try(ebayesStatistic <- "t")
    Try(SpotTypesIncludedNamesVec <- GetSpotTypesIncludedNames(parameterizationTreeIndex))
    Try(numSpotTypes <- length(SpotTypesIncludedNamesVec))

    for (coef in (1:NumParameters))
    {
      Try(options(digits=3))
      Try(HTML.title(paste("Box Plots showing the Range of t Statistics for each Spot Type, for Parameter ",ParameterNamesVec[coef],sep=""),HR=3))
      # There is a heading HR=4 in the R2HTML cascaded style sheet.
      Try(for (SpotTypeIndex in (1:numSpotTypes))
      {
        Try(SpotType <- SpotTypesIncludedNamesVec[SpotTypeIndex])
        Try(HTML.title(paste("Box Plot(s) showing the Range of t Statistics for each Spot Type, for Parameter ",ParameterNamesVec[coef]," and Spot Type ",SpotType,".",sep=""),HR=3))
        Try(gal2 <-      cbind(gal,SpotTypeStatus=SpotTypeStatus))
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
          Try(genelist2 <- uniquegenelist(gal2[!Omit,],ndups=ndups,spacing=spacing))
        }
        else
          Try(genelist2 <- uniquegenelist(gal2,ndups=ndups,spacing=spacing)))
        Try(subGeneList <- genelist2[genelist2[,"SpotTypeStatus"]==SpotType,])
        Try(t1 <- table((subGeneList[,"Name"])))
        Try(SpotSubTypes <- dimnames(t1)[[1]])
        Try(numMatches <- length(SpotSubTypes))
        if (numMatches==0) next()
        Try(if (NumParameters>1)
          ebayesStatisticsVector <- eb[[ebayesStatistic]][,coef]
        else
          ebayesStatisticsVector <- eb[[ebayesStatistic]])
        Try(plotTitle <- paste("t Statistic Box Plot(s) for Spot Type",SpotType))
        Try(ylabel <- "t Statistic")
        Try(if (numMatches<=50)
        {
          Try(plotCommand <- "boxplot(")
          Try(for (i in (1:numMatches))
            plotCommand <- paste(plotCommand,"ebayesStatisticsVector[genelist2[,\"Name\"]==\"",SpotSubTypes[i],"\"],",sep=""))
          Try(plotCommand <- paste(plotCommand,"names=c(",sep=""))
          Try(
          if (numMatches>1)
            for (i in (1:(numMatches-1)))
              plotCommand <- paste(plotCommand,"\"",SpotSubTypes[i],"\"",",",sep=""))
          Try(plotCommand <- paste(plotCommand,"\"",SpotSubTypes[numMatches],"\"),",sep=""))
          Try(if (numMatches>1)
            Try(plotCommand <- paste(plotCommand,"ylab=\"",ylabel,"\",main=plotTitle)",sep=""))
          else
            Try(plotCommand <- paste(plotCommand,"xlab=\"",SpotType,"\",","ylab=\"",ylabel,"\",main=plotTitle)",sep="")))
        }
        else
        {
          Try(plotCommand <- "boxplot(")
          Try(plotCommand <- paste(plotCommand,"ebayesStatisticsVector[genelist2[,\"SpotTypeStatus\"]==\"",SpotType,"\"],",sep=""))
          Try(plotCommand <- paste(plotCommand,"xlab=\"",SpotType,"\",","names=\"",SpotType,"\",",sep=""))
          Try(plotCommand <- paste(plotCommand,"ylab=\"",ylabel,"\",main=plotTitle)",sep=""))
        })

      #  tkmessageBox(message=plotCommand)

        plotEbayesBoxPlot <- function()
        {
          Try(if (numMatches>50)
          {
            Try(numMatches <- 1)
            Try(opar<-par(bg="white",lab=c(numMatches,20,7)))
          }
          else
            Try(opar<-par(bg="white",cex.axis=0.7,las=2,plt=c(0.15,0.9,0.2,0.85),lab=c(numMatches,20,7))))

          Try(eval(parse(text=plotCommand)))
          Try(tempGraphPar <- par(opar))
          Try(grid(NULL,col="navy"))
        }
        Try(HTMLplotUsingFunction(Caption = plotTitle, File=fileNameWithPath, GraphRelativeDirectory = HTMLfileRelativePath ,
          GraphAbsoluteDirectory = HTMLfilePath, GraphFileName = paste("tStatBPsP",coef,"_ST_",SpotTypeIndex,sep=""),
          GraphSaveAs = "png", GraphBorder = 1,  Align = "left", plotFunction=plotEbayesBoxPlot,
          Width=plotWidth,Height=plotHeight,PointSize=plotPointSize,BG=plotBG,res=plotRes))

      })
    }
  }


  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
  Try(HTMLhr())
  Try(HTMLli(txt="This report was generated by "))
  Try(HTMLli(txt=paste("limmaGUI Version",getPackageVersion("limmaGUI"),"(by James Wettenhall), using")))
  Try(HTMLli(txt=paste("limma Version",getPackageVersion("limma"),"(by Gordon Smyth),")))
  Try(HTMLli(txt=paste("R2HTML Version",getPackageVersion("R2HTML"),"(by Eric Lecoutre) and ")))
  Try(HTMLli(txt=paste("xtable Version",getPackageVersion("xtable"),"(by David Dahl)")))
  Try(HTMLEndFile())
}
