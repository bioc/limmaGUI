ImportMA <- function()
{
  Try(WD <- SetWD())
  Try(if (WD=="") return())
  Try(FileNames <- ImportMADialog())
  
  Try(if (length(FileNames)==0 || FileNames$MFile=="" || FileNames$AFile=="")
    return())

  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))        
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))

  Try(if (NumParameterizations>0)
    for (parameterizationIndex in (1:NumParameterizations))
    {
      Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
      Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
      Try(tkdelete(.limmaGUIglobals$ParameterizationTREE,ParameterizationNameNode))
      Try(assign("ParameterizationList",deleteItemFromList(ParameterizationList,ParameterizationNameNode),limmaGUIenvironment))         
    })

  Try(initGlobals())
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))        
  Try(MAtemp <- read.marrayTools(FileNames$MFile,FileNames$AFile))

  Try(WhetherImportedDataHasBeenNormalized <- tclvalue(tkmessageBox(title="Importing M A Data",
    message="Has this data been normalized?",icon="question",type="yesno")))

  Try(assign("MAimported",MAtemp,limmaGUIenvironment))
  
  Try(gal <- as.data.frame(MAtemp$genes))
  
  Try(ColumnsToBeRemoved <- c())
  Try(if (("grid.r" %in% tolower(colnames(gal))))
    Try(ColumnsToBeRemoved <- c(ColumnsToBeRemoved,match("grid.r",tolower(colnames(gal))))))
  Try(if (("grid.c" %in% tolower(colnames(gal))))
    Try(ColumnsToBeRemoved <- c(ColumnsToBeRemoved,match("grid.c",tolower(colnames(gal))))))
  Try(if (length(ColumnsToBeRemoved)>0 && !("block" %in% tolower(colnames(gal))))
  {
    Try(gal <- data.frame(Block=rep(0,nrow(gal)),gal))
    Try(ColumnsToBeRemoved <- ColumnsToBeRemoved + 1) #Inserting Block shifts existing columns to right.
  })
  Try(ncolGAL <- length(colnames(gal)))
  Try(for (i in (1:ncolGAL))
  {
    Try(if (tolower(colnames(gal)[i])=="spot.r")
      Try(colnames(gal)[i] <- "Row"))
    Try(if (tolower(colnames(gal)[i])=="spot.c")
      Try(colnames(gal)[i] <- "Column"))  
  })
  Try(if (length(ColumnsToBeRemoved)>0)
  {
    Try(if (("grid.r" %in% tolower(colnames(gal)))&&("grid.c" %in% tolower(colnames(gal))))
    {
      Try(GridRcol <- match("grid.r",tolower(colnames(gal))))
      Try(GridCcol <- match("grid.c",tolower(colnames(gal))))
      Try(gal[,"Block"] <- (gal[,GridRcol]-1)*4+gal[,GridCcol])
    })
    Try(gal <- gal[,setdiff(colnames(gal),colnames(gal)[ColumnsToBeRemoved])])
  })
  
  Try(assign("gal",gal,limmaGUIenvironment))
  Try(MAtemp$genes <- gal)
  Try(assign("MA",MAtemp,limmaGUIenvironment))
  Try(if (WhetherImportedDataHasBeenNormalized=="yes")
  {
    Try(assign("RawMADataWasImported",FALSE,limmaGUIenvironment))
    Try(assign("NormalizedMADataWasImported",TRUE,limmaGUIenvironment))
  } else
  {
    Try(assign("RawMADataWasImported",TRUE,limmaGUIenvironment))
    Try(assign("NormalizedMADataWasImported",FALSE,limmaGUIenvironment))
  })
  
  Try(RawMADataWasImported <- get("RawMADataWasImported",envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported <- get("NormalizedMADataWasImported",envir=limmaGUIenvironment))  
  
  Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))  
  Try(MA.Available$Raw <- FALSE)
  Try(MA.Available$WithinArrays <- FALSE)
  Try(MA.Available$BetweenArrays <- FALSE)
  Try(MA.Available$Both <- FALSE)
      
  Try(if (RawMADataWasImported)
  {
    Try(assign("ArraysLoaded",TRUE,limmaGUIenvironment))
    Try(assign("MAraw",MAtemp,limmaGUIenvironment))  
    Try(MA.Available$Raw <- TRUE)
		Try(tkdelete(.limmaGUIglobals$mainTree,"Raw.Status"))
		Try(tkinsert(.limmaGUIglobals$mainTree,"end","Raw","Raw.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
    
    Try(RGtmp <- list())
    Try(RGtmp$R <- 2^(.5*MAtemp$M+MAtemp$A))
    Try(RGtmp$G <- 2^(MAtemp$A-.5*MAtemp$M))
    Try(assign("RG",new("RGList",RGtmp),limmaGUIenvironment))
    Try(assign("RG.Available",TRUE,limmaGUIenvironment))  
  })
  
  Try(assign("maLayout",getLayout(MAtemp$genes),limmaGUIenvironment))
  Try(assign("Layout.Available",TRUE,limmaGUIenvironment))
  
  Try(tkdelete(.limmaGUIglobals$mainTree,"Raw.Status"))
  Try(tkdelete(.limmaGUIglobals$mainTree,"WithinOnly.Status"))
  Try(tkdelete(.limmaGUIglobals$mainTree,"BetweenOnly.Status"))  
  Try(tkdelete(.limmaGUIglobals$mainTree,"WithinAndBetween.Status"))    
  Try(if (MA.Available$Raw)
    Try(tkinsert(.limmaGUIglobals$mainTree,"end","Raw","Raw.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
  else
    Try(tkinsert(.limmaGUIglobals$mainTree,"end","Raw","Raw.Status" ,text="Not Available",font=.limmaGUIglobals$limmaGUIfontTree)))
  Try(tkinsert(.limmaGUIglobals$mainTree,"end","WithinOnly","WithinOnly.Status" ,text="Not Available",font=.limmaGUIglobals$limmaGUIfontTree))
  Try(tkinsert(.limmaGUIglobals$mainTree,"end","BetweenOnly","BetweenOnly.Status" ,text="Not Available",font=.limmaGUIglobals$limmaGUIfontTree))
  Try(tkinsert(.limmaGUIglobals$mainTree,"end","WithinAndBetween","WithinAndBetween.Status" ,text="Not Available",font=.limmaGUIglobals$limmaGUIfontTree))

  Try(tkdelete(.limmaGUIglobals$mainTree,"RG.Status"))
  Try(tkdelete(.limmaGUIglobals$mainTree,"Layout.Status"))          
  
  Try(RG.Available     <- get("RG.Available" , envir=limmaGUIenvironment))  
  Try(Layout.Available <- get("Layout.Available" , envir=limmaGUIenvironment))      
  
  Try(if (RG.Available)  
    Try(tkinsert(.limmaGUIglobals$mainTree,"end","RG","RG.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
  else
    Try(tkinsert(.limmaGUIglobals$mainTree,"end","RG","RG.Status" ,text="Not Available",font=.limmaGUIglobals$limmaGUIfontTree))    )
  Try(if (Layout.Available)  
    Try(tkinsert(.limmaGUIglobals$mainTree,"end","Layout","Layout.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
  else
    Try(tkinsert(.limmaGUIglobals$mainTree,"end","Layout","Layout.Status" ,text="Not Available",font=.limmaGUIglobals$limmaGUIfontTree))    )
  
  Try(assign("SpotTypes" , data.frame(SpotType=I("gene"),ID=I("*"),Name=I("*"),Color=I("black")),limmaGUIenvironment))
  Try(SlideNamesVec <- colnames(MAtemp$M))
  Try(assign("SlideNamesVec",SlideNamesVec,limmaGUIenvironment))
  Try(assign("NumSlides",length(SlideNamesVec),limmaGUIenvironment))
  Try(UpdateSpotTypesStatus())          
  Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
  
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
  Try(GetlimmaDataSetName())
  Try(tkfocus(.limmaGUIglobals$ttMain))        

  return()  
}

ImportMADialog <- function()
{
  Try(MFile <- tclVar("No file is selected."))
  Try(AFile <- tclVar("No file is selected."))
  
  Try(ttImportMA <- tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.deiconify(ttImportMA))
  Try(tkgrab.set(ttImportMA))
  Try(tkfocus(ttImportMA))
  Try(tkwm.title(ttImportMA,"Import M and A"))
  Try(tkgrid(tklabel(ttImportMA,text="    ")))
  Try(label1 <- tklabel(ttImportMA,text="Select Log Ratios (M) File and Log Intensities (A) File",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttImportMA,text="    "), label1))
  Try(tkgrid.configure(label1,columnspan=2))
  Try(tkgrid(tklabel(ttImportMA,text="    ")))
  Try(label2 <- tklabel(ttImportMA,text="Log Ratio (M) File:",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttImportMA,text="    "), label2))
  Try(tkgrid.configure(label2,columnspan=2))     
  Try(label3 <- tklabel(ttImportMA,bg="white",font=.limmaGUIglobals$limmaGUIfont2,
      textvariable=MFile))
  Try(tkgrid(tklabel(ttImportMA,text="    "), label3))
  Try(tkgrid.configure(label3,columnspan=2))       
  Try(tkgrid(tklabel(ttImportMA,text="    ")))
  Try(label4 <- tklabel(ttImportMA,text="Log Intensity (A) File:",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttImportMA,text="    "), label4))
  Try(tkgrid.configure(label4,columnspan=2))         
  Try(label5 <- tklabel(ttImportMA,bg="white",font=.limmaGUIglobals$limmaGUIfont2,
      textvariable=AFile))
  Try(tkgrid(tklabel(ttImportMA,text="    "), label5))
  Try(tkgrid.configure(label5,columnspan=2))           
  Try(tkgrid(tklabel(ttImportMA,text="    ")))
  Try(tkgrid(tklabel(ttImportMA,text="    ")))
  
  Try(SelectMFile <- function() {Try(tmp<-tclvalue(tkgetOpenFile()));Try(if(nchar(tmp))tclvalue(MFile)<-tmp)})
  Try(SelectMButton <- tkbutton(ttImportMA,text="Select M File",font=.limmaGUIglobals$limmaGUIfont2,command=SelectMFile))
  Try(SelectAFile <- function() {Try(tmp<-tclvalue(tkgetOpenFile()));Try(if(nchar(tmp))tclvalue(AFile)<-tmp)})
  Try(SelectAButton <- tkbutton(ttImportMA,text="Select A File",font=.limmaGUIglobals$limmaGUIfont2,command=SelectAFile))
  
  Try(tkgrid(tklabel(ttImportMA,text="    "),SelectMButton,SelectAButton,tklabel(ttImportMA,text="    ")))

  Try(tkgrid(tklabel(ttImportMA,text="    ")))  

  ReturnVal <- list()
  
  Try(onOK <- function() 
  {
    Try(if (tclvalue(MFile)=="No file is selected.") tclvalue(MFile) <- "")
    Try(if (tclvalue(AFile)=="No file is selected.") tclvalue(AFile) <- "")
    Try(ReturnVal <<- list(MFile=tclvalue(MFile),AFile=tclvalue(AFile)))
    Try(tkgrab.release(ttImportMA));Try(tkdestroy(ttImportMA));Try(tkfocus(.limmaGUIglobals$ttMain))
  })
  Try(OK.but     <- tkbutton(ttImportMA,text="   OK   ",font=.limmaGUIglobals$limmaGUIfont2,command=onOK))
  Try(onCancel <- function() {Try(tkgrab.release(ttImportMA));Try(tkdestroy(ttImportMA));Try(tkfocus(.limmaGUIglobals$ttMain));Try(ReturnVal<<-list())})
  Try(Cancel.but <- tkbutton(ttImportMA,text=" Cancel ",font=.limmaGUIglobals$limmaGUIfont2,command=onCancel))  

  Try(tkgrid(tklabel(ttImportMA,text="    "),OK.but,Cancel.but))    
  Try(tkgrid(tklabel(ttImportMA,text="    ")))  

  Try(tkbind(ttImportMA, "<Destroy>", function(){Try(tkgrab.release(ttImportMA));Try(tkfocus(.limmaGUIglobals$ttMain));return(list())}))
  Try(tkwait.window(ttImportMA))
  Try(tkfocus(.limmaGUIglobals$ttMain))

  return(ReturnVal)
}



read.marrayTools <- function(MFile,AFile,path=NULL,verbose=TRUE,sep="\t",quote="\"",header=TRUE,...) {
#	Extracts a genelist, M and A from outputfiles from marrayTools (using gpTools or spotTools)
#	James Wettenhall
#	1 Mar 2004.

	if(missing(MFile) || missing(AFile)) {
		stop("Must specify input files")
	}

	if(!is.null(path)) MFile <- file.path(path,MFile)
	if(!is.null(path)) AFile <- file.path(path,AFile)

	ncols <- ncol(tmp<-read.table(MFile,nrows=10,as.is=TRUE,header=TRUE))

	# Assume that the last annotation column does not contain numerical data.
	column <- ncols
	while(column>=0 && is.numeric(tmp[2,column])) 
		column <- column - 1 
	if (column==0)
		stop("A gene (annotation) column is expected immediately to the left of the numerical data.")

	NumAnnotationColumns <- column

	nslides <- ncols - NumAnnotationColumns

	nspots <- length(readLines(MFile)) - 1

	M <- read.table(MFile,sep=sep,quote=quote,header=header,as.is=TRUE,...)
	if(verbose) 
		cat(paste("Read",MFile,"\n"))
	A <- read.table(AFile,sep=sep,quote=quote,header=header,as.is=TRUE,...)  
	if(verbose) 
		cat(paste("Read",AFile,"\n"))

	MA <- list()
	MA$genes <- M[,1:NumAnnotationColumns]

	M <- M[,(NumAnnotationColumns+1):ncols]
	A <- A[,(NumAnnotationColumns+1):ncols]

	MA$M <- M
	MA$A <- A

	new("MAList",MA)
}



