GetGeneLabelsOptions <- function()
{
  Try(ttGeneLabelsOptions <- tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.deiconify(ttGeneLabelsOptions))
  Try(tkgrab.set(ttGeneLabelsOptions))
  Try(tkfocus(ttGeneLabelsOptions))
  Try(tkwm.title(ttGeneLabelsOptions,"D.E. Gene Labels"))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       ")))
  Try(HowManyDEGenesTcl <- tclVar(paste(10)))
  Try(entry.HowManyDEGenes<-tkentry(ttGeneLabelsOptions,width="12",font=.limmaGUIglobals$limmaGUIfont2,
    textvariable=HowManyDEGenesTcl,bg="white"))
  Try(GeneLabelsMaxLengthTcl <- tclVar(paste(10)))
  Try(entry.GeneLabelsMaxLength<-tkentry(ttGeneLabelsOptions,width="12",font=.limmaGUIglobals$limmaGUIfont2,
    textvariable=GeneLabelsMaxLengthTcl,bg="white"))

  Try(ReturnVal <- list())
  onOK <- function()
  {
    Try(tkgrab.release(ttGeneLabelsOptions))
    Try(tkdestroy(ttGeneLabelsOptions))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    Try(galColName <- tclvalue(IDorNameTcl))
    Try(if (galColName=="Other")
      galColName <- tclvalue(OtherTcl))
    Try(ReturnVal <<- list(HowManyDEGeneLabels=as.integer(tclvalue(HowManyDEGenesTcl)),
                           GeneLabelsMaxLength=as.integer(tclvalue(GeneLabelsMaxLengthTcl)),
                           IDorName=galColName))
  }
  onCancel <- function() {Try(tkgrab.release(ttGeneLabelsOptions));Try(tkdestroy(ttGeneLabelsOptions));Try(tkfocus(.limmaGUIglobals$ttMain)); ReturnVal <<- list()}

  Try(OK.but <-tkbutton(ttGeneLabelsOptions,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <-tkbutton(ttGeneLabelsOptions,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2))


  Try(pleaseLabel <- tklabel(ttGeneLabelsOptions,text="Please select D.E. gene labeling options.",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="    "),pleaseLabel,sticky="w"))
  Try(tkgrid.configure(pleaseLabel,columnspan=2))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       ")))
  Try(numberLabel <- tklabel(ttGeneLabelsOptions,text="Number of labeled differentially expressed genes: ",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(numberLabel,row=3,column=1,columnspan=2))
  Try(tkgrid(entry.HowManyDEGenes,row=3,column=3,sticky="w"))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="    "),row=3,column=4))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       "),row=4))
  Try(tkgrid.configure(entry.HowManyDEGenes,sticky="w"))
  Try(maximumLabel <- tklabel(ttGeneLabelsOptions,text="Maximum length of gene labels: ",font=.limmaGUIglobals$limmaGUIfont2))

  Try(tkgrid(maximumLabel,row=5,column=1,columnspan=2))
  Try(tkgrid(entry.GeneLabelsMaxLength,row=5,column=3,sticky="w"))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="    "),row=5,column=4))

  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       "),row=6))
	Try(IDorNameTcl <- tclVar("Name"))
  Try(rb1 <- tkradiobutton(ttGeneLabelsOptions,text="Use Gene ID",variable=IDorNameTcl,value="ID",font=.limmaGUIglobals$limmaGUIfont2))
	Try(rb2 <- tkradiobutton(ttGeneLabelsOptions,text="Use Gene Name",variable=IDorNameTcl,value="Name",font=.limmaGUIglobals$limmaGUIfont2))
	Try(rb3 <- tkradiobutton(ttGeneLabelsOptions,text="Use Other:",variable=IDorNameTcl,value="Other",font=.limmaGUIglobals$limmaGUIfont2))


  Try(OtherTcl <- tclVar("Other"))
  Try(entry.Other <-tkentry(ttGeneLabelsOptions,width="20",textvariable=OtherTcl,font=.limmaGUIglobals$limmaGUIfont2))

	Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       "),rb1,row=7))
	Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       "),rb2,row=8))
	Try(tkgrid.configure(rb1,sticky="w",columnspan=2))
	Try(tkgrid.configure(rb2,sticky="w",columnspan=2))

	Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       "),rb3,entry.Other,row=9))
	Try(tkgrid.configure(rb3,sticky="w",columnspan=1))
	Try(tkgrid.configure(entry.Other,sticky="w"))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       "),row=10))

  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="       "),OK.but,Cancel.but,row=11))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(ttGeneLabelsOptions,text="    "),row=12))

  Try(tkfocus(ttGeneLabelsOptions))

  Try(tkbind(ttGeneLabelsOptions, "<Destroy>", function() {Try(tkgrab.release(ttGeneLabelsOptions));Try(tkfocus(.limmaGUIglobals$ttMain));}))
  Try(tkwait.window(ttGeneLabelsOptions))

  return(ReturnVal)

}


MAPlotAvg <- function()
{
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
  Try(gal <- get("gal",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))
  Try(ndups   <- get("ndups",  envir=limmaGUIenvironment))
  Try(spacing   <- get("spacing",envir=limmaGUIenvironment))       # Global version

  if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      Try(tkmessageBox(title="M A Plot (with fitted M values)",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="M A Plot (with fitted M values)",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    return()
  }
  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return()    )
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
  Try(spacing <- GetReducedDuplicateSpacing(parameterizationTreeIndex))

  if (Try(LinearModelComputed[parameterizationIndex]==FALSE))
  {
      Try(tkmessageBox(title="M A Plot With Fitted M Values",message=paste("No linear model fit is available for ",ParameterizationNamesVec[parameterizationIndex],".  Please try Compute Linear Model Fit from the Linear Model menu.",sep=""),type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }
  Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))

  GetCoefReturnVal <- GetCoef(parameterizationTreeIndex)
  if (GetCoefReturnVal$coefIndex==0) return()
  Try(coef <- (GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]])$coefOrContrastIndex)
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))

  Try(GeneLabelsOptions <- GetGeneLabelsOptions())
  Try(if (length(GeneLabelsOptions)==0) return())
  Try(numDEgenesLabeled   <- GeneLabelsOptions$HowManyDEGeneLabels)
  Try(GeneLabelsMaxLength <- GeneLabelsOptions$GeneLabelsMaxLength)
  Try(IDorName <- GeneLabelsOptions$IDorName)

	Try(ShowColorCodedSpotTypes <- tclvalue(tkmessageBox(title="Color Coded Spot Types",message="Show color-coded spot types?",icon="question",type="yesnocancel")))
	Try(if (ShowColorCodedSpotTypes=="cancel") return())
	Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
	Try(SpotTypesForLinearModel <- (ParameterizationList[[ParameterizationNameNode]])$SpotTypesForLinearModel)
	Try(SelectedSpotTypes <- SpotTypes[SpotTypesForLinearModel,])
	Try(showLegend <- TRUE)
	Try(if (ShowColorCodedSpotTypes=="yes")
	{
		Try(PlotSymbols <- SelectPlotSymbols(SelectedSpotTypes))
		if (length(PlotSymbols)==0)
				return()
		Try(showLegend  <- PlotSymbols$showLegend)
		Try(PlotSymbols <- PlotSymbols$PlotSymbols)

		Try(SpotTypeStatus <- get("SpotTypeStatus", envir=limmaGUIenvironment))
		Try(numSelectedSpotTypes <- nrow(SelectedSpotTypes))

		Try(pchAllNumeric <- TRUE)
		Try(pchAllCharacter <- TRUE)
		for (i in (1:numSelectedSpotTypes))
		{
				Try(if (PlotSymbols[[i]]$pchIsNumeric==TRUE)
						Try(PlotSymbols[[i]]$pch <- as.numeric(PlotSymbols[[i]]$pch)))
				Try(pchAllNumeric <- (pchAllNumeric && PlotSymbols[[i]]$pchIsNumeric))
				Try(pchAllCharacter <- (pchAllCharacter && (!PlotSymbols[[i]]$pchIsNumeric)))
		}

		Try(cex <- c())
		Try(colVec <- c())
		Try(if (pchAllNumeric || pchAllCharacter)
				Try(pch <- c())
		else
				Try(pch <- list()))

		for (i in (1:numSelectedSpotTypes))
		{
				Try(cex[i] <- PlotSymbols[[i]]$cex)
				Try(colVec[i] <- PlotSymbols[[i]]$col)
				Try(if (pchAllNumeric || pchAllCharacter)
						Try(pch[i] <- PlotSymbols[[i]]$pch)
				else
						Try(pch[[i]] <- PlotSymbols[[i]]$pch))
		}
		Try(values <- SelectedSpotTypes$SpotType)
	})

  Try(plotTitle <- paste("Average MA Plot showing DE genes for",ParameterNamesVec[coef]))

  Try(plotLabels <- GetPlotLabels(plotTitle,"mean(A)","M"))
  Try(if (length(plotLabels)==0) return())
  Try(plotTitle <- plotLabels$plotTitle)
  Try(xLabel    <- plotLabels$xLabel)
  Try(yLabel    <- plotLabels$yLabel)

  Try(if ("genelist" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
    Try(genelist <- (ParameterizationList[[ParameterizationNameNode]])$genelist)
  else
    Try(genelist <- get("genelist",limmaGUIenvironment)))

  Try(ContrastParameterizationIndex <- GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]]$ContrastParameterizationIndex)

  Try(Amatrix <- NULL)
  Try(if ("Amatrix" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
    Amatrix <- (ParameterizationList[[ParameterizationNameNode]])$Amatrix)

  Try(if (GetCoefReturnVal$parameterIsFromMainFit)
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
  })

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

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
      Try(A <- unwrapdups(A[!Omit,],ndups=ndups,spacing=spacing))
    }
    else
      Try(A <- unwrapdups(A,ndups=ndups,spacing=spacing)))
 }
 else
      Try(A <- unwrapdups(Amatrix,ndups=ndups,spacing=spacing)))

  Try(meanA <- c())

  for (i in (1:nrow(A)))
  {
    Try(meanA[i] <- mean(A[i,]))
  }

  Try(A <- meanA)

  Try(if (NumParameters>1)
    Try(M <- fit$coefficients[,coef])
  else
    Try(M <- fit$coefficients))
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
  Try(if (numDEgenesLabeled>0)
  {
    Try(if (NumParameters>1)
      Try(ord <- order(eb$lods[,coef],decreasing=TRUE))
    else
      Try(ord <- order(eb$lods,decreasing=TRUE)))
    Try(topGenes <- ord[1:numDEgenesLabeled])
  })
  Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
   Try(cexScalar <- 0.3)
  else
    Try(cexScalar <- 0.1))
  Try(SpotTypesForLinearModel <- ParameterizationList[[ParameterizationNameNode]]$SpotTypesForLinearModel)
  plotAvgMA <- function()
  {
    Try(opar<-par(bg="white"))

    Try(if (ShowColorCodedSpotTypes=="yes")
      Try(plot(A,M,pch=16,cex=0.3,xlab=xLabel,col="white",ylab=yLabel))
    else
      Try(plot(A,M,pch=16,cex=cexScalar,xlab=xLabel,ylab=yLabel)))
    Try(title(plotTitle))
    Try(if (numDEgenesLabeled>0)
      Try(text(A[topGenes],M[topGenes],labels=substr(as.character(genelist[topGenes,IDorName]),1,GeneLabelsMaxLength),cex=0.8,col="blue")))
    Try(if (ShowColorCodedSpotTypes=="yes")
    {
      Try(SpotTypeStatus <- get("SpotTypeStatus",envir=limmaGUIenvironment))
#      Try(SpotTypes <- get("SpotTypes",envir=limmaGUIenvironment))
#     Try(PointTypes    <- SpotTypes$SpotType)
#     Try(PointColors   <- SpotTypes$Color)
#     Try(numPointTypes <- length(PointTypes))
#     Try(cex           <- rep(0.6,numPointTypes))
#     Try(for (i in (1:numPointTypes))
#       if (tolower(PointTypes[i])=="gene"||PointTypes[i]=="cDNA")
#       {
#         Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
#           cex[i] <- 0.3
#         else
#           cex[i] <- 0.1)
#       })
#     Try(values <- PointTypes)
#     Try(colVec <- PointColors)
      Try(numPointTypes <- nrow(SelectedSpotTypes))
      Try(pch <- as.list(pch))
      Try(cex <- rep(cex,length=numPointTypes))
      Try(for (i in (1:numPointTypes))
      {
        Try(if (SpotTypesForLinearModel[i]==FALSE)
          next())
        Try(sel <- SpotTypeStatus == SelectedSpotTypes$SpotType[i])
        Try(points(A[sel],M[sel],col=colVec[i],pch=pch[[i]],cex=cex[i]))
      })
      Try(if (showLegend)
        Try(legend(min(A, na.rm = TRUE), pch = 16,
                max(M, na.rm = TRUE), legend = values, col = colVec,
                cex = 0.9)))


    })
    Try(tempGraphPar <- par(opar))
  }

   Try(LocalHScale <- .limmaGUIglobals$Myhscale)
   Try(LocalVScale <- .limmaGUIglobals$Myvscale)

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

  Try(if (.limmaGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttAvgMAPlot <- tktoplevel(.limmaGUIglobals$ttMain))
    Try(tkwm.title(ttAvgMAPlot,plotTitle))
    Try(Require("tkrplot"))
    Try(img <-tkrplot(ttAvgMAPlot,plotAvgMA,hscale=LocalHScale,vscale=LocalVScale) )
    Try(SetupPlotKeyBindings(tt=ttAvgMAPlot,img=img))
    Try(SetupPlotMenus(tt=ttAvgMAPlot,initialfile=paste(limmaDataSetNameText,"MAPlotAvg",ParameterNamesVec[coef],sep=""),
                 plotFunction=plotAvgMA,img=img))
    Try(tkgrid(img))
    Try(tkfocus(ttAvgMAPlot))
  }
  else
  {
    Try(plot.new())
    Try(plotAvgMA())
  })
    Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
}

MMPlot <- function()
{
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(gal <- get("gal",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))

  if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      tkmessageBox(title="M M Plot (with fitted M values)",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error")
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="M M Plot (with fitted M values)",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    return()
  }
  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return())
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])

  if (Try(LinearModelComputed[parameterizationIndex]==FALSE))
  {
      Try(tkmessageBox(title="M M Plot With Fitted M Values",message=paste("No linear model fit is available for ",ParameterizationNamesVec[parameterizationIndex],".  Please try Compute Linear Model from the Linear Model menu.",sep=""),type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  if (NumParameters<=1)
  {
      tkmessageBox(title="M M Plot With Fitted M Values",message="There is only one parameter in the linear model fit.",type="ok",icon="error")
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(ParameterNamesVec1  <- GetParameterNames(parameterizationTreeIndex))
  Try(ParameterNamesVec2  <- GetParameterNames(parameterizationTreeIndex))
  GetCoefReturnVal1 <- GetCoef(parameterizationTreeIndex,"first")
  if (GetCoefReturnVal1$coefIndex==0) return()
  Try(coef1 <- (GetCoefReturnVal1$coefIndexList[[GetCoefReturnVal1$coefIndex]])$coefOrContrastIndex)
  Try(ContrastParameterizationIndex1 <- GetCoefReturnVal1$coefIndexList[[GetCoefReturnVal1$coefIndex]]$ContrastParameterizationIndex)
  if (GetCoefReturnVal1$parameterIsFromMainFit)
  {
    Try(fit1 <- (ParameterizationList[[ParameterizationNameNode]])$fit)
    Try(eb1  <- (ParameterizationList[[ParameterizationNameNode]])$eb)
  }
  else
  {
    Try(fit1 <- ((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex1]])$fit)
    Try(eb1  <- ((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex1]])$eb)
    Try(ParameterNamesVec1 <- colnames(((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex1]])$contrastsMatrixInList$contrasts))
  }

  GetCoefReturnVal2 <- GetCoef(parameterizationTreeIndex,"second")
  if (GetCoefReturnVal2$coefIndex==0) return()
  Try(coef2 <- (GetCoefReturnVal2$coefIndexList[[GetCoefReturnVal2$coefIndex]])$coefOrContrastIndex)
  Try(ContrastParameterizationIndex2 <- GetCoefReturnVal2$coefIndexList[[GetCoefReturnVal2$coefIndex]]$ContrastParameterizationIndex)
  if (GetCoefReturnVal2$parameterIsFromMainFit)
  {
    Try(fit2 <- (ParameterizationList[[ParameterizationNameNode]])$fit)
    Try(eb2  <- (ParameterizationList[[ParameterizationNameNode]])$eb)
  }
  else
  {
    Try(fit2 <- ((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex2]])$fit)
    Try(eb2  <- ((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex2]])$eb)
    Try(ParameterNamesVec2 <- colnames(((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex2]])$contrastsMatrixInList$contrasts))
  }

  DEcutoff <- GetDEcutoff()
  if (length(DEcutoff)==0) return()

  cutoffStatistic <- DEcutoff$cutoffStatistic
  cutoff          <- DEcutoff$cutoff

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))
  # Include option for labelling top 10 or however many DE genes.

  if (cutoffStatistic=="abs(t)")
      sel <- (abs(eb1$t[,coef1])>cutoff | abs(eb2$t[,coef2])>cutoff)
  if (cutoffStatistic=="B")
      sel <- (abs(eb1$lods[,coef1])>cutoff | abs(eb2$lods[,coef2])>cutoff)

  Try(M1 <- fit1$coefficients[,coef1])
  Try(M2 <- fit2$coefficients[,coef2])

  Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
   Try(cex <- 0.3)
  else
    Try(cex <- 0.1))
  plotMM <- function()
  {
    Try(opar<-par(bg="white"))
    Try(plot(M1,M2,pch=16,cex=cex,,xlab=ParameterNamesVec1[coef1],ylab=ParameterNamesVec2[coef2]))
    Try(title(plotTitle))
    Try(points(M1[sel],M2[sel],col="blue"))
#    Try(text(M1[topGenes],M2[topGenes],labels=gal[topGenes,IDorName],cex=0.8,col="blue"))
    Try(tempGraphPar <- par(opar))
  }

   Try(LocalHScale <- .limmaGUIglobals$Myhscale)
   Try(LocalVScale <- .limmaGUIglobals$Myvscale)

  Try(plotTitle <- paste("M M Plot for",ParameterNamesVec1[coef1],"vs",ParameterNamesVec2[coef2],"showing DE genes with",cutoffStatistic,">",cutoff))
  Try(plotTitleList <- GetPlotTitle(plotTitle))
  Try(if (length(plotTitleList)==0) return())
  Try(plotTitle <- plotTitleList$plotTitle)


  Try(if (.limmaGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttMMPlot <- tktoplevel(.limmaGUIglobals$ttMain))
    Try(tkwm.title(ttMMPlot,plotTitle))
    Try(Require("tkrplot"))
    Try(img <-tkrplot(ttMMPlot,plotMM,hscale=LocalHScale,vscale=LocalVScale) )
    Try(SetupPlotKeyBindings(tt=ttMMPlot,img=img))
    Try(SetupPlotMenus(tt=ttMMPlot,initialfile=paste(limmaDataSetNameText,"MMPlot",ParameterNamesVec1[coef1],"vs",ParameterNamesVec2[coef2],sep=""),
                 plotFunction=plotMM,img=img))
    Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
    Try(tkgrid(img))
    Try(tkfocus(ttMMPlot))
  }
  else
  {
    Try(plot.new())
    Try(plotMM())
  })
    Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
}


GetParametersAndOrContrasts <- function(parameterizationTreeIndex,whatFor="heat")
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

  Try(ttGetParametersAndOrContrasts<-tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.deiconify(ttGetParametersAndOrContrasts))
  Try(tkgrab.set(ttGetParametersAndOrContrasts)  )
  Try(tkfocus(ttGetParametersAndOrContrasts))
  Try(tkwm.title(ttGetParametersAndOrContrasts,"Choose the parameters and/or contrasts"))
  Try(yscr <- tkscrollbar(ttGetParametersAndOrContrasts, repeatinterval=5,
                       command=function(...)tkyview(tl,...)))
  Try(xscr <- tkscrollbar(ttGetParametersAndOrContrasts, repeatinterval=5,
                       command=function(...)tkxview(tl,...) ,orient="horizontal"))
  Try(tl<-tklistbox(ttGetParametersAndOrContrasts,height=4,selectmode="multiple",xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(yscr,...),background="white",font=.limmaGUIglobals$limmaGUIfont2)   )
  Try(if (whatFor=="heat")
    Try(lbl2<-tklabel(ttGetParametersAndOrContrasts,text=
      "Choose the parameters and/or contrasts to be included in the heat diagram.",font=.limmaGUIglobals$limmaGUIfont2)))
  Try(if (whatFor=="venn")
    Try(lbl2<-tklabel(ttGetParametersAndOrContrasts,text=
      "Choose one, two or three parameters and/or contrasts for the venn diagram.",font=.limmaGUIglobals$limmaGUIfont2)))
  Try(tkgrid(tklabel(ttGetParametersAndOrContrasts,text="       ")))
  Try(tkgrid(tklabel(ttGetParametersAndOrContrasts,text="       ")))
  Try(tkgrid(tklabel(ttGetParametersAndOrContrasts,text="    "),lbl2))
  Try(tkgrid.configure(lbl2,sticky="w"))
  Try(tkgrid(tklabel(ttGetParametersAndOrContrasts,text="         ")))
  Try(tkgrid(tklabel(ttGetParametersAndOrContrasts,text="    "),tl,yscr))
  Try(tkgrid.configure(tl,rowspan=4,sticky="ew"))
  Try(tkgrid.configure(yscr,rowspan=4,sticky="wns"))
  Try(tkgrid(tklabel(ttGetParametersAndOrContrasts,text="    "),xscr,sticky="wne"))

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


  Try(tkselection.set(tl,0,"end"))

  Try(ReturnVal <- list())
  onOK <- function()
  {

      Try(indicesSelected <- as.numeric(strsplit(tclvalue(tkcurselection(tl))," ")[[1]])+1)
      Try(numIndicesSelected <- length(indicesSelected))
      Try(ReturnVal <<- list())
      for (i in (1:numIndicesSelected))
      {
        Try(parameterNum <- indicesSelected[i])
        Try(tkgrab.release(ttGetParametersAndOrContrasts));Try(tkdestroy(ttGetParametersAndOrContrasts));Try(tkfocus(.limmaGUIglobals$ttMain))
        Try(if (parameterNum<=NumParameters)
          Try(ReturnVal[[i]] <<- list(coefIndex=parameterNum,parameterIsFromMainFit=TRUE,coefIndexList=coefIndexList))
        else
          Try(ReturnVal[[i]] <<- list(coefIndex=parameterNum,parameterIsFromMainFit=FALSE,coefIndexList=coefIndexList)))
      }
  }
  onCancel <- function() {Try(tkgrab.release(ttGetParametersAndOrContrasts));Try(tkdestroy(ttGetParametersAndOrContrasts));Try(tkfocus(.limmaGUIglobals$ttMain));Try(ReturnVal <<- list())}
  Try(tkframeOKCancel <- tkframe(ttGetParametersAndOrContrasts))
  Try(OK.but     <- tkbutton(tkframeOKCancel,text="   OK   ",command=onOK,    font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <- tkbutton(tkframeOKCancel,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(tkframeOKCancel,text="    "),columnspan=2))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(tkframeOKCancel,text="    "),columnspan=2))
  Try(tkgrid(tklabel(ttGetParametersAndOrContrasts,text="    "),tkframeOKCancel))
  Try(tkfocus(ttGetParametersAndOrContrasts))
  Try(tkbind(ttGetParametersAndOrContrasts, "<Destroy>", function() {Try(tkgrab.release(ttGetParametersAndOrContrasts));Try(tkfocus(.limmaGUIglobals$ttMain));}))
  Try(tkwait.window(ttGetParametersAndOrContrasts))

  return (ReturnVal)
}

HeatDiagramDialog <- function(parameterName)
{
  Try(ttHeatDiagramDialog <- tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.title(ttHeatDiagramDialog,"Heat Diagram Options"))
  Try(tkwm.deiconify(ttHeatDiagramDialog))
  Try(tkgrab.set(ttHeatDiagramDialog))
  Try(tkfocus(ttHeatDiagramDialog))
  Try(tkframe1 <- tkframe(ttHeatDiagramDialog))
  Try(tkgrid(tklabel(tkframe1,text="    ")))
  Try(tkgrid(tklabel(tkframe1,text="    "),
             tklabel(tkframe1,text="The absolute value of the (moderated) t statistic will be used to plot",
               font=.limmaGUIglobals$limmaGUIfont2)))
  Try(tkgrid(tklabel(tkframe1,text="    "),
             tklabel(tkframe1,text=paste("the heat diagram, relative to parameter ",parameterName,".",sep=""),font=.limmaGUIglobals$limmaGUIfont2),
             tklabel(tkframe1,text="    ")))
  Try(tkgrid(tklabel(tkframe1,text="    ")))
  Try(primaryCutoffTcl <- tclVar("4"))
  Try(otherCutoffTcl   <- tclVar("3"))
  Try(entry.primaryCutoff <- tkentry(tkframe1,textvariable=primaryCutoffTcl,bg="white",width=10,font=.limmaGUIglobals$limmaGUIfont2))
  Try(entry.otherCutoff   <- tkentry(tkframe1,textvariable=otherCutoffTcl,  bg="white",width=10,font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(tkframe1,text="    "),tklabel(tkframe1,text=paste("D.E. cutoff for parameter ",
    parameterName,":   ",sep=""),font=.limmaGUIglobals$limmaGUIfont2),entry.primaryCutoff,tklabel(tkframe1,text="    ")))
  Try(tkgrid(tklabel(tkframe1,text="    "),tklabel(tkframe1,text=
    "D.E. cutoff for other parameters:   ",font=.limmaGUIglobals$limmaGUIfont2),entry.otherCutoff,tklabel(tkframe1,text="    ")))
  Try(tkgrid.configure(entry.primaryCutoff,sticky="w"))
  Try(tkgrid.configure(entry.otherCutoff,sticky="w"))
  Try(tkgrid(tklabel(tkframe1,text="    ")))
  Try(tkgrid(tkframe1))
  Try(tkframeOKCancel <- tkframe(ttHeatDiagramDialog))
  ReturnVal <- list()
  onOK <- function()
  {
    Try(primaryCutoffVal <- as.numeric(tclvalue(primaryCutoffTcl)))
    Try(otherCutoffVal   <- as.numeric(tclvalue(otherCutoffTcl)))
    Try(tkgrab.release(ttHeatDiagramDialog))
    Try(tkdestroy(ttHeatDiagramDialog))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    Try(ReturnVal <<- list(primaryCutoff=primaryCutoffVal,otherCutoff=otherCutoffVal))
  }
  Try(onCancel <- function() {Try(tkgrab.release(ttHeatDiagramDialog));Try(tkdestroy(ttHeatDiagramDialog));Try(tkfocus(.limmaGUIglobals$ttMain));Try(ReturnVal <<- list())})
  Try(OK.but <-tkbutton(tkframeOKCancel,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <-tkbutton(tkframeOKCancel,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid(tklabel(tkframeOKCancel,text="    ")))
  Try(tkgrid(tkframeOKCancel))
  Try(tkfocus(entry.primaryCutoff))
  Try(tkbind(ttHeatDiagramDialog, "<Destroy>", function() {Try(tkgrab.release(ttHeatDiagramDialog));Try(tkfocus(.limmaGUIglobals$ttMain))}))
  Try(tkwait.window(ttHeatDiagramDialog))

  return (ReturnVal)

}


HeatDiagramPlot <- function()
{
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))

  Try(if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      tkmessageBox(title="Heat Diagram",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error")
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  })

  Try(if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="Heat Diagram",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    return()
  })

  Try(if (NumParameters<=1)
  {
    Try(tkmessageBox(title="Heat Diagram",message="To plot a heat diagram, you need to have more than one parameter, i.e. more than two RNA types.",type="ok",icon="error"))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    return()
  })

  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return())
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])

  if (Try(LinearModelComputed[parameterizationIndex]==FALSE))
  {
      Try(tkmessageBox(title="Heat Diagram",message=paste("No linear model fit is available for ",ParameterizationNamesVec[parameterizationIndex],".  Please try Compute Linear Model from the Linear Model menu.",sep=""),type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  GetCoefReturnVal <- GetCoef(parameterizationTreeIndex,"heat")
  if (GetCoefReturnVal$coefIndex==0) return()
  Try(coef <- (GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]])$coefOrContrastIndex)
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))
  Try(ContrastParameterizationIndex <- GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]]$ContrastParameterizationIndex)
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

  Try(fitHD <- list())
  Try(fitHD$coefficients <- as.matrix(as.matrix(fit$coefficients)[,coef]))
  Try(colnames(fitHD$coefficients) <- colnames(fit$coefficients)[coef])
  Try(ebHD <- list())
  Try(ebHD$t  <- as.matrix(as.matrix(eb$t)[,coef]))
  Try(colnames(ebHD$t) <- colnames(eb$t)[coef])

  Try(ParametersAndOrContrasts <- GetParametersAndOrContrasts(parameterizationTreeIndex,whatFor="heat"))
  Try(NumParametersSelected <- length(ParametersAndOrContrasts))
  Try(if (NumParametersSelected==0)
    return())
  Try(coefList <- list())
  for (i in (1:NumParametersSelected))
  {
    Try(coefList[[i]] <- ((ParametersAndOrContrasts[[i]])$coefIndexList[[(ParametersAndOrContrasts[[i]])$coefIndex]])$coefOrContrastIndex)
    Try(ContrastParameterizationIndex <- ParametersAndOrContrasts[[i]]$coefIndexList[[ParametersAndOrContrasts[[i]]$coefIndex]]$ContrastParameterizationIndex)
    Try(if ((ParametersAndOrContrasts[[i]])$parameterIsFromMainFit)
    {
      Try(fit <- (ParameterizationList[[ParameterizationNameNode]])$fit)
      Try(eb  <- (ParameterizationList[[ParameterizationNameNode]])$eb)
    }
    else
    {
      Try(fit <- ((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex]])$fit)
      Try(eb  <- ((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex]])$eb)
    })

    if (colnames(fit$coefficients)[coefList[[i]]]!=colnames(fitHD$coefficients)[1])
    {
      Try(fitHD$coefficients <- cbind(fitHD$coefficients,as.matrix(as.matrix(fit$coefficients)[,coefList[[i]]])))
      Try(ncolfitcoefs <- ncol(fitHD$coefficients))
      Try(colnames(fitHD$coefficients)[ncolfitcoefs] <- colnames(fit$coefficients)[coefList[[i]]])
      Try(ebHD$t             <- cbind(ebHD$t,  as.matrix(as.matrix(eb$t)[,coefList[[i]]])))
      Try(colnames(ebHD$t)[ncolfitcoefs] <- colnames(eb$t)[coefList[[i]]])
    }
  }


  Try(HeatDiagramOptions <- HeatDiagramDialog(colnames(fitHD$coefficients)[1]))
  Try(if (length(HeatDiagramOptions)==0)
    return())
  Try(primaryCutoff <- HeatDiagramOptions$primaryCutoff)
  Try(otherCutoff   <- HeatDiagramOptions$otherCutoff)

  Try(gal      <- get("gal",envir=limmaGUIenvironment))
  Try(if ("genelist" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
    Try(genelist <- (ParameterizationList[[ParameterizationNameNode]])$genelist)
  else
    Try(genelist <- get("genelist",limmaGUIenvironment)))

  plotHD <- function()
  {
    Try(opar<-par(bg="white"))
    Try(heatdiagram(abs(ebHD$t),fitHD$coefficients,primary=1,
      critical.primary=primaryCutoff,critical.other=otherCutoff,
      names=genelist[,"Name"]))
    Try(title(plotTitle))
    Try(TempGraphPar<-par(opar))
  }

  Try(LocalHScale <- .limmaGUIglobals$Myhscale * 1.5)
  Try(LocalVScale <- .limmaGUIglobals$Myvscale * 0.5)

  Try(plotTitle <- paste("Heat diagram relative to parameter",ParameterNamesVec[coef]))

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
  Try(tkfocus(.limmaGUIglobals$ttMain))
  Try(plotTitleList <- GetPlotTitle(plotTitle))
  Try(if (length(plotTitleList)==0) return())
  Try(plotTitle <- plotTitleList$plotTitle)

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

  Try(if (.limmaGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttHeatDiagramPlot <- tktoplevel(.limmaGUIglobals$ttMain))
    Try(tkwm.title(ttHeatDiagramPlot,plotTitle))
    Try(Require("tkrplot"))
    Try(img <-tkrplot(ttHeatDiagramPlot,plotHD,hscale=LocalHScale,vscale=LocalVScale))
    Try(SetupPlotKeyBindings(tt=ttHeatDiagramPlot,img=img))
    Try(SetupPlotMenus(tt=ttHeatDiagramPlot,initialfile=paste(limmaDataSetNameText,"HeatDiagram",sep=""),
                 plotFunction=plotHD,img=img))
    Try(tkgrid(img))
    Try(tkfocus(ttHeatDiagramPlot))
  }
  else
  {
    Try(plot.new())
    Try(plotHD())
  })
    Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
}

ImageArrayPlotDialog <- function(slidenum)
{
  Try(SlideNamesVec <- get("SlideNamesVec",envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported <- get("NormalizedMADataWasImported",envir=limmaGUIenvironment))

  Try(if (NormalizedMADataWasImported==FALSE)
    Try(RCodeString <- "Mraw")
  else
    Try(RCodeString <- "M"))
  Try(ttImageArrayPlotDialog <- tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.deiconify(ttImageArrayPlotDialog))
  Try(tkgrab.set(ttImageArrayPlotDialog))
  Try(tkfocus(ttImageArrayPlotDialog))
  Try(SlideName <- SlideNamesVec[slidenum])
  Try(tkwm.title(ttImageArrayPlotDialog,paste("Image Array Plot for Slide ",SlideName,sep="")))
  tkframe1 <- tkframe(ttImageArrayPlotDialog)
  tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2)
  tkframe4 <- tkframe(tkframe1)
  tkgrid(tklabel(tkframe1,text="    "))
  tkgrid(tklabel(tkframe1,text="Please enter an R expression which you would like an image plot of.",font=.limmaGUIglobals$limmaGUIfont2))
  Try(if (NormalizedMADataWasImported==FALSE)
    tkgrid(tklabel(tkframe1,text="You may use R, Rb, G, Gb, Mraw, Araw, and any standard R functions.",font=.limmaGUIglobals$limmaGUIfont2))
  else
    tkgrid(tklabel(tkframe1,text="You may use M, A, and any standard R functions.",font=.limmaGUIglobals$limmaGUIfont2)))
  tkgrid(tklabel(tkframe1,text="    "))
  tkgrid(tklabel(tkframe2,text="R expression for image array plot",font=.limmaGUIglobals$limmaGUIfont2),columnspan=2)
  Try(Rexpression<- tclVar(RCodeString))
  tkgrid(tklabel(tkframe2,text="    "))
  entry.Rexpression<-tkentry(tkframe2,width="20",font=.limmaGUIglobals$limmaGUIfont2,textvariable=Rexpression,bg="white")
  tkgrid(tklabel(tkframe2,text="R expression : ",font=.limmaGUIglobals$limmaGUIfont2),entry.Rexpression,sticky="w")
  tkgrid(tkframe2)
  tkgrid(tklabel(tkframe1,text="    "))
  ReturnVal <- ""
  onOK <- function()
  {
    # OK

    Try(RexpressionVal <- tclvalue(Rexpression))
    Try(tkgrab.release(ttImageArrayPlotDialog))
    Try(tkdestroy(ttImageArrayPlotDialog))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    Try(ReturnVal <<- RexpressionVal)
  }
  Try(tkbind(entry.Rexpression, "<Return>",onOK))
  onCancel <- function() {Try(tkgrab.release(ttImageArrayPlotDialog));Try(tkdestroy(ttImageArrayPlotDialog));Try(tkfocus(.limmaGUIglobals$ttMain));Try(ReturnVal <<- "")}
  OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2)
  Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2)
  tkgrid(OK.but,Cancel.but)
  tkgrid(tklabel(tkframe4,text="    "))
  tkgrid(tkframe4)
  tkgrid(tkframe1)
  Try(tkfocus(entry.Rexpression))
  Try(tkbind(ttImageArrayPlotDialog, "<Destroy>", function() {Try(tkgrab.release(ttImageArrayPlotDialog));Try(tkfocus(.limmaGUIglobals$ttMain))}))
  Try(tkwait.window(ttImageArrayPlotDialog))
  return (ReturnVal)
}

imageplotlimmaGUI <- function(z, layout=list(ngrid.r=12,ngrid.c=4,nspot.r=26,nspot.c=26), low=NULL, high=NULL, ncolors=123, zerocenter=NULL, zlim=NULL,mar=1,...) {
#  Image plot of spotted microarray data
#  Gordon Smyth
#  20 Nov 2001.  Last revised 27 Nov 2001.

#  Check input
  gr <- layout$ngrid.r
  gc <- layout$ngrid.c
  sr <- layout$nspot.r
  sc <- layout$nspot.c
  if(is.null(gr)||is.null(gc)||is.null(sr)||is.null(sc)) stop("Layout needs to contain components ngrid.r, ngrid.c, nspot.r and spot.c")
  if(length(z) != gr*gc*sr*sc) stop("Number of image spots does not agree with layout dimensions")

#  Check colours
  if(is.character(low)) low <- col2rgb(low)/255
  if(is.character(high)) high <- col2rgb(high)/255
  if(!is.null(low) && is.null(high)) high <- c(1,1,1) - low
  if(is.null(low) && !is.null(high)) low <- c(1,1,1) - high

#  Is zlim preset?
  if(!is.null(zlim)) {
    z <- pmax(zlim[1],z)
    z <- pmin(zlim[2],z)
  }

#  Plot differential expression from "green" to "red" or plot one variable from "white" to "blue"?
  zr <- range(z,na.rm=TRUE)
  zmax <- max(abs(zr))
  zmin <- zr[1]
  if(is.null(zerocenter)) zerocenter <- (zmin < 0)
  if(zerocenter) {
    if(is.null(low)) low <- c(0,1,0)
    if(is.null(high)) high <- c(1,0,0)
    if(is.null(zlim)) zlim <- c(-zmax,zmax)
  } else {
    if(is.null(low)) low <- c(1,1,1)
    if(is.null(high)) high <- c(0,0,1)
    if(is.null(zlim)) zlim <- c(zmin,zmax)
  }

#  Now make the plot
  col <- rgb( seq(low[1],high[1],len=ncolors), seq(low[2],high[2],len=ncolors), seq(low[3],high[3],len=ncolors) )
  dim(z) <- c(sc,sr,gc,gr)
  z <- aperm(z,perm=c(2,4,1,3))
  dim(z) <- c(gr*sr,gc*sc)
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar=rep(mar,4))
  image(0:(gr*sr),0:(gc*sc),z,zlim=zlim,col=col,axes=FALSE,...)
  for (igrid in 0:gc) lines( c(0,gr*sr), rep(igrid*sc,2) )
  for (igrid in 0:gr) lines( rep(igrid*sr,2), c(0,gc*sc) )
  invisible()
}



ImageArrayPlot <- function()
{
  Try(SlideNamesVec <- get("SlideNamesVec",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList", envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))

  if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      Try(tkmessageBox(title="Image Array Plot",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  SetLayoutParamReturnVal<-1
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
  if (length(maLayout)==0) SetLayoutParamReturnVal <-SetLayoutParameters()
  if (SetLayoutParamReturnVal==0)
    return()
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
  Try(slidenum <- GetSlideNum())
  if (slidenum==0)
      return()
  Try(SlideName <- SlideNamesVec[slidenum])
  Try(MA    <- get("MA",envir=limmaGUIenvironment))

  Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
  Try(if (NormalizedMADataWasImported==FALSE)
  {
    Try(RG    <- get("RG",envir=limmaGUIenvironment))
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
  })

  Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))

  if ((nrow(MA$M)>1))
  {
    Try(M <- MA$M)
    Try(A <- MA$A)
  }

  Try(if (NormalizedMADataWasImported==FALSE)
  {
		Try(Mraw <- MAraw$M[,slidenum])
		Try(Araw <- MAraw$A[,slidenum])
		Try(R <- RG$R[,slidenum])
		Try(Rb <- RG$Rb[,slidenum])
		Try(G <- RG$G[,slidenum])
		Try(Gb <- RG$Gb[,slidenum])
  })

  Try(RexpressionVal <- ImageArrayPlotDialog(slidenum))
  Try(if (nchar(RexpressionVal)==0) return())

  Try(ExpressionContainsRed   <- length(grep("R",RexpressionVal))||length(grep("Rb",RexpressionVal)))
  Try(ExpressionContainsGreen <- length(grep("G",RexpressionVal))||length(grep("Gb",RexpressionVal)))

  Try(ExpressionContainsRedOnly   <- (ExpressionContainsRed && !ExpressionContainsGreen))
  Try(ExpressionContainsGreenOnly <- (ExpressionContainsGreen && !ExpressionContainsRed))

  Try(if (ExpressionContainsRedOnly || ExpressionContainsGreenOnly)
  {
    Try(if (ExpressionContainsRedOnly)
      Try(expr <- paste("imageplotlimmaGUI(",RexpressionVal,",layout=maLayout,low=\"white\",high=\"red\",mar=0)",sep="")))
    Try(if (ExpressionContainsGreenOnly)
      Try(expr <- paste("imageplotlimmaGUI(",RexpressionVal,",layout=maLayout,low=\"white\",high=\"green\",mar=0)",sep="")))
  }
  else
    Try(expr <- paste("imageplotlimmaGUI(",RexpressionVal,",layout=maLayout,mar=0)",sep="")))
  Try(exprResult <- try(parse(text=expr)))
  if (inherits(exprResult, "try-error"))
  {
    tkmessageBox(message="Syntax error",icon="error")
    return()
  }
  Try(parPlotSize <- c())
  Try(usrCoords <- c())
  plotImageArray <- function()
  {
    Try(eval(exprResult))  # Evaluate in default environment, i.e. parent.frame()
    Try(parPlotSize <<- par("plt"))
    Try(usrCoords   <<- par("usr"))
  }
  Try(LocalHScale <- .limmaGUIglobals$Myhscale)
  Try(LocalVScale <- .limmaGUIglobals$Myvscale)

  Try(plotTitle <- paste("Image array plot of",RexpressionVal,"for Slide",SlideName,sep=" "))
  Try(plotTitleList <- GetPlotTitle(plotTitle))
  Try(if (length(plotTitleList)==0) return())
  Try(tkfocus(.limmaGUIglobals$ttMain))
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))

  Try(plotTitle <- plotTitleList$plotTitle)

  Try(if (.limmaGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttImageArrayPlotGraph <- tktoplevel(.limmaGUIglobals$ttMain))
    Try(tkwm.title(ttImageArrayPlotGraph,plotTitle))
    Try(Require("tkrplot"))
    Try(img <-tkrplot(ttImageArrayPlotGraph,plotImageArray,hscale=LocalHScale,vscale=LocalVScale))
    Try(SetupPlotKeyBindings(tt=ttImageArrayPlotGraph,img=img))
    Try(plotMenus<-SetupPlotMenus(tt=ttImageArrayPlotGraph,initialfile=paste(limmaDataSetNameText,"ImagePlotSlide",SlideNamesVec[slidenum],sep=""),
               plotFunction=plotImageArray,img=img))
    Try(resizeMenu<-plotMenus$resizeMenu)
    Try(tkdelete(resizeMenu,"0"))
    Try(tkadd(resizeMenu, "command", label="Resize Window",command=function() {Resize(img=img,plotFunction=plotImageArray);Try(parPlotSize <<- par("plt"));Try(usrCoords   <<- par("usr"));  Try(tkconfigure(img,cursor="hand2"))}))
    Try(tkgrid(img))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    Try(tkfocus(ttImageArrayPlotGraph))
  }
  else
  {
    Try(plot.new())
    Try(plotImageArray())
  })
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))

  Try(if (.limmaGUIglobals$graphicsDevice!="tkrplot")
    return())

  Try(tkconfigure(img,cursor="hand2"))
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
  Try(ngrid.r <- maLayout$ngrid.r)
  Try(ngrid.c <- maLayout$ngrid.c)
  Try(nspot.r <- maLayout$nspot.r)
  Try(nspot.c <- maLayout$nspot.c)
  Try(gal <- get("gal",envir=limmaGUIenvironment))
  OnLeftClick <- function(x,y)
  {
    Try(width  <- as.numeric(tclvalue(tkwinfo("reqwidth",img))))
    Try(height <- as.numeric(tclvalue(tkwinfo("reqheight",img))))

    Try(xMin <- parPlotSize[1] * width)
    Try(xMax <- parPlotSize[2] * width)
    Try(yMin <- parPlotSize[3] * height)
    Try(yMax <- parPlotSize[4] * height)

    Try(rangeX <- usrCoords[2] - usrCoords[1])
    Try(rangeY <- usrCoords[4] - usrCoords[3])

    Try(xClick <- as.numeric(x)+0.5)
    Try(yClick <- as.numeric(y)+0.5)
    Try(yClick <- height - yClick)

#    Try(tkmessageBox(message=paste("xClick: ",xClick,", yClick: ",yClick,sep="")))
#    Try(tkmessageBox(message=paste("xMin: ",xMin,", xMax: ",xMax,sep="")))
#    Try(tkmessageBox(message=paste("yMin: ",yMin,", yMax: ",yMax,sep="")))

    Try(xPlotCoord <- xClick / width)
    Try(yPlotCoord <- yClick / height)

#   Try(xPlotCoord <- usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin))
#   Try(yPlotCoord <- usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin))

#    Try(tkmessageBox(message=paste("xPlotCoord: ",xPlotCoord,", yPlotCoord: ",yPlotCoord,sep="")))

    # Assume image plot is plotted on its side (the default)
    Try(blockRow    <- as.integer(xPlotCoord*ngrid.r)+1)
    Try(blockColumn <- as.integer(yPlotCoord*ngrid.c)+1)
    Try(Block <- (blockRow-1) * maLayout$ngrid.c + blockColumn)

    # m mod n
    mod <- function(m,n) m-as.integer(m/n)*n
    Try(xCoordPixels <- as.integer(xPlotCoord*nspot.r*ngrid.r))
    Try(yCoordPixels <- as.integer(yPlotCoord*nspot.c*ngrid.c))

    Try(Column <- mod(yCoordPixels,nspot.c)+1)
    Try(Row    <- mod(xCoordPixels,nspot.r)+1)

    Try(if ("Block" %in% colnames(gal))
      Try(gene <- gal[gal[,"Block"]==Block&gal[,"Row"]==Row&gal[,"Column"]==Column,])
    else if (("Meta Row" %in% colnames(gal)) && ("Meta Column" %in% colnames(gal)))
      Try(gene <- gal[gal[,"Meta Row"]==blockRow&gal[,"Meta Column"]==blockColumn&
          gal[,"Row"]==Row&gal[,"Column"]==Column,])
    else
    {
       Try(tkmessageBox(title="Interactive Image Plot Error",
         message=paste("To use the interactive image plot feature, the gene list",
           "column headings must be (Block,Row,Column) or (Meta Row,Meta Column,Row,Column)"),icon="error"))
       return()
    })

    Try(msg <- "")
    Try(ncolGAL <- ncol(gal))
    Try(for (i in (1:ncolGAL))
    {
      Try(colName <- colnames(gal)[i])
      Try(msg <- paste(msg,colName,": ",gene[,colName],"; ",sep=""))
    })
    Try(tkmessageBox(title="Spot Information",message=msg,icon="info"))
  }
  Try(tkbind(img, "<Button-1>",OnLeftClick))

}


LogOddsPlot <- function()
{
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))

  if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      Try(tkmessageBox(title="Log Odds Plot",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="Log Odds Plot",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    return()
  }
  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return()    )
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])

  if (Try(LinearModelComputed[parameterizationIndex]==FALSE))
  {
      Try(tkmessageBox(title="Log Odds Plot",message=paste("No linear model fit is available for ",ParameterizationNamesVec[parameterizationIndex],".  Please try Compute Linear Model from the Linear Model menu.",sep=""),type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(if ("genelist" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
    Try(genelist <- (ParameterizationList[[ParameterizationNameNode]])$genelist)
  else
    Try(genelist <- get("genelist",limmaGUIenvironment)))
  Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))
  GetCoefReturnVal <- GetCoef(parameterizationTreeIndex)
  if (GetCoefReturnVal$coefIndex==0) return()
  Try(coef <- (GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]])$coefOrContrastIndex)
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(ContrastParameterizationIndex <- GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]]$ContrastParameterizationIndex)
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

  # Have to ask the user how many of the top DE genes they want labeled.
  Try(GeneLabelsOptions <- GetGeneLabelsOptions())
  Try(if (length(GeneLabelsOptions)==0) return())
  Try(numDEgenesLabeled   <- GeneLabelsOptions$HowManyDEGeneLabels)
  Try(GeneLabelsMaxLength <- GeneLabelsOptions$GeneLabelsMaxLength)
  Try(IDorName <- GeneLabelsOptions$IDorName)

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

  Try(if (numDEgenesLabeled>0)
  {
    Try(if (NumParameters>1)
        Try(ord <- order(eb$lods[,coef],decreasing=TRUE))
    else
        Try(ord <- order(eb$lods,decreasing=TRUE)))
    Try(topGenes <- ord[1:numDEgenesLabeled])
  })
  Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
    Try(cex <- 0.3)
  else
    Try(cex <- 0.1))
  plotLogOdds <- function()
  {
    Try(opar<-par(bg="white"))
    if (NumParameters>1)
    {
      plot(fit$coef[,coef],eb$lods[,coef],pch=16,cex=cex,xlab=xLabel,ylab=yLabel)
      Try(title(plotTitle))
      if (numDEgenesLabeled>0)
        text(fit$coef[topGenes,coef],eb$lods[topGenes,coef],labels=substr(as.character(genelist[topGenes,IDorName]),1,GeneLabelsMaxLength),cex=0.8,col="blue")
    }
    else
    {
      plot(fit$coef,eb$lods,pch=16,cex=cex,xlab=xLabel,ylab=yLabel)
      Try(title(plotTitle))
      if (numDEgenesLabeled>0)
        text(fit$coef[topGenes],eb$lods[topGenes],labels=substring(as.character(genelist[topGenes,IDorName]),1,10),cex=0.8,col="blue")
    }
    Try(tempGraphPar <- par(opar))
  }
   Try(LocalHScale <- .limmaGUIglobals$Myhscale)
   Try(LocalVScale <- .limmaGUIglobals$Myvscale)

  Try(plotTitle <- paste("Log Odds Plot for",ParameterNamesVec[coef]))
  Try(plotLabels <- GetPlotLabels(plotTitle,"Log Fold Change","Log Odds"))
  Try(if (length(plotLabels)==0) return())
  Try(plotTitle <- plotLabels$plotTitle)
  Try(xLabel    <- plotLabels$xLabel)
  Try(yLabel    <- plotLabels$yLabel)

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
  Try(tkfocus(.limmaGUIglobals$ttMain))
  Try(plotTitleList <- GetPlotTitle(plotTitle))
  Try(if (length(plotTitleList)==0) return())
  Try(plotTitle <- plotTitleList$plotTitle)

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))


  Try(if (.limmaGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttLogOddsPlot <- tktoplevel(.limmaGUIglobals$ttMain))
    Try(tkwm.title(ttLogOddsPlot,plotTitle))
    Try(Require("tkrplot"))
    Try(img <-tkrplot(ttLogOddsPlot,plotLogOdds,hscale=LocalHScale,vscale=LocalVScale))
    Try(SetupPlotKeyBindings(tt=ttLogOddsPlot,img=img))
    Try(SetupPlotMenus(tt=ttLogOddsPlot,initialfile=paste(limmaDataSetNameText,"LogOddsPlot",ParameterNamesVec[coef],sep=""),
                 plotFunction=plotLogOdds,img=img))
    Try(tkgrid(img))
    Try(tkfocus(ttLogOddsPlot))
  }
  else
  {
    Try(plot.new())
    Try(plotLogOdds())
  })
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
}


DupCorBoxPlot <- function()
{
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(ndups   <- get("ndups",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))

  if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      tkmessageBox(title="Duplicate Correlation Plot",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error")
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="Duplicate Correlation Plot",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    return()
  }

  Try(if (ndups==1)
  {
    Try(tkmessageBox(title="Duplicate Correlation Plot",message="There are no duplicates.",type="ok",icon="error"))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    return()
  })

  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return()  )
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])

  if (Try(LinearModelComputed[parameterizationIndex]==FALSE))
  {
      Try(tkmessageBox(title="Duplicate Correlation Box Plot",message=paste("No linear model fit is available for ",ParameterizationNamesVec[parameterizationIndex],".  Please try Compute Linear Model from the Linear Model menu.",sep=""),type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(dupcor <- (ParameterizationList[[ParameterizationNameNode]])$dupcor)

  if ((is.numeric(dupcor)) && (dupcor==0))
  {
      Try(tkmessageBox(title="Duplicate Correlation Box Plot",message="There are no duplicates.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  Try(if (!("cor.genes" %in% names(dupcor))&&!("all.correlations" %in% names(dupcor)))
  {
    Try(tkmessageBox(title="Duplicate Correlation Box Plot",message="Only available if limmaGUI calculates duplicate correlation (rather than the user)",type="ok",icon="error"))
    return()
  })

  plotDupCor <- function()
  {
    Try(opar<-par(bg="white"))
    Try(if ("cor.genes" %in% names(dupcor))
      Try(boxplot(dupcor$cor.genes))
    else
      Try(boxplot(dupcor$all.correlations)))
    Try(title(plotTitle))
    Try(opar<-par(bg="white"))
  }
   Try(LocalHScale <- .limmaGUIglobals$Myhscale)
   Try(LocalVScale <- .limmaGUIglobals$Myvscale)

  Try(plotTitle <- paste("Duplicate Correlation Box Plot"))
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
  Try(tkfocus(.limmaGUIglobals$ttMain))
  Try(plotTitleList <- GetPlotTitle(plotTitle))
  Try(if (length(plotTitleList)==0) return())
  Try(plotTitle <- plotTitleList$plotTitle)
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))


  Try(if (.limmaGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttDupCorBoxplot <- tktoplevel(.limmaGUIglobals$ttMain))
    Try(tkwm.title(ttDupCorBoxplot,plotTitle))
    Try(Require("tkrplot"))
    Try(img <-tkrplot(ttDupCorBoxplot,plotDupCor,hscale=LocalHScale,vscale=LocalVScale) )
    Try(SetupPlotKeyBindings(tt=ttDupCorBoxplot,img=img))
    Try(SetupPlotMenus(tt=ttDupCorBoxplot,initialfile=paste(limmaDataSetNameText,"DupCorBoxPlot",sep=""),
                 plotFunction=plotDupCor,img=img))
    Try(tkgrid(img))
    Try(tkfocus(ttDupCorBoxplot))
  }
  else
  {
    Try(plot.new())
    Try(plotDupCor())
  })
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))

}


QQTplot <- function()
{
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))

  if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      Try(tkmessageBox(title="Quantile-Quantile t-Statistic Plot",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }
  if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="Quantile-Quantile t-Statistic Plot",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    return()
  }
  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return())
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])

  if (Try(LinearModelComputed[parameterizationIndex]==FALSE))
  {
      Try(tkmessageBox(title="Quantile-Quantile t Statistic Plot",message=paste("No linear model fit is available for ",ParameterizationNamesVec[parameterizationIndex],".  Please try Compute Linear Model from the Linear Model menu.",sep=""),type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  GetCoefReturnVal <- GetCoef(parameterizationTreeIndex)
  if (GetCoefReturnVal$coefIndex==0) return()
  Try(coef <- (GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]])$coefOrContrastIndex)
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(ContrastParameterizationIndex <- GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]]$ContrastParameterizationIndex)
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

  Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
    Try(cex <- 0.3)
  else
    Try(cex <- 0.1))
  plotQQT <- function()
  {
    Try(opar<-par(bg="white"))
    if (NumParameters>1)
      qqt(eb$t[,coef],df=fit$df+eb$df,pch=16,cex=cex,main=plotTitle)
    else
      qqt(eb$t,df=fit$df+eb$df,pch=16,cex=cex,main=plotTitle)
    abline(0,1)
    Try(tempGraphPar <- par(opar))
  }
   Try(LocalHScale <- .limmaGUIglobals$Myhscale)
   Try(LocalVScale <- .limmaGUIglobals$Myvscale)

  Try(plotTitle <- paste("Student's t Quantile-Quantile Plot for",ParameterNamesVec[coef]))
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
  Try(plotTitleList <- GetPlotTitle(plotTitle))
  Try(if (length(plotTitleList)==0) return())
  Try(tkfocus(.limmaGUIglobals$ttMain))
  Try(plotTitle <- plotTitleList$plotTitle)

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

  Try(if (.limmaGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttQQTplot <- tktoplevel(.limmaGUIglobals$ttMain))
    Try(tkwm.title(ttQQTplot,plotTitle))
    Try(Require("tkrplot"))
    Try(img <-tkrplot(ttQQTplot,plotQQT,hscale=LocalHScale,vscale=LocalVScale))
    Try(SetupPlotKeyBindings(tt=ttQQTplot,img=img))
    Try(SetupPlotMenus(tt=ttQQTplot,initialfile=paste(limmaDataSetNameText,"QQTPlot",ParameterNamesVec[coef],sep=""),
                 plotFunction=plotQQT,img=img))
    Try(tkgrid(img))
    Try(tkfocus(ttQQTplot))
  }
  else
  {
    Try(plot.new())
    Try(plotQQT())
  })
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))

}
#
#
#
#
MBoxPlot <- function(){
	#need to use limma funcion
	# boxplot(MA$M~col(MA$M),names=colnames(MA$M))
	#instead of sma function
	#Exclude the M .v. Print Tip group
	#
	Try(SlideNamesVec   <- get("SlideNamesVec",envir=limmaGUIenvironment))
	Try(ArraysLoaded    <- get("ArraysLoaded", envir=limmaGUIenvironment))
	Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
	Try(WeightingType   <- get("WeightingType",envir=limmaGUIenvironment))
	Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
	#
	if(ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE){
		tkmessageBox(title="M Box Plot",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error")
		Try(tkfocus(.limmaGUIglobals$ttMain))
		return()
	}
	#
	SetLayoutParamReturnVal<-1
	Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
	if (length(maLayout)==0) SetLayoutParamReturnVal <-SetLayoutParameters()
	if (SetLayoutParamReturnVal==0) return()
	Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
	#
	#ttMBoxPlot<-tktoplevel(.limmaGUIglobals$ttMain)
	#tkwm.deiconify(ttMBoxPlot)
	#tkgrab.set(ttMBoxPlot)
	#tkfocus(ttMBoxPlot)
	#tkwm.title(ttMBoxPlot,"M Box Plot Options")
	plotby <- tclVar("Slide")
	#tkframe1 <- tkframe(ttMBoxPlot,borderwidth=2)
	#tkframe3 <- tkframe(tkframe1,relief="groove",borderwidth=2)
	#tkframe4<-tkframe(tkframe1,borderwidth=2)
	#
	#Abort <- 0
	#plotbyval <- ""
	#onOK <- function(){
	#	plotbyval <<- tclvalue(plotby)
	#	Try(tkgrab.release(ttMBoxPlot))
	#	Try(tkdestroy(ttMBoxPlot))
	#	Try(tkfocus(.limmaGUIglobals$ttMain))
	#	Abort <<-0
	#} #end of onOK <- function()
	#
	#onCancel <- function() {Try(tkgrab.release(ttMBoxPlot));Try(tkdestroy(ttMBoxPlot));Try(tkfocus(.limmaGUIglobals$ttMain));Abort <<-1}
	#
	#OK.but       <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2)
	#Cancel.but   <-tkbutton(tkframe4,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2)
	#tkgrid(tklabel(tkframe1,text="                    "))
	#tkgrid(tklabel(tkframe3,text="Plot by print-tip group or by slide?",font=.limmaGUIglobals$limmaGUIfont2),sticky="ew")
	#PrintTip.but <- tkradiobutton(tkframe3,text="Print Tip Group",variable=plotby,value="PrintTip",font=.limmaGUIglobals$limmaGUIfont2)
	#Slide.but    <- tkradiobutton(tkframe3,text="Slide",variable=plotby,value="Slide",font=.limmaGUIglobals$limmaGUIfont2)
	#tkgrid.configure(PrintTip.but,sticky="w")
	#tkgrid.configure(Slide.but,sticky="w")
	#
	#tkgrid(PrintTip.but)
	#tkgrid(Slide.but)
	#tkgrid(tkframe3,sticky="w")
	#tkgrid(tkframe1)
	#tkgrid(tklabel(tkframe4,text="    "),sticky="ew")
	#tkgrid(OK.but,Cancel.but)
	#tkgrid(tkframe4,sticky="ew")
	#tkfocus(ttMBoxPlot)
	#tkbind(ttMBoxPlot, "<Destroy>", function() {Try(tkgrab.release(ttMBoxPlot));Try(tkfocus(.limmaGUIglobals$ttMain));})
	#Only allow plot by slide
	plotbyval = "Slide"
	#tkwait.window(ttMBoxPlot)
	#if (Abort==1) return()
	#
	slidenum <- 0
	#if (plotbyval=="PrintTip"){
	#	slidenum <- GetSlideNum()
	#	if (slidenum==0){
	#		return()
	#	}
	#}
	#
	Try(
		if (NormalizedMADataWasImported==FALSE){
			Try(NormalizeWithinArraysMB <-tkmessageBox(title="Normalization Within Arrays",message="Normalize Within Arrays?",type="yesnocancel",icon="question",default="no"))
			Try(WhetherToNormalizeWithinArrays <- tclvalue(NormalizeWithinArraysMB))
			if (WhetherToNormalizeWithinArrays=="cancel")return()
			#
			Try(NormalizeBetweenArraysMB <-tkmessageBox(title="Normalization Between Arrays",message="Normalize Between Arrays?",type="yesnocancel",icon="question",default="no"))
			Try(WhetherToNormalizeBetweenArrays <- tclvalue(NormalizeBetweenArraysMB))
			if (WhetherToNormalizeBetweenArrays=="cancel")return()
			#
			Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
			Try(tkfocus(.limmaGUIglobals$ttMain))
			#
			Try(RG <- get("RG",envir=limmaGUIenvironment))
			Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
			Try(
				if(!exists("WithinArrayNormalizationMethod",envir=limmaGUIenvironment)){
					Try(WithinArrayNormalizationMethod <- "printtiploess")
					Try(assign("WithinArrayNormalizationMethod",WithinArrayNormalizationMethod,limmaGUIenvironment))
				}
			)
			Try(WithinArrayNormalizationMethod <- get("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
			#
			Try(
				if (WhetherToNormalizeWithinArrays=="yes"){
					if (MA.Available$WithinArrays){
						Try(MA <- get("MAwithinArrays",envir=limmaGUIenvironment))
					}else{
						if (WeightingType == "none"){
							Try (MA <- normalizeWithinArrays(RG,maLayout,method=WithinArrayNormalizationMethod))
						}else{
							Try(MA <- normalizeWithinArrays(RG,weights=RG$weights,maLayout,method=WithinArrayNormalizationMethod))
						}
						Try(assign("MAwithinArrays",MA,limmaGUIenvironment))
						Try(MA.Available$WithinArrays <- TRUE)
						Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
						Try(tkdelete(.limmaGUIglobals$mainTree,"WithinOnly.Status"))
						Try(WithinArrayNormalizationMethod <- get("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
						Try(tkinsert(.limmaGUIglobals$mainTree,"end","WithinOnly","WithinOnly.Status" ,text=paste("Available (using ",WithinArrayNormalizationMethod,")",sep=""),font=.limmaGUIglobals$limmaGUIfontTree))
					} #end of else/if (MA.Available$WithinArrays)
				}else{
					if (MA.Available$Raw){
						Try(MA <- get("MAraw",envir=limmaGUIenvironment))
					}else{
						Try (MA <- MA.RG(RG))
						Try(assign("MAraw",MA,limmaGUIenvironment))
						Try(MA.Available$Raw <- TRUE)
						Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
						Try(tkdelete(.limmaGUIglobals$mainTree,"Raw.Status"))
						Try(tkinsert(.limmaGUIglobals$mainTree,"end","Raw","Raw.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
					} #end of else/if (MA.Available$Raw)
				} #end of else/if (WhetherToNormalizeWithinArrays=="yes")
			)
			#
			Try(
				if (WhetherToNormalizeBetweenArrays=="yes"){
					if (WhetherToNormalizeWithinArrays=="yes"){
						if (MA.Available$Both){
							Try(MA <- get("MAboth",envir=limmaGUIenvironment))
						}else{
							Try (MA <- normalizeBetweenArrays(MA))
							Try(assign("MAboth",MA,limmaGUIenvironment))
							Try(MA.Available$Both <- TRUE)
							Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
							Try(tkdelete(.limmaGUIglobals$mainTree,"WithinAndBetween.Status"))
							Try(tkinsert(.limmaGUIglobals$mainTree,"end","WithinAndBetween","WithinAndBetween.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
						} #end of else/if (MA.Available$Both)
					}else{
						if (MA.Available$BetweenArrays){
							Try(MA <- get("MAbetweenArrays",envir=limmaGUIenvironment))
						}else{
							Try (MA <- normalizeBetweenArrays(MA))
							Try(assign("MAbetweenArrays",MA,limmaGUIenvironment))
							Try(MA.Available$BetweenArrays <- TRUE)
							Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
							Try(tkdelete(.limmaGUIglobals$mainTree,"BetweenOnly.Status"))
							Try(tkinsert(.limmaGUIglobals$mainTree,"end","BetweenOnly","BetweenOnly.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
						} #end of else/if (MA.Available$BetweenArrays)
					} #end of else/if (WhetherToNormalizeWithinArrays=="yes")
				} #end of if (WhetherToNormalizeBetweenArrays=="yes")
			)
		}else{
			Try(MA <- get("MAimported",envir=limmaGUIenvironment))
		} #end of else/if (NormalizedMADataWasImported==FALSE)
	)
	#
	plot.scale.box0 <- function(){
		###Try(Require("sma")) #replaced by limma function 16 October 2009
		Try(
			if (plotbyval=="PrintTip"){ #This wont happen now
				Try(opar<-par(bg="white"))
				Try(plot.scale.box(MA$M[,slidenum],maLayout,col=rainbow(maLayout$ngrid.r*maLayout$ngrid.c),xlab="Print Tip Group",ylab="M"))
				Try(title(plotTitle))
				Try(tempGraphPar <- par(opar))
			}else{ #then plotbyval = Slide
				Try(opar<-par(bg="white"))
				# Maybe allow las=2 as an option, i.e. vertical text labels for x axis.
				Try(
					if (min(nchar(gsub("[^0-9]","",SlideNamesVec))==nchar(SlideNamesVec))==TRUE){
						SlideNamesVec <- paste("Slide",SlideNamesVec)
					}
				)
				### this is the sma command
				#Try(plot.scale.box(MA$M,        x.names=SlideNamesVec, xlab="Slide", ylab="M"))
				###Use the limma command
				#           boxplot(MA$M~col(MA$M),names=colnames(MA$M))
				Try(boxplot(MA$M~col(MA$M),x.names=SlideNamesVec,xlab="Slide",ylab="M"))
				Try(title(plotTitle))
				Try(tempGraphPar <- par(opar))
			} #end of else/if (plotbyval=="PrintTip")
		)
		Try(abline(0,0))
	} #end of plot.scale.box0 <- function()
	#
	Try(LocalHScale <- .limmaGUIglobals$Myhscale)
	Try(LocalVScale <- .limmaGUIglobals$Myvscale)
	#
	if (plotbyval=="PrintTip"){ #wont happen now
		if (NormalizedMADataWasImported==FALSE){
			if (WhetherToNormalizeWithinArrays=="yes"){
				if (WhetherToNormalizeBetweenArrays=="yes"){
					Try(plotTitle <- paste("M Box Plot for slide ",SlideNamesVec[slidenum]," with normalization within and between arrays",sep=""))
				}else{
					Try(plotTitle <- paste("M Box Plot for slide ",SlideNamesVec[slidenum]," with normalization within arrays only",sep=""))
				}
			}else{
				if(WhetherToNormalizeBetweenArrays=="yes"){
					Try(plotTitle <- paste("M Box Plot for slide ",SlideNamesVec[slidenum]," with normalization between arrays only",sep=""))
				}else{
					Try(plotTitle <- paste("M Box Plot for slide ",SlideNamesVec[slidenum]," with no normalization",sep=""))
				}
			} #end of else/if (WhetherToNormalizeWithinArrays=="yes")
		}else{
			Try(plotTitle <- paste("M Box Plot for slide ",SlideNamesVec[slidenum],sep=""))
		} #end of else/if (NormalizedMADataWasImported==FALSE)
	}else{ #plotbyval = "Slide"
		if(NormalizedMADataWasImported==FALSE){
			if(WhetherToNormalizeWithinArrays=="yes"){
				if (WhetherToNormalizeBetweenArrays=="yes"){
					Try(plotTitle <- paste("M Box Plot for all slides with normalization within and between arrays",sep=""))
				}else{
					Try(plotTitle <- paste("M Box Plot for all slides with normalization within arrays only",sep=""))
				}
			}else{
				if(WhetherToNormalizeBetweenArrays=="yes"){
					Try(plotTitle <- paste("M Box Plot for all slides with normalization between arrays only",sep=""))
				}else{
					Try(plotTitle <- paste("M Box Plot for all slides with no normalization",sep=""))
				}
			} #end of else/if (WhetherToNormalizeWithinArrays=="yes")
		}else{
			Try(plotTitle <- paste("M Box Plot for all slides"))
		} #end of else/if(NormalizedMADataWasImported==FALSE)
	} #end of else/if (plotbyval=="PrintTip")
	#
	Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
	Try(plotTitleList <- GetPlotTitle(plotTitle))
	Try(if (length(plotTitleList)==0) return())
	#
	Try(tkfocus(.limmaGUIglobals$ttMain))
	Try(plotTitle <- plotTitleList$plotTitle)
	#
	Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
	Try(tkfocus(.limmaGUIglobals$ttMain))
	#
	Try(
		if(.limmaGUIglobals$graphicsDevice=="tkrplot"){
			Try(ttMBoxPlotGraph <- tktoplevel(.limmaGUIglobals$ttMain))
			Try(tkconfigure(ttMBoxPlotGraph,cursor="watch"))
			Try(tkwm.title(ttMBoxPlotGraph,plotTitle))
			Try(Require("tkrplot"))
			Try(img <-tkrplot(ttMBoxPlotGraph,plot.scale.box0,hscale=LocalHScale,vscale=LocalVScale))
			Try(SetupPlotKeyBindings(tt=ttMBoxPlotGraph,img=img))
			Try(SetupPlotMenus(tt=ttMBoxPlotGraph,initialfile=paste(limmaDataSetNameText,"MBoxPlotSlide",SlideNamesVec[slidenum],sep=""),plotFunction=plot.scale.box0,img=img))
			Try(tkgrid(img))
			Try(tkconfigure(ttMBoxPlotGraph,cursor="arrow"))
			Try(tkfocus(ttMBoxPlotGraph))
		}else{
			Try(plot.new())
			Try(plot.scale.box0())
		} #end of else/if(.limmaGUIglobals$graphicsDevice=="tkrplot")
	)
	Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
} #end of MBoxPlot <- function()
#
#
PrintTipGroupMAPlot <- function(){
  Try(SlideNamesVec <- get("SlideNamesVec",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
  Try(RG              <- get("RG", envir=limmaGUIenvironment))
  Try(MAraw           <- get("MAraw",envir=limmaGUIenvironment))

  if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      Try(tkmessageBox(title="Print-Tip Group M A Plot",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  SetLayoutParamReturnVal<-1
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
  if (length(maLayout)==0)
    SetLayoutParamReturnVal <-SetLayoutParameters()
  if (SetLayoutParamReturnVal==0)
    return()
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))

  slidenum <- GetSlideNum()
  if (slidenum==0)
      return()

  Try(if (NormalizedMADataWasImported==TRUE)
    Try(MA <- get("MA",envir=limmaGUIenvironment))
  else
  {
		Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
		if (MA.Available$Raw)
			Try(MA <- get("MAraw",envir=limmaGUIenvironment))
		else
		{
			Try (MA <- MA.RG(RG))
			Try(assign("MAraw",MA,limmaGUIenvironment))
			Try(MA.Available$Raw <- TRUE)
			Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
			Try(tkdelete(.limmaGUIglobals$mainTree,"Raw.Status"))
			Try(tkinsert(.limmaGUIglobals$mainTree,"end","Raw","Raw.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
		}
  })
  PrintTipGroupPlot <- function()
  {
     Try(opar<-par(bg="white"))
     plotPrintTipLoess(MA,layout=maLayout,array=slidenum)
     Try(tempGraphPar <- par(opar))
  }
   Try(LocalHScale <- .limmaGUIglobals$Myhscale)
   Try(LocalVScale <- .limmaGUIglobals$Myvscale)

   Try(plotTitle <- paste("Print-Tip Group M A Plots for slide ",SlideNamesVec[slidenum],sep=""))

  Try(if (.limmaGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttPrintTipGroupMAPlotGraph <- tktoplevel(.limmaGUIglobals$ttMain))
    Try(Require("tkrplot"))
    Try(tkwm.title(ttPrintTipGroupMAPlotGraph,plotTitle))
    Try(img <- tkrplot(ttPrintTipGroupMAPlotGraph,PrintTipGroupPlot,hscale=LocalHScale,vscale=LocalVScale) )
    Try(SetupPlotKeyBindings(tt=ttPrintTipGroupMAPlotGraph,img=img))
    Try(SetupPlotMenus(tt=ttPrintTipGroupMAPlotGraph,initialfile=paste(limmaDataSetNameText,"PrintTipGroupMAPlotSlide",SlideNamesVec[slidenum],sep=""),
                 plotFunction=PrintTipGroupPlot,img=img))

    Try(tkgrid(img))
    Try(tkfocus(ttPrintTipGroupMAPlotGraph))
  }
  else
  {
    Try(plot.new())
    Try(PrintTipGroupPlot())
  })
} #end of PrintTipGroupMAPlot <- function()

MAPlot <- function(){
	Try(SlideNamesVec <- get("SlideNamesVec",envir=limmaGUIenvironment))
	Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
	Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
	Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))
	#
	if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE){
		Try(tkmessageBox(title="M A Plot",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
		Try(tkfocus(.limmaGUIglobals$ttMain))
		return()
	}
	#
	Try(
		if (NormalizedMADataWasImported==FALSE){
			Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
			Try(RG <- get("RG",envir=limmaGUIenvironment))
		}else{
			Try(MA <- get("MAimported",envir=limmaGUIenvironment))
			Try(RGtmp <- list())
			Try(RGtmp$R <- 2^(MA$M*.5+MA$A))
			Try(RGtmp$G <- 2^(MA$A-.5*MA$M))
			Try(RG <- new("RGList",RGtmp))
		}
	)
	#
	SetLayoutParamReturnVal<-1
	Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
	if (length(maLayout)==0){
		SetLayoutParamReturnVal <-SetLayoutParameters()
	}
	if (SetLayoutParamReturnVal==0){
		return()
	}
	#
	Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
	#
	slidenum <- GetSlideNum()
	if (slidenum==0){
		return()
	}
	#
	Try(
		if (NormalizedMADataWasImported==FALSE){
			Try(NormalizeWithinArrayMB <-tkmessageBox(title="Normalization Within A Single Array",message="Normalize Within Single Array (fast approximate method) ?",type="yesnocancel",icon="question",default="no"))
			Try(WhetherToNormalizeWithinArray <- tclvalue(NormalizeWithinArrayMB))
			if (WhetherToNormalizeWithinArray=="cancel"){
				return()
			}
		} #end of if (NormalizedMADataWasImported==FALSE)
	)
	#
	Try(
		if (NormalizedMADataWasImported==FALSE){
			if (WhetherToNormalizeWithinArray=="no"){
				# We don't actually need MAraw in this plot.  But users may expect that plotting M and A in this will
				# will create an MAraw object.
				Try (MAraw <- MA.RG(RG))
				Try(assign("MAraw",MAraw,limmaGUIenvironment))
				Try(MA.Available$Raw <- TRUE)
				Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
				Try(tkdelete(.limmaGUIglobals$mainTree,"Raw.Status"))
				Try(tkinsert(.limmaGUIglobals$mainTree,"end","Raw","Raw.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
			}
		}
	)
	#
	Try(lowessChoice <- GetLowessType())
	Try(
		if (lowessChoice==""){ # User pressed Cancel
			Try(tkfocus(.limmaGUIglobals$ttMain))
			return()
		}
	)
	#
	MAraw <- MA.RG(RG)
	#
	Try(
		if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows"){
			Try(cex <- 0.3) #Do this for Unix and MacOSX
		}else{
			Try(cex <- 0.1) #Do thhis for Windows
		}
	)
	plotFun <- function(){
		Try(Require("sma"))
		Try(opar<-par(bg="white"))
		#
		Try(
			if (NormalizedMADataWasImported==FALSE){
				Try(
					if (WhetherToNormalizeWithinArray=="yes"){
						normval <- "p"
					}else{
						normval <- "n"
					}
				)
			}else{
				normval <- "n"
			} #end of else/if (NormalizedMADataWasImported==FALSE)
		)
		#
		Try(
			if (WhetherToNormalizeWithinArray=="yes"){
				plot.type <- "n"
			}else{
				plot.type <- "r"
			}
		)
		#
		Try(
			if (lowessChoice=="printtip"){
				###Try(plot.print.tip.lowess(RG,maLayout,pch=16,cex=cex,image=slidenum,norm=normval))
				Try(plotPrintTipLoess(RG,maLayout,array=slidenum,span=0.4,...))
			}
		)
		#
		Try(
			if ((lowessChoice!="printtip")  && normval=="p"){
				normval <- "l"
			}
		)
		#
		Try(
			if (lowessChoice=="none"){
				Try(
					if (length(SlideNamesVec)>1){
						Try(plot(MAraw$A[,slidenum],MAraw$M[,slidenum],pch=16,cex=0.3,xlab="A",ylab="M"))
					}else{
						Try(plot(MAraw$A,MAraw$M,pch=16,cex=0.3,xlab="A",ylab="M"))
					}
				)
			} #end of if (lowessChoice=="none")
		)
		#
		Try(
			if (lowessChoice=="global"){
				Try(plot.mva(RG,pch=16,cex=cex,image=slidenum,norm=normval,plot.type=plot.type))
			}
		)
		#
		Try(title(plotTitle))
		#
		Try(tempGraphPar <- par(opar))
		#
	} #end of plotFun <- function()
	#
	#
	Try(LocalHScale <- .limmaGUIglobals$Myhscale)
	Try(LocalVScale <- .limmaGUIglobals$Myvscale)
	#
	Try(
		if(NormalizedMADataWasImported==FALSE){
			if (WhetherToNormalizeWithinArray=="yes"){
				Try(plotTitle <- paste("M A Plot for slide ",SlideNamesVec[slidenum]," with normalization",sep=""))
			}else{
				Try(plotTitle <- paste("M A Plot for slide ",SlideNamesVec[slidenum]," with no normalization",sep=""))
			} #end of else/if (WhetherToNormalizeWithinArray=="yes")
		}else{
			Try(plotTitle <- paste("M A Plot for slide ",SlideNamesVec[slidenum]))
		} #end of else/if(NormalizedMADataWasImported==FALSE)
	)
	#
	Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
	Try(tkfocus(.limmaGUIglobals$ttMain))
	Try(plotTitleList <- GetPlotTitle(plotTitle))
	Try(if (length(plotTitleList)==0) return())
	Try(plotTitle <- plotTitleList$plotTitle)
	#
	Try(
		if (.limmaGUIglobals$graphicsDevice=="tkrplot"){
			Try(ttMAPlotGraph <- tktoplevel(.limmaGUIglobals$ttMain))
			Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
			Try(tkconfigure(ttMAPlotGraph,cursor="watch"))
			Try(tkfocus(.limmaGUIglobals$ttMain))
			Try(Require("tkrplot"))
			Try(tkwm.title(ttMAPlotGraph,plotTitle))
			Try(img <-tkrplot(ttMAPlotGraph,plotFun,hscale=LocalHScale,vscale=LocalVScale) )
			Try(SetupPlotKeyBindings(tt=ttMAPlotGraph,img=img))
			Try(SetupPlotMenus(tt=ttMAPlotGraph,initialfile=paste(limmaDataSetNameText,"MAPlotSlide",SlideNamesVec[slidenum],sep=""),plotFunction=plotFun,img=img))
			Try(tkgrid(img))
			Try(tkconfigure(ttMAPlotGraph,cursor="arrow"))
			Try(tkfocus(ttMAPlotGraph))
		}else{
			Try(plot.new())
			Try(plotFun())
		} #end of else/if (.limmaGUIglobals$graphicsDevice=="tkrplot")
	)
	#
	Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
	#
} #end of MAPlot <- function()


ChoosePlotSymbolByClicking <- function(spotType,cex)
{
  # This function should only be called if the tkrplot graphics device is being used.
  # limmaGUI has only recently offered the choice of the standard R graphics device as
  # an alternative to tkrplot, and the interactive plots haven't yet been modified to
  # work with R's locator() function.

  Try(ttChoosePlotSymbolByClicking<-tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.deiconify(ttChoosePlotSymbolByClicking))
  Try(tkgrab.set(ttChoosePlotSymbolByClicking))
  Try(tkfocus(ttChoosePlotSymbolByClicking)  )
  Try(tkwm.title(ttChoosePlotSymbolByClicking,
       paste("Please Choose A Plot Symbol and Size for Points of type \"",spotType,"\" in the Plot",sep="")))
  Try(parPlotSize <- c())

  Try(Pex <- cex)

  ChoosePlotSymbol <- function()
  {
      ##-------- Showing all the extra & some char graphics symbols ------------
      opar <- par (bg="white")
      ipch <- 1:(np <- 25+11); k <- floor(sqrt(np)); dd <- c(-1,1)/2
      rx <- dd + range(ix <- (ipch-1) %/% k)
      ry <- dd + range(iy <- 3 + (k-1)-(ipch-1) %% k)
      pch <- as.list(ipch)
      pch[25+ 1:11] <- as.list(c("*",".", "o","O","0","+","-",":","|","%","#"))
      Try(plot(rx, ry, type="n", axes=FALSE,xlim=c(-0.5,5.5),ylim=c(2.5,8.5),xaxs="i",yaxs="i",xlab = "", ylab = "",
          main = paste("Please choose a plot symbol and point size for spots of type \"",spotType,"\".",sep="")))
      Try(abline(v = ix, h = iy, col = "lightgray", lty = "dotted"))
      for(i in 1:np) {
       pc <- pch[[i]]
       Try(points(ix[i], iy[i], pch = pc, col = "red", bg = "yellow", cex = Pex))
       ## red symbols with a yellow interior (where available)
       Try(text(ix[i] - .3, iy[i], pc, col = "brown", cex = 1.2))
       parPlotSize <<- par("plt")
       par(opar)
      }
  }

  onUpdate <- function()
  {
    Try(Pex <<- as.numeric(tclvalue(PlotPointSize)))
    Try(tkrreplot(img))
  }

  Try(PlotPointSize <- tclVar(paste(Pex)))
  Try(entry.PlotPointSize <-tkentry(ttChoosePlotSymbolByClicking,width="20",textvariable=PlotPointSize,font=.limmaGUIglobals$limmaGUIfont2,bg="white"))
  Try(Update.but <- tkbutton(ttChoosePlotSymbolByClicking,text="Update",command=onUpdate,font=.limmaGUIglobals$limmaGUIfont2))
  Require("tkrplot")

  Try(LocalHScale <- .limmaGUIglobals$Myhscale)
  Try(LocalVScale <- .limmaGUIglobals$Myvscale)

  Try(img <- tkrplot(ttChoosePlotSymbolByClicking,fun=ChoosePlotSymbol,hscale=2*LocalHScale/1.6,vscale=1.6 *LocalVScale/1.6))
  Try(tkgrid(img,columnspan=3))
  Try(label1 <- tklabel(ttChoosePlotSymbolByClicking,text="Plot point size : ",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(label1,entry.PlotPointSize,Update.but))
  Try(tkgrid.configure(label1,sticky="e"))
  Try(tkbind(entry.PlotPointSize,"<Return>",onUpdate))
  Try(tkgrid(tklabel(ttChoosePlotSymbolByClicking,text="    ")))

  Try(tkconfigure(img,cursor="hand2"))
  ReturnVal <- list()
  LeftClick <- function(x,y) # x and y are the mouse coordinates
  {
   Try(onUpdate())
   Try(width  <- as.integer(tclvalue(tkwinfo("width",img))))
   Try(height <- as.integer(tclvalue(tkwinfo("height",img))))

   xMin <- parPlotSize[1] * width
   xMax <- parPlotSize[2] * width
   yMin <- (1-parPlotSize[4]) * height
   yMax <- (1-parPlotSize[3]) * height

   x <- as.numeric(x)
   y <- as.numeric(y)

   xInt <- floor((x-xMin)*6/(xMax-xMin))
   yInt <- floor((y-yMin)*6/(yMax-yMin))

   if (xInt < 0 || xInt>5) return()
   if (yInt < 0 || yInt>5) return()

   pchNum <- (yInt + 1 + 6*xInt)
   pch <- c(as.character(1:25),'*','.','o','O','0','+','-',':','|','%','#')

   mbVal<-tkmessageBox(title="Plot Symbol and Size Chosen",message=paste("Use plot symbol ",pch[pchNum]," with plot point size ",as.numeric(tclvalue(PlotPointSize))," for spots of type \"",spotType,"\"?",sep=""),type="okcancel",icon="question")
   if (tclvalue(mbVal)=="cancel")
       return()

   ### onOK ###
   Try(tkgrab.release(ttChoosePlotSymbolByClicking))
   Try(tkdestroy(ttChoosePlotSymbolByClicking))
   Try(tkfocus(.limmaGUIglobals$ttMain))
   Try(ReturnVal <<- list(pch=pch[pchNum],cex=as.numeric(tclvalue(PlotPointSize)),pchIsNumeric=(pch<=25)))
  }
  onCancel <- function() {tkgrab.release(ttChoosePlotSymbolByClicking); tkdestroy(ttChoosePlotSymbolByClicking); tkfocus(.limmaGUIglobals$ttMain); ReturnVal <- list() }

  Try(tkbind(img, "<Button-1>",LeftClick))
  Try(tkbind(img, "<Enter>",   onUpdate))
  Try(tkbind(ttChoosePlotSymbolByClicking, "<Destroy>", function() {tkgrab.release(ttChoosePlotSymbolByClicking); tkfocus(.limmaGUIglobals$ttMain);}))

  Cancel.but <- tkbutton(ttChoosePlotSymbolByClicking,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2)
  Try(tkgrid(tklabel(ttChoosePlotSymbolByClicking,text="    "),Cancel.but))
  Try(tkwait.window(ttChoosePlotSymbolByClicking))
  Try(tkfocus(.limmaGUIglobals$ttMain))
  return (ReturnVal)
}

SelectPlotSymbols <- function(SpotTypes)
{
  # SpotTypes contains attribute SpotType, a vector of strings, e.g. c("gene","calibration","ratio")

  Try(if (.limmaGUIglobals$limmaGUIpresentation==TRUE)
    blankLabelText <- "  "
  else
    blankLabelText <- "    ")

  Try(ttSelectPlotSymbolsDialog<-tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.deiconify(ttSelectPlotSymbolsDialog))
  Try(tkgrab.set(ttSelectPlotSymbolsDialog))
  Try(tkfocus(ttSelectPlotSymbolsDialog))
  Try(tkwm.title(ttSelectPlotSymbolsDialog,"Plot point types"))
  Try(tkgrid(tklabel(ttSelectPlotSymbolsDialog,text=blankLabelText)))

  Try(PointTypes <- SpotTypes$SpotType)
  Try(PointColors <- SpotTypes$Color)
  Try(if ("PointSize"  %in% colnames(SpotTypes))
    PointSizes <- SpotTypes$PointSize
  else
    PointSizes <- c())
  Try(numPointTypes <- length(PointTypes))

  #label2 and 3 were separated onto 2 lines for Presentation.  See if they still look OK in normal font sizes.
  Try(label1 <- tklabel(ttSelectPlotSymbolsDialog,text="If desired, you may adjust the symbols, sizes and colors used for plot points.",font=.limmaGUIglobals$limmaGUIfont2))
  Try(if (.limmaGUIglobals$limmaGUIpresentation==FALSE && numPointTypes>10) # scrollable frame tends not to be wide enough.
  {
    Try(label2 <- tklabel(ttSelectPlotSymbolsDialog,text="The plot point symbol can be a character typed from the keyboard or a special symbol selected using the Browse button.",font=.limmaGUIglobals$limmaGUIfont2))
    Try(label3 <- tklabel(ttSelectPlotSymbolsDialog,text="    ",font=.limmaGUIglobals$limmaGUIfont2))
  }
  else
  {
    Try(label2 <- tklabel(ttSelectPlotSymbolsDialog,text="The plot point symbol can be a character typed from the keyboard or ",font=.limmaGUIglobals$limmaGUIfont2))
    Try(label3 <- tklabel(ttSelectPlotSymbolsDialog,text="a special symbol selected using the Browse button.",font=.limmaGUIglobals$limmaGUIfont2))
  })
  Try(tkgrid(label1,columnspan=2))
  Try(tkgrid(label2,columnspan=2))
  Try(tkgrid(label3,columnspan=2))
  Try(tkgrid(tklabel(ttSelectPlotSymbolsDialog,text=blankLabelText)))

  pchIsNumeric <- rep(TRUE,numPointTypes)

  PointTypeTcl <- list()
  for (i in (1:numPointTypes))
      Try(PointTypeTcl[[i]] <- tclVar(PointTypes[i]))

  textVariable.pch <- list()
  for (i in (1:numPointTypes))
      Try(textVariable.pch[[i]] <- tclVar("16"))

  textVariable.cex <- list()
  Try(for (i in (1:numPointTypes))
  {
    Try(if (length(PointSizes)>0)
      Try(textVariable.cex[[i]] <- tclVar(PointSizes[i]))
    else
      Try(if (tolower(PointTypes[i]) =="gene"||PointTypes[i]=="cDNA")
      {
          Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows")
           Try(textVariable.cex[[i]] <- tclVar("0.3"))
          else
            Try(textVariable.cex[[i]] <- tclVar("0.1")))
      }
      else
      {
          Try(textVariable.cex[[i]] <- tclVar("0.6"))
      }))
  })

  textVariable.color <- list()
  for (i in (1:numPointTypes))
      Try(textVariable.color[[i]] <- tclVar(
      rgb(col2rgb(PointColors[i])["red",],col2rgb(PointColors[i])["green",],col2rgb(PointColors[i])["blue",],maxColorValue=255)
        ))

  Try(if (numPointTypes>10)
  {
    TclRequire("BWidget")
    Try(sw <- tkwidget(ttSelectPlotSymbolsDialog,"ScrolledWindow",relief="sunken",borderwidth=2))
    Try(sf <- tkwidget(sw,"ScrollableFrame"))
    Try(tcl(sw,"setwidget",sf))
    Try(subfID <- tclvalue(tcl(sf,"getframe")))
  }
  else
  {
    Try(sw <- tkframe(ttSelectPlotSymbolsDialog,borderwidth=2))
    Try(subfID <- .Tk.ID(sw))
  })

  entry.pch <- list()
  for (i in (1:numPointTypes))
      Try(entry.pch[[i]] <- tcl("entry",paste(subfID,".pch",i,sep=""),width="10",font=.limmaGUIglobals$limmaGUIfont2,textvariable=textVariable.pch[[i]]))

  entry.cex <- list()
  for (i in (1:numPointTypes))
      Try(entry.cex[[i]] <- tcl("entry",paste(subfID,".cex",i,sep=""),width="10",font=.limmaGUIglobals$limmaGUIfont2,textvariable=textVariable.cex[[i]]))

  canvas.color <- list()
  for (i in (1:numPointTypes))
      Try(canvas.color[[i]] <- tcl("canvas",paste(subfID,".canvas",i,sep=""),width="80",height="25",bg=tclvalue(textVariable.color[[i]])))

  onBrowseColor <- function(indexPointType)
  {
      Try(color <- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=tclvalue(textVariable.color[[indexPointType]]),title="Choose a color")))))
      if (nchar(color)==0)
          return()
      Try(tclvalue(textVariable.color[[indexPointType]]) <- color)
      Try(tcl(paste(subfID,".canvas",indexPointType,sep=""),"configure",bg=color))
  }

  onBrowse <- function(indexPointType)
  {
      Try(if (.limmaGUIglobals$graphicsDevice!="tkrplot")
      {
        Try(tkmessageBox(title="Browse unavailable",message="This feature is only available when using the Tk R Plot graphics device.",
          icon="error"))
        return()
      })
      Try(plotSymbolAndSize <- ChoosePlotSymbolByClicking(PointTypes[indexPointType],
                               as.numeric(tclvalue(textVariable.cex[[indexPointType]]))))
      if (length(plotSymbolAndSize)==0)
          return()
      Try(tclvalue(textVariable.pch[[indexPointType]]) <- paste(plotSymbolAndSize$pch))
      Try(tclvalue(textVariable.cex[[indexPointType]]) <- paste(plotSymbolAndSize$cex))
      Try(pchIsNumeric[indexPointType] <- plotSymbolAndSize$pchIsNumeric)
  }

  for (i in (1:numPointTypes))
      eval(parse(text=paste("onBrowse",i," <- function() {onBrowse(",i,",)}",sep="")))

  for (i in (1:numPointTypes))
      eval(parse(text=paste("onBrowseColor",i," <- function() {onBrowseColor(",i,",)}",sep="")))

  button.browse <- list()
  for (i in (1:numPointTypes))
      Try(button.browse[[i]] <- tcl("button",paste(subfID,".browse",i,sep=""),text="Browse",command=eval(parse(text=paste("onBrowse",i,sep=""))),font=.limmaGUIglobals$limmaGUIfont2))

  button.browse.color <- list()
  for (i in (1:numPointTypes))
      Try(button.browse.color[[i]] <- tcl("button",paste(subfID,".browsecol",i,sep=""),text="Browse",command=eval(parse(text=paste("onBrowseColor",i,sep=""))),font=.limmaGUIglobals$limmaGUIfont2))

  Try(tkgrid(tcl("label",paste(subfID,".lab",1,sep=""),text=blankLabelText),
         tcl("label",paste(subfID,".lab",2,sep=""),text="Spot Type ",font=.limmaGUIglobals$limmaGUIfont2),
         tcl("label",paste(subfID,".lab",3,sep=""),text="Plot Point Symbol",font=.limmaGUIglobals$limmaGUIfont2),
         tcl("label",paste(subfID,".lab",4,sep=""),text="Plot Point Size  ",font=.limmaGUIglobals$limmaGUIfont2),
         tcl("label",paste(subfID,".lab",5,sep=""),text=blankLabelText),
         tcl("label",paste(subfID,".lab",6,sep=""),text=blankLabelText),
         tcl("label",paste(subfID,".lab",7,sep=""),text="Plot Point Color",font=.limmaGUIglobals$limmaGUIfont2),
         tcl("label",paste(subfID,".lab",8,sep=""),text=blankLabelText),
         tcl("label",paste(subfID,".lab",9,sep=""),text=blankLabelText)))

  tkgrid(tcl("label",paste(subfID,".lab",10,sep=""),text=blankLabelText))

  for (i in (1:numPointTypes))
  {
      Try(tkgrid(tcl("label",paste(subfID,".lab",11,"_",i,sep=""),text=blankLabelText),
             tcl("label",paste(subfID,".lab",12,"_",i,sep=""),text=PointTypes[i],font=.limmaGUIglobals$limmaGUIfont2,bg="white"),
             entry.pch[[i]],entry.cex[[i]],button.browse[[i]],
             tcl("label",paste(subfID,".lab",13,"_",i,sep=""),text=blankLabelText),canvas.color[[i]],
             button.browse.color[[i]],
             tcl("label",paste(subfID,".lab",14,"_",i,sep=""),text=blankLabelText)))
      Try(tkgrid.configure(button.browse[[i]],sticky="w"))
  }

  tkgrid(sw,columnspan=2,sticky="nsew")

  Try(legendCheckboxTcl <- tclVar("1"))
  Try(legendCheckbox <- tkcheckbutton(ttSelectPlotSymbolsDialog,variable=legendCheckboxTcl))
  Try(tkgrid(tklabel(ttSelectPlotSymbolsDialog,text=blankLabelText))  )
  Try(legendLabel <- tklabel(ttSelectPlotSymbolsDialog,text="Show Legend",font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(legendCheckbox,legendLabel))
  Try(tkgrid.configure(legendCheckbox,sticky="e"))
  Try(tkgrid.configure(legendLabel,sticky="w"))
  Try(tkgrid(tklabel(ttSelectPlotSymbolsDialog,text=blankLabelText))  )

  ReturnVal <- list()
  onOK <- function()
  {
      Try(optVal <- options(warn=-1)) # Supress warning for testing if as.numeric(pch) gives NA.
      Try(ReturnVal <<- list())
      Try(for (i in (1:numPointTypes))
      {
          Try(if (is.na(as.numeric(tclvalue(textVariable.pch[[i]]))))
              pchIsNumeric[i] <- FALSE
          else
              pchIsNumeric[i] <- TRUE)

          if (pchIsNumeric[i])
              Try(listToAdd <- list(pch=as.numeric(tclvalue(textVariable.pch[[i]])),
                                                 cex=as.numeric(tclvalue(textVariable.cex[[i]])),
                                                          col=tclvalue(textVariable.color[[i]]),
                                                          pchIsNumeric=pchIsNumeric[i]))
          else
              Try(listToAdd <- list(pch=tclvalue(textVariable.pch[[i]]),
                                                 cex=as.numeric(tclvalue(textVariable.cex[[i]])),
                                                 col=tclvalue(textVariable.color[[i]]),
                                                 pchIsNumeric=pchIsNumeric[i]))
          Try(ReturnVal[[i]] <<- listToAdd)
      })
      Try(if (tclvalue(legendCheckboxTcl)=="1")
        showLegend <- TRUE
      else
        showLegend <- FALSE)
      Try(ReturnVal <<- list(PlotSymbols=ReturnVal,showLegend=showLegend))
      Try(tkgrab.release(ttSelectPlotSymbolsDialog))
      Try(tkdestroy(ttSelectPlotSymbolsDialog))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      Try(options(optVal))
  }
  Try(onCancel <- function() {Try(tkgrab.release(ttSelectPlotSymbolsDialog));Try(tkdestroy(ttSelectPlotSymbolsDialog));Try(tkfocus(.limmaGUIglobals$ttMain));ReturnVal <<- list()})
  Try(OK.but <-tkbutton(ttSelectPlotSymbolsDialog,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <-tkbutton(ttSelectPlotSymbolsDialog,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttSelectPlotSymbolsDialog,text=blankLabelText))  )
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(ttSelectPlotSymbolsDialog,text=blankLabelText)))
  Try(tkfocus(ttSelectPlotSymbolsDialog))
  Try(tkbind(ttSelectPlotSymbolsDialog, "<Destroy>", function() {tkgrab.release(ttSelectPlotSymbolsDialog); tkfocus(.limmaGUIglobals$ttMain);}))
  Try(tkwait.window(ttSelectPlotSymbolsDialog))

  return (ReturnVal)
}

plotMAColorCoded <- function()
{
  Try(SlideNamesVec <- get("SlideNamesVec",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
  Try(SpotTypes     <- get("SpotTypes", envir=limmaGUIenvironment))

  Try(gal <- get("gal",envir=limmaGUIenvironment))
  Try(WeightingType <- get("WeightingType",envir=limmaGUIenvironment))
  Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))

  if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      Try(tkmessageBox(title="Color-Coded M A Plot",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  Try(MA.Available <- get("MA.Available",envir=limmaGUIenvironment))
  Try(if (NormalizedMADataWasImported==FALSE)
  {
    Try(RG <- get("RG",envir=limmaGUIenvironment))
    Try(MAraw <- get("MAraw",envir=limmaGUIenvironment))
  }
  else
  {
    Try(MA <- get("MAimported",envir=limmaGUIenvironment))
  })

  if (nrow(SpotTypes)==0)
  {
      Try(tkmessageBox(title="Spot Types",message="No spot types have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  Try(slidenum <- GetSlideNum())
  if (slidenum==0)
      return()

  Try(PlotSymbols <- SelectPlotSymbols(SpotTypes))
  if (length(PlotSymbols)==0)
      return()
  Try(showLegend  <- PlotSymbols$showLegend)
  Try(PlotSymbols <- PlotSymbols$PlotSymbols)

  Try(SpotTypeStatus <- get("SpotTypeStatus", envir=limmaGUIenvironment))
  Try(numSpotTypes <- nrow(SpotTypes))

  Try(pchAllNumeric <- TRUE)
  Try(pchAllCharacter <- TRUE)
  for (i in (1:numSpotTypes))
  {
      Try(if (PlotSymbols[[i]]$pchIsNumeric==TRUE)
          Try(PlotSymbols[[i]]$pch <- as.numeric(PlotSymbols[[i]]$pch)))
      Try(pchAllNumeric <- (pchAllNumeric && PlotSymbols[[i]]$pchIsNumeric))
      Try(pchAllCharacter <- (pchAllCharacter && (!PlotSymbols[[i]]$pchIsNumeric)))
  }

  Try(cex <- c())
  Try(col <- c())
  Try(if (pchAllNumeric || pchAllCharacter)
      Try(pch <- c())
  else
      Try(pch <- list()))

  for (i in (1:numSpotTypes))
  {
  # Some of the i's below have been changed from numSpotTypes-i+1
      Try(cex[i] <- PlotSymbols[[i]]$cex)
      Try(col[i] <- PlotSymbols[[i]]$col)
      Try(if (pchAllNumeric || pchAllCharacter)
          Try(pch[i] <- PlotSymbols[[i]]$pch)
      else
          Try(pch[[i]] <- PlotSymbols[[i]]$pch))
  }
#  Try(values <- rev(SpotTypes$SpotType))
  Try(values <- SpotTypes$SpotType)

# For debugging
  Try(assign("values",values,limmaGUIenvironment))
  Try(assign("pch",pch,limmaGUIenvironment))
  Try(assign("cex",cex,limmaGUIenvironment))
  Try(assign("col",col,limmaGUIenvironment))

  Try(if (NormalizedMADataWasImported==FALSE)
  {
		Try(NormalizeWithinArraysMB <-tkmessageBox(title="Normalization Within Arrays",message="Normalize Within Arrays?",type="yesnocancel",icon="question",default="no"))
		Try(WhetherToNormalizeWithinArrays <- tclvalue(NormalizeWithinArraysMB))
		if (WhetherToNormalizeWithinArrays=="cancel")
				return()

		Try(NormalizeBetweenArraysMB <-tkmessageBox(title="Normalization Between Arrays",message="Normalize Between Arrays?",type="yesnocancel",icon="question",default="no"))
		Try(WhetherToNormalizeBetweenArrays <- tclvalue(NormalizeBetweenArraysMB))
		if (WhetherToNormalizeBetweenArrays=="cancel")
				return()
	})

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

  Try(if (NormalizedMADataWasImported==FALSE)
  {
		Try(if (!exists("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))
		{
			Try(WithinArrayNormalizationMethod <- "printtiploess")
			Try(assign("WithinArrayNormalizationMethod",WithinArrayNormalizationMethod,limmaGUIenvironment))
		})
		Try(WithinArrayNormalizationMethod <- get("WithinArrayNormalizationMethod",envir=limmaGUIenvironment))

		SetLayoutParamReturnVal<-1
		Try(if (WhetherToNormalizeWithinArrays=="yes")
		{
				if (length(maLayout)==0) SetLayoutParamReturnVal <-Try(SetLayoutParameters())
				if (SetLayoutParamReturnVal==0) return()
				Try(maLayout <- get("maLayout",envir=limmaGUIenvironment))
				if (MA.Available$WithinArrays)
					Try(MA <- get("MAwithinArrays",envir=limmaGUIenvironment))
				else
				{
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
		}
		else
		{
				if (MA.Available$Raw)
					Try(MA <- get("MAraw",envir=limmaGUIenvironment))
				else
				{
					Try (MA <- MA.RG(RG))
					Try(assign("MAraw",MA,limmaGUIenvironment))
					Try(MA.Available$Raw <- TRUE)
					Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
					Try(tkdelete(.limmaGUIglobals$mainTree,"Raw.Status"))
					Try(tkinsert(.limmaGUIglobals$mainTree,"end","Raw","Raw.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
				}
		})

		Try(if (WhetherToNormalizeBetweenArrays=="yes")
		{
			if (WhetherToNormalizeWithinArrays=="yes")
			{
				if (MA.Available$Both)
					Try(MA <- get("MAboth",envir=limmaGUIenvironment))
				else
				{
					Try (MA <- normalizeBetweenArrays(MA))
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
					Try(MA <- get("MAbetweenArrays",envir=limmaGUIenvironment))
				else
				{
					Try (MA <- normalizeBetweenArrays(MA))
					Try(assign("MAbetweenArrays",MA,limmaGUIenvironment))
					Try(MA.Available$BetweenArrays <- TRUE)
					Try(assign("MA.Available",MA.Available,limmaGUIenvironment))
					Try(tkdelete(.limmaGUIglobals$mainTree,"BetweenOnly.Status"))
					Try(tkinsert(.limmaGUIglobals$mainTree,"end","BetweenOnly","BetweenOnly.Status" ,text="Available",font=.limmaGUIglobals$limmaGUIfontTree))
				}
			}
		})
  })

  Try(if (min(nchar(gsub("[^0-9]","",SlideNamesVec))==nchar(SlideNamesVec))==TRUE)
     SlideNamesVec <- paste("Slide",SlideNamesVec))
  Try(plotTitle <- paste("M A Plot for ",SlideNamesVec[slidenum],sep=""))
  Try(if (NormalizedMADataWasImported==FALSE)
  {
		Try(if (WhetherToNormalizeWithinArrays=="no"&&WhetherToNormalizeBetweenArrays=="no")
			Try(plotTitle <- paste(plotTitle,"with no normalization")))
		Try(if (WhetherToNormalizeWithinArrays=="yes"&&WhetherToNormalizeBetweenArrays=="no")
			Try(plotTitle <- paste(plotTitle,"with normalization within arrays only")))
		Try(if (WhetherToNormalizeWithinArrays=="no"&&WhetherToNormalizeBetweenArrays=="yes")
			Try(plotTitle <- paste(plotTitle,"with normalization between arrays only")))
		Try(if (WhetherToNormalizeWithinArrays=="yes"&&WhetherToNormalizeBetweenArrays=="yes")
			Try(plotTitle <- paste(plotTitle,"with normalization within and between arrays only")))
  })
  Try(plotLabels <- GetPlotLabels(plotTitle,"A","M"))
  Try(if (length(plotLabels)==0) return())
  Try(plotTitle <- plotLabels$plotTitle)
  Try(xLabel    <- plotLabels$xLabel)
  Try(yLabel    <- plotLabels$yLabel)

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

  Try(xlim <- c(min(MA$A[,slidenum],na.rm=TRUE),max(MA$A[,slidenum],na.rm=TRUE))) # For default x limits.

  plotMA0 <- function()
  {
     Try(opar<-par(bg="white"))
     Try(plotMA(MA,pch=pch,cex=cex,array=slidenum,status=SpotTypeStatus,values=values,
         col=col,xlab=xLabel,ylab=yLabel,legend=showLegend,xlim=xlim,main=""))
     Try(title(plotTitle))
     Try(tempGraphPar <- par(opar))
  }
  Try(LocalHScale <- .limmaGUIglobals$Myhscale)
  Try(LocalVScale <- .limmaGUIglobals$Myvscale)

#  Try(LocalHScale <- LocalHScale * 1.25)


  Try(if (.limmaGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttplotMAColorCodedGraph <- tktoplevel(.limmaGUIglobals$ttMain))
    Try(Require("tkrplot"))
    Try(tkwm.title(ttplotMAColorCodedGraph,plotTitle))
    Try(img <-tkrplot(ttplotMAColorCodedGraph,plotMA0,hscale=LocalHScale,vscale=LocalVScale) )
    Try(SetupPlotKeyBindings(tt=ttplotMAColorCodedGraph,img=img))
    Try(plotMenus<-SetupPlotMenus(tt=ttplotMAColorCodedGraph,initialfile=paste(limmaDataSetNameText,"plotMAColorCodedSlide",SlideNamesVec[slidenum],sep=""),
                 plotFunction=plotMA0,img=img))
    Try(resizeMenu<-plotMenus$resizeMenu)
    Try(tkadd(resizeMenu, "command", label="Resize Horizontal (A) Axis",command=function() {Try(GetNEWxlimReturnVal<-GetNEWxlim(xlim));if (length(GetNEWxlimReturnVal)==0) return() else xlim <<- GetNEWxlimReturnVal;Try(tkconfigure(img,cursor="watch"));Try(tkrreplot(img,fun=plotMA0,hscale=LocalHScale,vscale=LocalVScale));Try(tkconfigure(img,cursor="arrow"))}))
    Try(tkgrid(img))
    Try(tkfocus(ttplotMAColorCodedGraph))
  }
  else
  {
    Try(plot.new())
    Try(plotMA0())
  })
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
}

GetNEWxlim <- function(xlim)
{
  ttGetNEWxlim<-tktoplevel(.limmaGUIglobals$ttMain)
  tkwm.deiconify(ttGetNEWxlim)
  tkgrab.set(ttGetNEWxlim)
  tkfocus(ttGetNEWxlim)
  tkwm.title(ttGetNEWxlim,"Horizontal axis limits")
  tkgrid(tklabel(ttGetNEWxlim,text="    "))
  Try(xminTcl <- tclVar(init=format(xlim[1],digits=4)))
  Try(entry.xmin<-tkentry(ttGetNEWxlim,width="40",font=.limmaGUIglobals$limmaGUIfont2,textvariable=xminTcl,bg="white"))
  Try(tkgrid(tklabel(ttGetNEWxlim,text="X Axis Lower Limit : ",font=.limmaGUIglobals$limmaGUIfont2),entry.xmin))
  Try(tkgrid(tklabel(ttGetNEWxlim,text="    ")))
  Try(xmaxTcl <- tclVar(init=format(xlim[2],digits=4)))
  entry.xmax<-tkentry(ttGetNEWxlim,width="40",font=.limmaGUIglobals$limmaGUIfont2,textvariable=xmaxTcl,bg="white")
  tkgrid(tklabel(ttGetNEWxlim,text="X Axis Upper Limit :   ",font=.limmaGUIglobals$limmaGUIfont2),entry.xmax)
  tkgrid(tklabel(ttGetNEWxlim,text="    "))

  tkgrid.configure(entry.xmin,entry.xmax,columnspan=2)
  ReturnVal <- c()
  onOK <- function()
  {
      xmin <- as.numeric(tclvalue(xminTcl))
      xmax <- as.numeric(tclvalue(xmaxTcl))
      Try(tkgrab.release(ttGetNEWxlim))
      Try(tkdestroy(ttGetNEWxlim))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      ReturnVal <<- c(xmin,xmax)
  }
  onCancel <- function() {Try(tkgrab.release(ttGetNEWxlim));Try(tkdestroy(ttGetNEWxlim));Try(tkfocus(.limmaGUIglobals$ttMain));ReturnVal <<- c()}
  OK.but <-tkbutton(ttGetNEWxlim,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2)
  Cancel.but <-tkbutton(ttGetNEWxlim,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2)
  tkgrid(tklabel(ttGetNEWxlim,text="    "),OK.but,Cancel.but)
  tkgrid(tklabel(ttGetNEWxlim,text="    "))
  Try(tkbind(ttGetNEWxlim, "<Destroy>", function() {Try(tkgrab.release(ttGetNEWxlim));Try(tkfocus(.limmaGUIglobals$ttMain));}))
  Try(tkfocus(ttGetNEWxlim))
  Try(tkwait.window(ttGetNEWxlim))

  return (ReturnVal)
}


ebayesBoxPlots <- function()
{
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(NumParameters         <- get("NumParameters",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(gal <- get("gal",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))
  Try(SpotTypeStatus <- get("SpotTypeStatus",envir=limmaGUIenvironment))

  if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      tkmessageBox(title="Empirical Bayes Statistics Box Plots",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error")
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="Empirical Bayes Statistics Box Plots",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    return()
  }
  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return())
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])
  if (Try(LinearModelComputed[parameterizationIndex]==FALSE))
  {
      Try(tkmessageBox(title="Empirical Bayes Statistic Box Plot",message=paste("No linear model fit is available for ",ParameterizationNamesVec[parameterizationIndex],".  Please try Compute Linear Model Fit from the Linear Model menu.",sep=""),type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  Try(ParameterNamesVec  <- GetParameterNames(parameterizationTreeIndex))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(if ("genelist" %in% attributes(ParameterizationList[[ParameterizationNameNode]])$names)
    Try(genelist <- (ParameterizationList[[ParameterizationNameNode]])$genelist)
  else
    Try(genelist <- get("genelist",limmaGUIenvironment)))
  GetCoefReturnVal <- GetCoef(parameterizationTreeIndex)
  if (GetCoefReturnVal$coefIndex==0) return()
  Try(coef <- (GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]])$coefOrContrastIndex)
  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))
  Try(ContrastParameterizationIndex <- GetCoefReturnVal$coefIndexList[[GetCoefReturnVal$coefIndex]]$ContrastParameterizationIndex)
  if (GetCoefReturnVal$parameterIsFromMainFit)
    Try(eb  <- (ParameterizationList[[ParameterizationNameNode]])$eb)
  else
  {
    Try(eb  <- ((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex]])$eb)
    Try(ParameterNamesVec <- colnames(((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex]])$contrastsMatrixInList$contrasts))
    Try(NumParameters <- length(ParameterNamesVec))
  }

  ebayesStatistic <- ChooseEbayesStatistic()
  if (ebayesStatistic=="")
    return()

  SpotType <- ChooseSpotType(parameterizationTreeIndex)
  if (SpotType=="")
    return()

### Now check (using table) how many spot sub-types there are for that spot type, e.g.
### for spot type "Calibration", there might be sub types "Calibration Control 1", ... "Calibration Control 6"

  Try(ndups   <- get("ndups",envir=limmaGUIenvironment))
  Try(spacing   <- get("spacing",envir=limmaGUIenvironment))       # Global version
  Try(spacing <- GetReducedDuplicateSpacing(parameterizationTreeIndex))
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

  if (numMatches>50)
    Try(tkmessageBox(title="Ebayes Box Plots",message=paste("There are ",numMatches," sub-types of spot type \"",SpotType,
      "\".  Because there are so many sub-types for this spot type, only one overall box plot will be shown, rather ",
      "than plotting individual box plots for each sub-type.",sep="")))

  if (numMatches==0)
  {
    Try(tkmessageBox(title="Ebayes Box Plots",message=paste("No spots were found with spot type \"",SpotType,
      "\".",sep="")))
    return()
  }

  Try(if (NumParameters>1)
    ebayesStatisticsVector <- eb[[ebayesStatistic]][,coef]
  else
    ebayesStatisticsVector <- eb[[ebayesStatistic]])

  Try(plotTitle <- "")
  Try(if (ebayesStatistic=="t")
  {
    plotTitle <- paste("t Statistic Box Plot(s) for Spot Type",SpotType)
    ylabel <- "t Statistic"
  })
  Try(if (ebayesStatistic=="lods")
  {
    plotTitle <- paste("B Statistic Box Plot(s) for Spot Type",SpotType)
    ylabel <- "B Statistic"
  })
  Try(if (ebayesStatistic=="p.value")
  {
    plotTitle <- paste("P Value Box Plot(s) for Spot Type",SpotType)
    ylabel <- "P Value"
  })

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))
  Try(tkfocus(.limmaGUIglobals$ttMain))
  Try(plotTitleList <- GetPlotTitle(plotTitle))
  Try(if (length(plotTitleList)==0) return())
  Try(plotTitle <- plotTitleList$plotTitle)
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

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
      Try(plotCommand <- paste(plotCommand,"ylab=\"",ylabel,"\");title(plotTitle)",sep=""))
    else
      Try(plotCommand <- paste(plotCommand,"xlab=\"",SpotType,"\",","ylab=\"",ylabel,"\");title(plotTitle)",sep="")))
  }
  else
  {
    Try(plotCommand <- "boxplot(")
    Try(plotCommand <- paste(plotCommand,"ebayesStatisticsVector[genelist2[,\"SpotTypeStatus\"]==\"",SpotType,"\"],",sep=""))
    Try(plotCommand <- paste(plotCommand,"xlab=\"",SpotType,"\",","names=\"",SpotType,"\",",sep=""))
    Try(plotCommand <- paste(plotCommand,"ylab=\"",ylabel,"\");title(plotTitle)",sep=""))
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

  Try(LocalHScale <- .limmaGUIglobals$Myhscale)
  Try(LocalVScale <- .limmaGUIglobals$Myvscale)

  Try(if (.limmaGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttEbayesBoxPlot <- tktoplevel(.limmaGUIglobals$ttMain))
    Try(tkwm.title(ttEbayesBoxPlot,plotTitle))
    Try(Require("tkrplot"))
    Try(img <-tkrplot(ttEbayesBoxPlot,plotEbayesBoxPlot,hscale=LocalHScale,vscale=LocalVScale) )
    Try(SetupPlotKeyBindings(tt=ttEbayesBoxPlot,img=img))
    Try(SetupPlotMenus(tt=ttEbayesBoxPlot,initialfile=paste(limmaDataSetNameText,"EBayesBoxPlot",ParameterNamesVec[coef],sep=""),
                 plotFunction=plotEbayesBoxPlot,img=img))
    Try(tkgrid(img))
    Try(tkfocus(ttEbayesBoxPlot))
  }
  else
  {
    Try(plot.new())
    Try(plotEbayesBoxPlot())
  })
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))

}

GetPlotLabels <- function(plottitle="",xlabel="",ylabel="")
{
  ttGetPlotLabels<-tktoplevel(.limmaGUIglobals$ttMain)
  tkwm.deiconify(ttGetPlotLabels)
  tkgrab.set(ttGetPlotLabels)
  tkfocus(ttGetPlotLabels)
  tkwm.title(ttGetPlotLabels,"Plot title and axis labels")
  tkgrid(tklabel(ttGetPlotLabels,text="    "))
  TitleTcl <- tclVar(init=plottitle)
  entry.Title<-tkentry(ttGetPlotLabels,width="40",font=.limmaGUIglobals$limmaGUIfont2,textvariable=TitleTcl,bg="white")
  tkgrid(tklabel(ttGetPlotLabels,text="Plot Title : ",font=.limmaGUIglobals$limmaGUIfont2),entry.Title)
  tkgrid(tklabel(ttGetPlotLabels,text="    "))
  xLabelTcl <- tclVar(init=xlabel)
  entry.xLabel<-tkentry(ttGetPlotLabels,width="40",font=.limmaGUIglobals$limmaGUIfont2,textvariable=xLabelTcl,bg="white")
  tkgrid(tklabel(ttGetPlotLabels,text="X Axis Label : ",font=.limmaGUIglobals$limmaGUIfont2),entry.xLabel)
  tkgrid(tklabel(ttGetPlotLabels,text="    "))
  yLabelTcl <- tclVar(init=ylabel)
  entry.yLabel<-tkentry(ttGetPlotLabels,width="40",font=.limmaGUIglobals$limmaGUIfont2,textvariable=yLabelTcl,bg="white")
  tkgrid(tklabel(ttGetPlotLabels,text="Y Axis Label :   ",font=.limmaGUIglobals$limmaGUIfont2),entry.yLabel)
  tkgrid(tklabel(ttGetPlotLabels,text="    "))

  tkgrid.configure(entry.Title,entry.xLabel,entry.yLabel,columnspan=2)
  ReturnVal <- list()
  onOK <- function()
  {
      plotTitle <- tclvalue(TitleTcl)
      xLabel <- tclvalue(xLabelTcl)
      yLabel <- tclvalue(yLabelTcl)
      Try(tkgrab.release(ttGetPlotLabels))
      Try(tkdestroy(ttGetPlotLabels))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      ReturnVal <<- list(plotTitle=plotTitle,xLabel=xLabel,yLabel=yLabel)
  }
  onCancel <- function() {Try(tkgrab.release(ttGetPlotLabels));Try(tkdestroy(ttGetPlotLabels));Try(tkfocus(.limmaGUIglobals$ttMain));ReturnVal <<- list()}
  OK.but <-tkbutton(ttGetPlotLabels,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2)
  Cancel.but <-tkbutton(ttGetPlotLabels,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2)
  tkgrid(tklabel(ttGetPlotLabels,text="    "),OK.but,Cancel.but)
  tkgrid(tklabel(ttGetPlotLabels,text="    "))
  Try(tkbind(ttGetPlotLabels, "<Destroy>", function() {Try(tkgrab.release(ttGetPlotLabels));Try(tkfocus(.limmaGUIglobals$ttMain));}))
  Try(tkfocus(ttGetPlotLabels))
  Try(tkwait.window(ttGetPlotLabels))

  return (ReturnVal)
}

GetPlotTitle <- function(plottitle="")
{
  ttGetPlotTitle<-tktoplevel(.limmaGUIglobals$ttMain)
  tkwm.deiconify(ttGetPlotTitle)
  tkgrab.set(ttGetPlotTitle)
  tkfocus(ttGetPlotTitle)
  tkwm.title(ttGetPlotTitle,"Plot title")
  tkgrid(tklabel(ttGetPlotTitle,text="    "))
  TitleTcl <- tclVar(init=plottitle)
  entry.Title<-tkentry(ttGetPlotTitle,width="50",font=.limmaGUIglobals$limmaGUIfont2,textvariable=TitleTcl,bg="white")
  tkgrid(tklabel(ttGetPlotTitle,text="Plot Title : ",font=.limmaGUIglobals$limmaGUIfont2),entry.Title)
  tkgrid(tklabel(ttGetPlotTitle,text="    "))

  tkgrid.configure(entry.Title,columnspan=2)
  ReturnVal <- list()
  onOK <- function()
  {
      plotTitle <- tclvalue(TitleTcl)
      Try(tkgrab.release(ttGetPlotTitle))
      Try(tkdestroy(ttGetPlotTitle))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      ReturnVal <<- list(plotTitle=plotTitle)
  }
  onCancel <- function() {Try(tkgrab.release(ttGetPlotTitle));Try(tkdestroy(ttGetPlotTitle));Try(tkfocus(.limmaGUIglobals$ttMain));ReturnVal <<- list()}
  OK.but <-tkbutton(ttGetPlotTitle,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2)
  Cancel.but <-tkbutton(ttGetPlotTitle,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2)
  tkgrid(tklabel(ttGetPlotTitle,text="    "),OK.but,Cancel.but)
  tkgrid(tklabel(ttGetPlotTitle,text="    "))
  Try(tkbind(entry.Title, "<Return>",onOK))
  Try(tkbind(ttGetPlotTitle, "<Destroy>", function() {Try(tkgrab.release(ttGetPlotTitle));Try(tkfocus(.limmaGUIglobals$ttMain));}))
  Try(tkfocus(ttGetPlotTitle))
  Try(tkwait.window(ttGetPlotTitle))

  return (ReturnVal)
}


GetPlotSize <- function()
{
  Try(Myhscale <- .limmaGUIglobals$Myhscale)
  Try(Myvscale <- .limmaGUIglobals$Myvscale)
  ttGetPlotSize<-tktoplevel(.limmaGUIglobals$ttMain)
  tkwm.deiconify(ttGetPlotSize)
  tkgrab.set(ttGetPlotSize)
  tkfocus(ttGetPlotSize)
  tkwm.title(ttGetPlotSize,"Plot size")
  tkgrid(tklabel(ttGetPlotSize,text="    "))
  tkgrid(tklabel(ttGetPlotSize,text="If desired, you may adjust the horizontal and vertical size of the plot.",font=.limmaGUIglobals$limmaGUIfont2),columnspan=2)
  tkgrid(tklabel(ttGetPlotSize,text="    "))
  HScaleTcl <- tclVar(paste(Myhscale))
  entry.HScale<-tkentry(ttGetPlotSize,width="20",font=.limmaGUIglobals$limmaGUIfont2,textvariable=HScaleTcl,bg="white")
  tkgrid(tklabel(ttGetPlotSize,text="Horizontal Scaling Factor : ",font=.limmaGUIglobals$limmaGUIfont2),entry.HScale,sticky="w")
  tkgrid(tklabel(ttGetPlotSize,text="    "))
  VScaleTcl <- tclVar(paste(Myvscale))
  entry.VScale<-tkentry(ttGetPlotSize,width="20",font=.limmaGUIglobals$limmaGUIfont2,textvariable=VScaleTcl,bg="white")
  tkgrid(tklabel(ttGetPlotSize,text="Vertical Scaling Factor :   ",font=.limmaGUIglobals$limmaGUIfont2),entry.VScale,sticky="w")
  tkgrid(tklabel(ttGetPlotSize,text="    "))
  ReturnVal <- 0
  HScale <- 0
  VScale <- 0
  onOK <- function()
  {
      HScale <<- as.numeric(tclvalue(HScaleTcl))
      VScale <<- as.numeric(tclvalue(VScaleTcl))
      Try(tkgrab.release(ttGetPlotSize))
      Try(tkdestroy(ttGetPlotSize))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      ReturnVal <<- list(HScale=HScale,VScale=VScale)
  }
  onCancel <- function() {Try(tkgrab.release(ttGetPlotSize));Try(tkdestroy(ttGetPlotSize));Try(tkfocus(.limmaGUIglobals$ttMain));ReturnVal <<- list()}
  OK.but <-tkbutton(ttGetPlotSize,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2)
  Cancel.but <-tkbutton(ttGetPlotSize,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2)
  tkgrid(OK.but,Cancel.but)
  tkgrid(tklabel(ttGetPlotSize,text="    "))
  Try(tkfocus(ttGetPlotSize))
  Try(tkbind(ttGetPlotSize, "<Destroy>", function() {Try(tkgrab.release(ttGetPlotSize));Try(tkfocus(.limmaGUIglobals$ttMain));}))
  Try(tkbind(entry.HScale, "<Return>",function() tkfocus(entry.VScale)))
  Try(tkbind(entry.VScale, "<Return>", onOK))
  Try(tkwait.window(ttGetPlotSize))

  return (ReturnVal)
}

SaveGraphAsJpeg <- function(initialfile,plotFunction)
{
  Try(jpegFileName <- tclvalue(tkgetSaveFile(initialfile=initialfile,filetypes="{{JPEG Files} {.jpg .jpeg}} {{All files} *}"))  )
  if (!nchar(jpegFileName))
    return()
  Try(len <- nchar(jpegFileName))
  if (len<4)
      Try(jpegFileName <- paste(jpegFileName,".jpg",sep=""))
  else if   ((tolower(substring(jpegFileName,len-3,len))!=".jpg") &&
  (len<5 || (tolower(substring(jpegFileName,len-4,len))!=".jpeg")))
        Try(jpegFileName <- paste(jpegFileName,".jpg",sep=""))

  Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
  {
    Try(jpegParams <- GetJpegOrPngX11Params(graphFileType="JPEG"))
    Try(bitmap(file=jpegFileName,bg=jpegParams$bg,res=jpegParams$res,type="jpeg"))
  }
  else
  {
    Try(jpegParams <- GetJpegOrPngParams(graphFileType="JPEG"))
    if (length(jpegParams)==0) return()
    Try(jpeg(file=jpegFileName,width=jpegParams$width,height=jpegParams$height,pointsize=jpegParams$pointsize,bg=jpegParams$bg))
  })
  Try(plotFunction())
  Try(dev.off())
}

SaveGraphAsPNG <- function(initialfile,plotFunction)
{
  Try(pngFileName <- tclvalue(tkgetSaveFile(initialfile=initialfile,filetypes="{{PNG Files} {.png}} {{All files} *}"))  )
  if (!nchar(pngFileName))
    return()
  Try(len <- nchar(pngFileName))
  if (len<4)
      Try(pngFileName <- paste(pngFileName,".png",sep=""))
  else if   ((tolower(substring(pngFileName,len-3,len))!=".png"))
        Try(pngFileName <- paste(pngFileName,".png",sep=""))

  Try(if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")
  {
    Try(pngParams <- GetJpegOrPngX11Params(graphFileType="PNG"))
    Try(bitmap(file=pngFileName,bg=pngParams$bg,res=pngParams$res))
  }
  else
  {
    Try(pngParams <- GetJpegOrPngParams(graphFileType="PNG"))
    if (length(pngParams)==0) return()
    Try(png(file=pngFileName,width=pngParams$width,height=pngParams$height,pointsize=pngParams$pointsize,bg=pngParams$bg))
  })
  Try(plotFunction())
  Try(dev.off())
}

SaveGraphAsPostscript <- function(initialfile,plotFunction)
{
  Try(psFileName <- tclvalue(tkgetSaveFile(initialfile=initialfile,filetypes="{{Postscript Files} {.ps .eps}} {{All files} *}"))  )
  if (!nchar(psFileName))
    return()
  Try(len <- nchar(psFileName))
  if (len<2)
      Try(psFileName <- paste(psFileName,".ps",sep=""))
  else if   ((tolower(substring(psFileName,len-2,len))!=".ps"))
        Try(psFileName <- paste(psFileName,".ps",sep=""))

  Try(postscript(file=psFileName,title=substring(psFileName,1,nchar(psFileName)-3)))
  Try(plotFunction())
  Try(dev.off())
}

SaveGraphAsPDF <- function(initialfile,plotFunction)
{
  Try(pdfFileName <- tclvalue(tkgetSaveFile(initialfile=initialfile,filetypes="{{PDF Files} {.pdf}} {{All files} *}"))  )
  if (!nchar(pdfFileName))
    return()
  Try(len <- nchar(pdfFileName))
  if (len<2)
      Try(pdfFileName <- paste(pdfFileName,".pdf",sep=""))
  else if   ((tolower(substring(pdfFileName,len-3,len))!=".pdf"))
        Try(pdfFileName <- paste(pdfFileName,".pdf",sep=""))

  Try(pdf(file=pdfFileName,title=substring(pdfFileName,1,nchar(pdfFileName)-4)))
  Try(plotFunction())
  Try(dev.off())
}


Resize <- function(img,plotFunction)
{
  Try(PlotSize <- GetPlotSize())
  Try(if (length(PlotSize)==0)      return())
  Try(LocalHScale <- PlotSize$HScale)
  Try(LocalVScale <- PlotSize$VScale)
  Try(tkconfigure(img,cursor="watch"))
  Try(tkfocus(img))
  Try(tkrreplot(img,fun=plotFunction,hscale=LocalHScale,vscale=LocalVScale))
  Try(tkconfigure(img,cursor="arrow"))
}

CopyGraph <- function(img) Try(tkrreplot(img))

SetupPlotKeyBindings <- function(tt,img)
{
  Try(tkbind(tt, "<Control-C>", function() CopyGraph(img)))
  Try(tkbind(tt, "<Control-c>", function() CopyGraph(img)))
}

SetupPlotMenus <- function(tt,initialfile,plotFunction,img)
{
  Try(topMenu <- tkmenu(tt))
  Try(tkconfigure(tt, menu=topMenu))
  Try(fileMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(editMenu <- tkmenu(topMenu, tearoff=FALSE))
  Try(resizeMenu <- tkmenu(topMenu, tearoff=FALSE))

  Try(tkadd(fileMenu, "command", label="Save As PNG",command=function() SaveGraphAsPNG(initialfile=initialfile,plotFunction=plotFunction))) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(fileMenu, "command", label="Save As JPEG",command=function() SaveGraphAsJpeg(initialfile=initialfile,plotFunction=plotFunction))) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(fileMenu, "command", label="Save As Postscript",command=function() SaveGraphAsPostscript(initialfile=initialfile,plotFunction=plotFunction))) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(fileMenu, "command", label="Save As PDF",command=function() SaveGraphAsPDF(initialfile=initialfile,plotFunction=plotFunction))) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(fileMenu, "separator"))
  Try(tkadd(fileMenu, "command", label="Close",command=function() tkdestroy(tt))) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(topMenu, "cascade", label="File",menu=fileMenu)) # ) # ,font=limmaGUIfontMenu))

  Try(tkadd(editMenu, "command", label="Copy <Ctrl-C>",command=function() CopyGraph(img=img))) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(topMenu, "cascade", label="Edit", menu=editMenu)) # ) # ,font=limmaGUIfontMenu))

  Try(tkadd(resizeMenu, "command", label="Resize Window",command=function() Resize(img=img,plotFunction=plotFunction))) # ) # ,font=limmaGUIfontMenu))
  Try(tkadd(topMenu, "cascade", label="Resize", menu=resizeMenu)) # ) # ,font=limmaGUIfontMenu))
  return (list(topMenu=topMenu,fileMenu=fileMenu,editMenu=editMenu,resizeMenu=resizeMenu))
}

GetJpegOrPngParams <- function(graphFileType)
{
  ttGetJpegOrPngParams<-tktoplevel(.limmaGUIglobals$ttMain)
  tkwm.deiconify(ttGetJpegOrPngParams)
  tkgrab.set(ttGetJpegOrPngParams)
  tkfocus(ttGetJpegOrPngParams)
  tkwm.title(ttGetJpegOrPngParams,paste(graphFileType,"Image Parameters"))
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  tkgrid(tklabel(ttGetJpegOrPngParams,text=paste(graphFileType,"Image Parameters"),font=.limmaGUIglobals$limmaGUIfont2),columnspan=2)
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  WidthTcl <- tclVar(paste(600))
  entry.Width<-tkentry(ttGetJpegOrPngParams,width="10",font=.limmaGUIglobals$limmaGUIfont2,textvariable=WidthTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="Width   ",font=.limmaGUIglobals$limmaGUIfont2),entry.Width,tklabel(ttGetJpegOrPngParams,text="    "),sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  HeightTcl <- tclVar(paste(600))
  entry.Height<-tkentry(ttGetJpegOrPngParams,width="10",font=.limmaGUIglobals$limmaGUIfont2,textvariable=HeightTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="Height    ",font=.limmaGUIglobals$limmaGUIfont2),entry.Height,tklabel(ttGetJpegOrPngParams,text="    "),sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  BackgroundTcl <- tclVar("white")
  entry.Background<-tkentry(ttGetJpegOrPngParams,width="10",font=.limmaGUIglobals$limmaGUIfont2,textvariable=BackgroundTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="Background    ",font=.limmaGUIglobals$limmaGUIfont2),entry.Background,tklabel(ttGetJpegOrPngParams,text="    "),sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  PointSizeTcl <- tclVar(paste(12))
  entry.PointSize<-tkentry(ttGetJpegOrPngParams,width="10",font=.limmaGUIglobals$limmaGUIfont2,textvariable=PointSizeTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="Font Size    ",font=.limmaGUIglobals$limmaGUIfont2),entry.PointSize,tklabel(ttGetJpegOrPngParams,text="    "),sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))

  ReturnVal <- list()
  Width <- 600
  Height <- 600
  Background <- "white"
  PointSize <- 12

  onOK <- function()
  {
      Try(Width  <<- as.numeric(tclvalue(WidthTcl)))
      Try(Height <<- as.numeric(tclvalue(HeightTcl)))
      Try(Background <<- tclvalue(BackgroundTcl))
      Try(PointSize <<- as.numeric(tclvalue(PointSizeTcl)))
      Try(tkgrab.release(ttGetJpegOrPngParams))
      Try(tkdestroy(ttGetJpegOrPngParams))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      Try(ReturnVal <<- list(width=Width,height=Height,pointsize=PointSize,bg=Background))
  }
  onCancel <- function() {Try(tkgrab.release(ttGetJpegOrPngParams));Try(tkdestroy(ttGetJpegOrPngParams));Try(tkfocus(.limmaGUIglobals$ttMain));Try(ReturnVal <<- list())}
  OK.but <-tkbutton(ttGetJpegOrPngParams,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2)
  Cancel.but <-tkbutton(ttGetJpegOrPngParams,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2)
  tkgrid(OK.but,Cancel.but)
  tkgrid(tklabel(ttGetJpegOrPngParams,text="    "))
  Try(tkfocus(ttGetJpegOrPngParams))
  Try(tkbind(ttGetJpegOrPngParams, "<Destroy>", function() {Try(tkgrab.release(ttGetJpegOrPngParams));Try(tkfocus(.limmaGUIglobals$ttMain));}))
  Try(tkwait.window(ttGetJpegOrPngParams))

  return (ReturnVal)
}

GetJpegOrPngX11Params <- function(graphFileType)
{
  ttGetJpegOrPngX11Params<-tktoplevel(.limmaGUIglobals$ttMain)
  tkwm.deiconify(ttGetJpegOrPngX11Params)
  tkgrab.set(ttGetJpegOrPngX11Params)
  tkfocus(ttGetJpegOrPngX11Params)
  tkwm.title(ttGetJpegOrPngX11Params,paste(graphFileType,"Image Parameters"))
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="    "))
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text=paste(graphFileType,"Image Parameters"),font=.limmaGUIglobals$limmaGUIfont2),columnspan=2)
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="    "))
  BackgroundTcl <- tclVar("white")
  entry.Background<-tkentry(ttGetJpegOrPngX11Params,width="20",font=.limmaGUIglobals$limmaGUIfont2,textvariable=BackgroundTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="Background    ",font=.limmaGUIglobals$limmaGUIfont2),entry.Background,sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="    "))
  ResolutionTcl <- tclVar("72")
  entry.Resolution<-tkentry(ttGetJpegOrPngX11Params,width="20",font=.limmaGUIglobals$limmaGUIfont2,textvariable=ResolutionTcl,bg="white")
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="Resolution    ",font=.limmaGUIglobals$limmaGUIfont2),entry.Resolution,sticky="w")
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="    "))

  ReturnVal <- list()
  Background <- "white"
  Resolution <- 72

  onOK <- function()
  {
      Try(Background <<- tclvalue(BackgroundTcl))
      Try(Resolution <<- as.numeric(tclvalue(ResolutionTcl)))
      Try(tkgrab.release(ttGetJpegOrPngX11Params))
      Try(tkdestroy(ttGetJpegOrPngX11Params))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      Try(ReturnVal <<- list(bg=Background,res=Resolution))
  }
  onCancel <- function() {Try(tkgrab.release(ttGetJpegOrPngX11Params));Try(tkdestroy(ttGetJpegOrPngX11Params));Try(tkfocus(.limmaGUIglobals$ttMain));Try(ReturnVal <<- list())}
  OK.but <-tkbutton(ttGetJpegOrPngX11Params,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2)
  Cancel.but <-tkbutton(ttGetJpegOrPngX11Params,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2)
  tkgrid(OK.but,Cancel.but)
  tkgrid(tklabel(ttGetJpegOrPngX11Params,text="    "))
  Try(tkfocus(ttGetJpegOrPngX11Params))
  Try(tkbind(ttGetJpegOrPngX11Params, "<Destroy>", function() {Try(tkgrab.release(ttGetJpegOrPngX11Params));Try(tkfocus(.limmaGUIglobals$ttMain));}))
  Try(tkwait.window(ttGetJpegOrPngX11Params))

  return (ReturnVal)
}

VennDiagramPlot <- function()
{
  Try(NumParameters <- get("NumParameters",envir=limmaGUIenvironment))
  Try(ParameterizationList <- get("ParameterizationList",envir=limmaGUIenvironment))
  Try(NumParameterizations <- get("NumParameterizations",envir=limmaGUIenvironment))
  Try(ParameterizationNamesVec <- get("ParameterizationNamesVec",envir=limmaGUIenvironment))
  Try(ParameterizationTreeIndexVec <- get("ParameterizationTreeIndexVec",envir=limmaGUIenvironment))
  Try(ArraysLoaded  <- get("ArraysLoaded", envir=limmaGUIenvironment))
  Try(NormalizedMADataWasImported<- get("NormalizedMADataWasImported", envir=limmaGUIenvironment))
  Try(LinearModelComputed <- get("LinearModelComputed", envir=limmaGUIenvironment))
  Try(limmaDataSetNameText <- get("limmaDataSetNameText",envir=limmaGUIenvironment))

  Try(if (ArraysLoaded==FALSE && NormalizedMADataWasImported==FALSE)
  {
      tkmessageBox(title="Venn Diagram",message="No arrays have been loaded.  Please try New or Open from the File menu.",type="ok",icon="error")
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  })

  Try(if (NumParameterizations==0)
  {
    Try(tkmessageBox(title="Venn Diagram",message="There are no parameterizations loaded.  Select \"Create New Parameterization\" or \"Compute Linear Model Fit\" from the \"Linear Model\" menu.",type="ok",icon="error"))
    Try(tkfocus(.limmaGUIglobals$ttMain))
    return()
  })

  Try(parameterizationIndex <- ChooseParameterization())
  Try(if (parameterizationIndex==0)    return())
  Try(parameterizationTreeIndex <- ParameterizationTreeIndexVec[parameterizationIndex])

  if (Try(LinearModelComputed[parameterizationIndex]==FALSE))
  {
      Try(tkmessageBox(title="Venn Diagram",message=paste("No linear model fit is available for ",ParameterizationNamesVec[parameterizationIndex],".  Please try Compute Linear Model from the Linear Model menu.",sep=""),type="ok",icon="error"))
      Try(tkfocus(.limmaGUIglobals$ttMain))
      return()
  }

  Try(ParameterizationNameNode <- paste("ParameterizationName.",parameterizationTreeIndex,sep=""))

  Try(designList <- (ParameterizationList[[ParameterizationNameNode]])$designList)
  Try(design <- designList$design)

  Try(tstats <- c())

  Try(ParametersAndOrContrasts <- GetParametersAndOrContrasts(parameterizationTreeIndex,whatFor="venn"))
  Try(NumParametersSelected <- length(ParametersAndOrContrasts))
  Try(if (NumParametersSelected==0)
    return())

  Try(include <- UpDownOrBoth())
  Try(if (include=="")
    return())

  Try(coefList <- list())
  Try(contrastsMatrix <- c())

#  Try(tkmessageBox(message=paste("NumParametersSelected :",NumParametersSelected)))

  for (i in (1:NumParametersSelected))
  {
    Try(coefList[[i]] <- ((ParametersAndOrContrasts[[i]])$coefIndexList[[(ParametersAndOrContrasts[[i]])$coefIndex]])$coefOrContrastIndex)
    Try(ContrastParameterizationIndex <- ParametersAndOrContrasts[[i]]$coefIndexList[[ParametersAndOrContrasts[[i]]$coefIndex]]$ContrastParameterizationIndex)
    Try(if ((ParametersAndOrContrasts[[i]])$parameterIsFromMainFit)
    {
      Try(tstat <- (ParameterizationList[[ParameterizationNameNode]])$eb$t)
      Try(tstat <- as.matrix(tstat))
      Try(if (ncol(tstat)>1)
        tstat <- tstat[,coefList[[i]]])
      Try(contrastsMatrix <- cbind(contrastsMatrix,as.matrix(rep(0,NumParameters))))
      Try(contrastsMatrix[coefList[[i]],ncol(contrastsMatrix)] <- 1)
      Try(ParameterOrContrastName <- colnames(design)[coefList[[i]]])
    }
    else
    {
#     tkmessageBox(message="OK, we've come across something which looks like a contrast")
      Try(tstat <- ((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex]])$eb$t)
      Try(tstat <- as.matrix(tstat))
      Try(if (ncol(tstat)>1)
        tstat <- tstat[,coefList[[i]]])
      Try(contrasts.matrix <- ((ParameterizationList[[ParameterizationNameNode]])$Contrasts[[ContrastParameterizationIndex]])$contrastsMatrixInList$contrasts)
#    Try(for (j in (1:ncol(contrasts.matrix)))
#      for (i in (1:nrow(contrasts.matrix)))
#        Try(tkmessageBox(message=paste("contrasts.matrix[",i,",",j,"] = ",contrasts.matrix[i,j],sep=""))))
#    Try(for (j in (1:ncol(contrasts.matrix)))
#        Try(tkmessageBox(message=paste("colnames(contrasts.matrix)[",j,"] = ",colnames(contrasts.matrix)[j],sep=""))))
      Try(contrastsMatrix <- cbind(contrastsMatrix,as.matrix(contrasts.matrix[,coefList[[i]]])))
      Try(ParameterOrContrastName <- colnames(contrasts.matrix)[coefList[[i]]])
    })

    Try(if (length(tstats)==0)
      Try(tstats <- as.matrix(tstat))
    else
    {
      Try(tstats <- cbind(tstats,  as.matrix(tstat)))
    })
    Try(colnames(tstats)[ncol(tstats)] <- ParameterOrContrastName)
  }

  Try(p.value <- 0.01)
  Try(pvalueText <- GetPValueCutoff(p.value))
  Try(if (pvalueText=="ID_CancelFromGetPValueCutoff") return())
  Try(while (pvalueText=="" || inherits(try(p.value <- eval(parse(text=pvalueText)),TRUE),"try-error"))
  {
    Try(tkmessageBox(title="Invalid P-Value",message="Please enter a valid decimal number for the p-value cutoff.",icon="error",type="ok",default="ok"))
    Try(pvalueText <- GetPValueCutoff())
    Try(if (pvalueText=="ID_CancelFromGetPValueCutoff") return())
  })

  Try(clas <- classifyTestsF(tstats,p.value=p.value))
  Try(vc   <- vennCounts(clas,include=include))

  plotVennDiagram <- function()
  {
    Try(opar<-par(bg="white"))
    Try(vennDiagramlimmaGUI(vc,include=include,cex=0.85,mar=rep(1,4)))
    Try(TempGraphPar<-par(opar))
  }

  Try(LocalHScale <- .limmaGUIglobals$Myhscale)
  Try(LocalVScale <- .limmaGUIglobals$Myvscale)

  # FIXME: It'd be nice to list the one, two or three parameters.
  Try(plotTitle <- paste("Venn diagram for parameterization",ParameterizationNamesVec[parameterizationIndex]))

  # Also it'd be nice to allow user labels for the sets, if the default labels are too large.

  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="watch"))
  Try(tkfocus(.limmaGUIglobals$ttMain))

  Try(if (.limmaGUIglobals$graphicsDevice=="tkrplot")
  {
    Try(ttVennDiagramPlot <- tktoplevel(.limmaGUIglobals$ttMain))
    Try(tkwm.title(ttVennDiagramPlot,plotTitle))
    Try(Require("tkrplot"))
    Try(img <- tkrplot(ttVennDiagramPlot,plotVennDiagram,hscale=LocalHScale,vscale=LocalVScale))
    Try(SetupPlotKeyBindings(tt=ttVennDiagramPlot,img=img))
    Try(SetupPlotMenus(tt=ttVennDiagramPlot,initialfile=paste(limmaDataSetNameText,"VennDiagram",sep=""),
                 plotFunction=plotVennDiagram,img=img))
    Try(tkgrid(img))
    Try(tkfocus(ttVennDiagramPlot))
  }
  else
  {
    Try(plot.new())
    Try(plotVennDiagram())
  })
  Try(tkconfigure(.limmaGUIglobals$ttMain,cursor="arrow"))

}


UpDownOrBoth <- function()
{
  Try(ttUpDownOrBoth <- tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.title(ttUpDownOrBoth,"D.E. Genes to Include in Venn Diagram"))
  Try(tkwm.deiconify(ttUpDownOrBoth))
  Try(tkgrab.set(ttUpDownOrBoth))
  Try(tkfocus(ttUpDownOrBoth))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    ")))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    "),tklabel(ttUpDownOrBoth,text="Which differentially expressed genes should be",font=.limmaGUIglobals$limmaGUIfont2),tklabel(ttUpDownOrBoth,text="    ")))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    "),tklabel(ttUpDownOrBoth,text="included in the Venn diagram?",font=.limmaGUIglobals$limmaGUIfont2),tklabel(ttUpDownOrBoth,text="    ")))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    ")))
  Try(UpDownOrBothTcl <- tclVar("both"))
  Try(frame1 <- tkframe(ttUpDownOrBoth,relief="groove",borderwidth="2"))
  Try(tkgrid(tkradiobutton(frame1,text="Up-regulated genes",variable=UpDownOrBothTcl,value="up",font=.limmaGUIglobals$limmaGUIfont2),sticky="w"))
  Try(tkgrid(tkradiobutton(frame1,text="Down-regulated genes",variable=UpDownOrBothTcl,value="down",font=.limmaGUIglobals$limmaGUIfont2),sticky="w"))
  Try(tkgrid(tkradiobutton(frame1,text="Both",variable=UpDownOrBothTcl,value="both",font=.limmaGUIglobals$limmaGUIfont2),sticky="w"))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    "),frame1))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    ")))
  Try(tkframeOKCancel <- tkframe(ttUpDownOrBoth))
  Try(ReturnVal <- "")
  Try(onOK <- function() { Try(ReturnVal <<- tclvalue(UpDownOrBothTcl)); Try(tkdestroy(ttUpDownOrBoth));Try(tkfocus(.limmaGUIglobals$ttMain))})
  Try(onCancel <- function() { Try(tkdestroy(ttUpDownOrBoth));Try(ReturnVal <- "")})
  Try(OK.but     <- tkbutton(tkframeOKCancel,text="   OK   ",command=onOK,    font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <- tkbutton(tkframeOKCancel,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(tkframeOKCancel,text="    "),columnspan=2))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(tkframeOKCancel,text="    "),columnspan=2))
  Try(tkgrid(tklabel(ttUpDownOrBoth,text="    "),tkframeOKCancel))

  Try(tkbind(ttUpDownOrBoth, "<Destroy>", function() {Try(tkgrab.release(ttUpDownOrBoth));Try(tkfocus(.limmaGUIglobals$ttMain))}))
  Try(tkwait.window(ttUpDownOrBoth))

  return (ReturnVal)
}


vennDiagramlimmaGUI <- function(object,include="both",names,cex=1.5,mar=rep(1,4),...) {
# Plot Venn diagram
# Gordon Smyth and James Wettenhall
# 4 July 2003.  Last modified 23 September 2003.

  if(class(object) != "VennCounts") object <- vennCounts(object,include=include)
  nsets <- ncol(object)-1
  if(nsets > 3) stop("Can't plot Venn diagram for more than 3 sets")
  if(missing(names)) names <- colnames(object)[1:nsets]
  counts <- object[,"Counts"]
  theta <- 2*pi*(1:360)/360
  xcentres <- list(0,c(-1,1),c(-1,1,0))[[nsets]]
  ycentres <- list(0,c(0,0),c(1/sqrt(3),1/sqrt(3),-2/sqrt(3)))[[nsets]]
  r <- c(1.5,1.5,1.5)[nsets]
  xtext <- list(-1.2,c(-1.2,1.2),c(-1.2,1.2,0))[[nsets]]
  ytext <- list(1.8,c(1.8,1.8),c(2.4,2.4,-3))[[nsets]]
  opar <- par(mar=mar)
  on.exit(par(opar))
  plot(x=0,y=0,type="n",xlim=c(-4,4),ylim=c(-4,4),xlab="",ylab="",axes=FALSE,...)
  for(circle in 1:nsets) {
    lines(xcentres[circle]+r*cos(theta),ycentres[circle]+r*sin(theta))
    text(xtext[circle],ytext[circle],names[circle],cex=cex)
  }
  switch(nsets,
    {
      rect(-3,-2.5,3,2.5)
      text(2.3,-2.1,counts[1],cex=cex)
      text(0,0,counts[2],cex=cex)
    }, {
      rect(-3,-2.5,3,2.5)
        text(2.3,-2.1,counts[1],cex=cex)
      text(1.5,0.1,counts[2],cex=cex)
      text(-1.5,0.1,counts[3],cex=cex)
      text(0,0.1,counts[4],cex=cex)
    }, {
      rect(-3,-3.5,3,3.3)
      text(2.5,-3,counts[1],cex=cex)
      text(0,-1.7,counts[2],cex=cex)
      text(1.5,1,counts[3],cex=cex)
      text(.75,-.35,counts[4],cex=cex)
      text(-1.5,1,counts[5],cex=cex)
      text(-.75,-.35,counts[6],cex=cex)
      text(0,.9,counts[7],cex=cex)
      text(0,0,counts[8],cex=cex)
    }
  )
  invisible()
}

GetPValueCutoff <- function(p.value=0.01)
{
  Try(ttGetPValueCutoff<-tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.deiconify(ttGetPValueCutoff))
  Try(tkgrab.set(ttGetPValueCutoff))
  Try(tkfocus(ttGetPValueCutoff))
  Try(tkwm.title(ttGetPValueCutoff,"P-Value Cutoff"))
  Try(tkgrid(tklabel(ttGetPValueCutoff,text="    ")))
  Try(PValueCutoffTcl <- tclVar(paste(p.value)))
  Try(entry.PValueCutoff <-tkentry(ttGetPValueCutoff,width="20",font=.limmaGUIglobals$limmaGUIfont2,textvariable=PValueCutoffTcl,bg="white"))
  Try(tkgrid(tklabel(ttGetPValueCutoff,text="Enter a p-value cutoff",font=.limmaGUIglobals$limmaGUIfont2),columnspan=2))
  Try(tkgrid(entry.PValueCutoff,columnspan=2))
  Try(ReturnVal <- "ID_CancelFromGetPValueCutoff")
  onOK <- function()
  {
    Try(PValueCutoffTxt <- tclvalue(PValueCutoffTcl))
    Try(ReturnVal <<- PValueCutoffTxt)
    Try(tkgrab.release(ttGetPValueCutoff));Try(tkdestroy(ttGetPValueCutoff));Try(tkfocus(.limmaGUIglobals$ttMain))
  }
  onCancel <- function()
  {
    Try(ReturnVal <<- "ID_CancelFromGetPValueCutoff")
    Try(tkgrab.release(ttGetPValueCutoff));Try(tkdestroy(ttGetPValueCutoff));Try(tkfocus(.limmaGUIglobals$ttMain))
  }
  Try(OK.but <-tkbutton(ttGetPValueCutoff,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <-tkbutton(ttGetPValueCutoff,text=" Cancel ",command=onCancel,font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(ttGetPValueCutoff,text="    ")))
  Try(tkgrid(OK.but,Cancel.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="w"))
  Try(tkgrid(tklabel(ttGetPValueCutoff,text="       ")))
  Try(tkfocus(entry.PValueCutoff))
  Try(tkbind(entry.PValueCutoff, "<Return>",onOK))
  Try(tkbind(ttGetPValueCutoff, "<Destroy>", function(){Try(tkgrab.release(ttGetPValueCutoff));Try(tkfocus(.limmaGUIglobals$ttMain));return("ID_CancelFromGetPValueCutoff")}))
  Try(tkwait.window(ttGetPValueCutoff))
  Try(tkfocus(.limmaGUIglobals$ttMain))
  return (ReturnVal)
}

GetLowessType <- function()
{
  Try(ttGetLowessType<-tktoplevel(.limmaGUIglobals$ttMain))
  Try(tkwm.title(ttGetLowessType,"Lowess Curve(s) Options"))
  Try(tkwm.deiconify(ttGetLowessType))
  Try(tkgrab.set(ttGetLowessType))
  Try(tkframe1 <- tkframe(ttGetLowessType,borderwidth=2))
  Try(tkframe2 <- tkframe(tkframe1,relief="groove",borderwidth=2))
  Try(tkframe4<-tkframe(tkframe1,borderwidth=2))

  Try(tkgrid(tklabel(tkframe1,text="    ")))

  Try(tkgrid(tklabel(tkframe2,text="Lowess Curve(s) Options",font=.limmaGUIglobals$limmaGUIfont2),column=2,rowspan=1,columnspan=2,sticky="w"))

  Try(LowessType <- "printtip")
  Try(LowessTypeTcl <- tclVar(LowessType))

  Try(none.but <- tkradiobutton(tkframe2,text="No Lowess Curve",variable=LowessTypeTcl,value="none",font=.limmaGUIglobals$limmaGUIfont2))
  Try(global.but <- tkradiobutton(tkframe2,text="Global Lowess Curve",variable=LowessTypeTcl,value="global",font=.limmaGUIglobals$limmaGUIfont2))
  Try(printtip.but <- tkradiobutton(tkframe2,text="Print-Tip Group Lowess Curves",variable=LowessTypeTcl,value="printtip",font=.limmaGUIglobals$limmaGUIfont2))


  Try(tkgrid(none.but,column=2))
  Try(tkgrid(global.but,column=2))
  Try(tkgrid(printtip.but,column=2))
  Try(tkgrid.configure(none.but,global.but,printtip.but,sticky="w"))
  Try(tkgrid(tkframe2))
  Try(NewLowessType <- "")
  onOK <- function()
  {
    Try(NewLowessType<<-tclvalue(LowessTypeTcl))
    Try(tkgrab.release(ttGetLowessType))
    Try(tkdestroy(ttGetLowessType))
    Try(LowessType <- NewLowessType)
    Try(tkfocus(.limmaGUIglobals$ttMain))
  }
  onHelp <- function()
  {
      ###Require("sma") ###removed on 16-Oct-2009
      Try(LowessType<-tclvalue(LowessTypeTcl))
      Try(if (LowessType=="printtip")
        Try(help("plotPrintTipLoess",htmlhelp=TRUE))) ##changed from plot.print.tip.lowess from the sma package
      Try(if (LowessType=="none")
        Try(help("plot.default",htmlhelp=TRUE)))
      Try(if (LowessType=="global")
      {
        Try(help("plot.mva",htmlhelp=TRUE))
      })
  }
  Try(OK.but <-tkbutton(tkframe4,text="   OK   ",command=onOK,font=.limmaGUIglobals$limmaGUIfont2))
  Try(Cancel.but <-tkbutton(tkframe4,text=" Cancel ",command=function(){Try(tkgrab.release(ttGetLowessType));Try(tkdestroy(ttGetLowessType));NewLowessType<-"";Try(tkfocus(.limmaGUIglobals$ttMain))},font=.limmaGUIglobals$limmaGUIfont2))
  Try(Help.but <- tkbutton(tkframe4,text=" Help ",command=onHelp,font=.limmaGUIglobals$limmaGUIfont2))
  Try(tkgrid(tklabel(tkframe4,text="                    ")))
  Try(tkgrid(OK.but,Cancel.but,Help.but))
  Try(tkgrid.configure(OK.but,sticky="e"))
  Try(tkgrid.configure(Cancel.but,sticky="e"))
  Try(tkgrid.configure(Help.but,sticky="e"))
  Try(tkgrid(tklabel(tkframe4,text="       ")))
  Try(tkgrid(tkframe4))
  Try(tkgrid(tkframe1))
  Try(tkfocus(OK.but))
  Try(tkbind(ttGetLowessType, "<Return>",onOK))
  Try(tkbind(ttGetLowessType, "<Destroy>", function() {Try(tkgrab.release(ttGetLowessType));Try(tkfocus(.limmaGUIglobals$ttMain))}))
  Try(tkwait.window(ttGetLowessType))

  Try(tkdestroy(ttGetLowessType))

# This return value below is not used.  The function above is used for its effect
# on LowessType in limmaGUIenvironment
  return(NewLowessType)
}
