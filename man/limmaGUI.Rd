\title{Graphical User Interface for the limma microarray package}
\name{limmaGUI}

\alias{AboutLimmaGUI}
\alias{AboutNormalization}
\alias{BChelp}
\alias{ChooseContrastsParameterization}
\alias{chooseDir}
\alias{ChooseEbayesStatistic}
\alias{ChooseParameterization}
\alias{ChoosePlotSymbolByClicking}
\alias{ChooseSpotType}
\alias{ComputeContrasts}
\alias{ComputeLinearModelFit}
\alias{CopyGraph}
\alias{CreateNewParameterization}
\alias{CreateTreeAndList}
\alias{DeleteContrastsParameterization}
\alias{deleteItemFromList}
\alias{DeleteParameterization}
\alias{DupCorBoxPlot}
\alias{ebayesBoxPlots}
\alias{evalRcode}
\alias{ExportAvalues}
\alias{ExportHTMLreport}
\alias{ExportMvalues}
\alias{ExportTopTable}
\alias{fixSeps}
\alias{GetBackgroundCorrectionMethod}
\alias{GetBetweenArrayNormalizationMethod}
\alias{GetCoef}
\alias{GetComponentsToExportInHTMLreport}
\alias{GetContrastsParameterizationNames}
\alias{GetContrastsParameterizationName}
\alias{GetDEcutoff}
\alias{GetDesignOrContrasts}
\alias{GetGeneLabelsOptions}
\alias{GetGenePixFlagWeightings}
\alias{GetImageAnalysisColumnHeadings}
\alias{GetImageProcessingFileType}
\alias{GetJpegOrPngParams}
\alias{GetJpegOrPngX11Params}
\alias{GetlimmaDataSetName}
\alias{GetlmFitMethod}
\alias{GetLowessType}
\alias{GetNEWxlim}
\alias{GetNormexpOffsetValue}
\alias{GetNumParametersNoTargets}
\alias{getPackageVersion}
\alias{GetParameterizationName}
\alias{GetParameterNames}
\alias{GetParametersAndOrContrasts}
\alias{GetPlotLabels}
\alias{GetPlotSize}
\alias{GetPlotTitle}
\alias{GetPValueCutoff}
\alias{GetReducedDuplicateSpacing}
\alias{GetRNATypesFrom.ContrastsFromDropDowns.String}
\alias{GetSlideNum}
\alias{GetSpotTypesForLinearModel}
\alias{GetSpotTypesIncludedNames}
\alias{GetWithinArrayNormalizationMethod}
\alias{GetWtAreaParams}
\alias{HeatDiagramDialog}
\alias{HeatDiagramPlot}
\alias{HowManyDups}
\alias{HTMLplotUsingFunction}
\alias{ImageArrayPlotDialog}
\alias{ImageArrayPlot}
\alias{imageplotlimmaGUI}
\alias{ImportMADialog}
\alias{ImportMA}
\alias{initGlobals}
\alias{InitNewParameterization}
\alias{limmaGUIhelp}
\alias{limmaGUI}
\alias{limmaHelp}
\alias{limmaUsersGuide}
\alias{lmFitMethodHelp}
\alias{LogOddsPlot}
\alias{MAPlotAvg}
\alias{MAPlot}
\alias{MBoxPlot}
\alias{MMPlot}
\alias{NewLimmaFile}
\alias{NormalizeNow}
\alias{nstrstr}
\alias{onDestroy}
\alias{onExit}
\alias{OpenALimmaFile}
\alias{OpenGALandTargetsandSpotTypesfiles}
\alias{OpenGALFile}
\alias{OpenLimmaFile}
\alias{OpenSpotTypesFile}
\alias{OpenTargetsFile}
\alias{plotMAColorCoded}
\alias{PlotOptions}
\alias{PrintTipGroupMAPlot}
\alias{QQTplot}
\alias{read.marrayTools}
\alias{readGALlimmaGUI}
\alias{ReadImageProcessingFiles}
\alias{Require}
\alias{Resize}
\alias{SaveAsLimmaFile}
\alias{SaveGraphAsJpeg}
\alias{SaveGraphAsPDF}
\alias{SaveGraphAsPNG}
\alias{SaveGraphAsPostscript}
\alias{SaveLimmaFile}
\alias{SelectPlotSymbols}
\alias{SetLayoutParameters}
\alias{SetupPlotKeyBindings}
\alias{SetupPlotMenus}
\alias{SetWD}
\alias{showChangeLog}
\alias{showCitations}
\alias{showGAL}
\alias{showTopTable}
\alias{SimplifyContrastsExpression}
\alias{strstr}
\alias{tclArrayVar}
\alias{TclRequire}
\alias{TryReadImgProcFile}
\alias{Try}
\alias{UpdateSpotTypesStatus}
\alias{UpDownOrBoth}
\alias{vennDiagramlimmaGUI}
\alias{VennDiagramPlot}
\alias{ViewDesignOrContrastsMatrixAsPairs}
\alias{ViewDesignOrContrastsMatrixInTable}
\alias{ViewExistingContrastsParameterization}
\alias{ViewExistingParameterization}
\alias{ViewRNATargets}
\alias{ViewSpotTypes}
\alias{WithinBetweenOrBoth}
\alias{wtflags2}

\description{Graphical User Interface for the limma microarray package}

\usage{
AboutLimmaGUI()
AboutNormalization()
BChelp()
ChooseContrastsParameterization(parameterizationTreeIndex)
ChooseEbayesStatistic()
ChooseParameterization()
ChoosePlotSymbolByClicking(spotType, cex)
ChooseSpotType(parameterizationTreeIndex)
ComputeContrasts()
CopyGraph(img)
CreateNewParameterization()
DeleteContrastsParameterization()
deleteItemFromList(list1, itemName = NULL, index = NULL)
DeleteParameterization()
DupCorBoxPlot()
ebayesBoxPlots()
evalRcode()
ExportHTMLreport()
ExportTopTable()
fixSeps(string)
GetBackgroundCorrectionMethod()
GetBetweenArrayNormalizationMethod()
GetCoef(parameterizationTreeIndex, whichCoef = "onlyOne")
GetComponentsToExportInHTMLreport(parameterizationIndex = NULL)
GetContrastsParameterizationName()
GetContrastsParameterizationNames(parameterizationTreeIndex)
GetDEcutoff()
GetDesignOrContrasts(Design = FALSE, Contrasts = FALSE, NumContrasts = 0,parameterizationIndex = 0)
GetGeneLabelsOptions()
GetImageAnalysisColumnHeadings()
GetImageProcessingFileType()
GetJpegOrPngParams(graphFileType)
GetJpegOrPngX11Params(graphFileType)
GetlimmaDataSetName()
GetlmFitMethod()
GetLowessType()
GetNEWxlim(xlim)
GetNormexpOffsetValue(CurrentNormexpOffsetValue)
GetNumParametersNoTargets()
getPackageVersion(pkgName)
GetParameterizationName()
GetParameterNames(parameterizationTreeIndex)
GetParametersAndOrContrasts(parameterizationTreeIndex, whatFor = "heat")
GetPlotLabels(plottitle = "", xlabel = "", ylabel = "")
GetPlotSize()
GetPlotTitle(plottitle = "")
GetPValueCutoff(p.value=0.01)
GetReducedDuplicateSpacing(parameterizationTreeIndex)
GetRNATypesFrom.ContrastsFromDropDowns.String(string)
GetSlideNum()
GetSpotTypesForLinearModel()
GetSpotTypesIncludedNames(parameterizationTreeIndex)
GetWithinArrayNormalizationMethod()
GetWtAreaParams()
HeatDiagramDialog(parameterName)
HeatDiagramPlot()
HowManyDups()
HTMLplotUsingFunction(Caption = "", File = .HTML.file,
                 GraphRelativeDirectory = ".", GraphAbsoluteDirectory =
                 NULL, GraphFileName = "", GraphSaveAs = "png",
                 GraphBorder = 1, Align = "center", plotFunction =
                 NULL, Width = 600, Height = 600, PointSize = 12, BG =
                 "white", res = 72, \dots)
ImageArrayPlot()
ImageArrayPlotDialog(slidenum)
ImportMA()
ImportMADialog()
initGlobals()
InitNewParameterization()
limmaGUI(BigfontsForlimmaGUIpresentation=FALSE)
limmaHelp()
limmaUsersGuide(view = TRUE)
lmFitMethodHelp()
LogOddsPlot()
MAPlot()
MAPlotAvg()
MBoxPlot()
MMPlot()
NewLimmaFile()
NormalizeNow()
nstrstr(haystack, needle)
onDestroy()
onExit()
OpenALimmaFile(FileName)
OpenGALandTargetsandSpotTypesfiles()
OpenGALFile()
OpenLimmaFile()
OpenSpotTypesFile()
OpenTargetsFile()
plotMAColorCoded()
PlotOptions()
PrintTipGroupMAPlot()
QQTplot()
read.marrayTools(MFile,AFile, path, verbose, sep, quote, header, ...)
ReadImageProcessingFiles()
Require(pkg)
Resize(img, plotFunction)
SaveAsLimmaFile()
SaveGraphAsJpeg(initialfile, plotFunction)
SaveGraphAsPDF(initialfile, plotFunction)
SaveGraphAsPNG(initialfile, plotFunction)
SaveGraphAsPostscript(initialfile, plotFunction)
SaveLimmaFile()
SelectPlotSymbols(SpotTypes)
SetLayoutParameters()
SetupPlotKeyBindings(tt, img)
SetupPlotMenus(tt, initialfile, plotFunction, img)
SetWD()
showChangeLog()
showCitations()
showGAL()
showTopTable(\dots, export = FALSE)
SimplifyContrastsExpression(string)
strstr(haystack, needle)
tclArrayVar()
TclRequire(tclPkg)
TryReadImgProcFile(expr)
UpdateSpotTypesStatus()
UpDownOrBoth()
VennDiagramPlot()
ViewDesignOrContrastsMatrixAsPairs(DesignOrContrasts, designOrContrastsList,
                 parameterizationIndex, contrastsParameterizationIndex
                 = NULL)
ViewDesignOrContrastsMatrixInTable(DesignOrContrasts, designOrContrastsList,
                 parameterizationIndex, contrastsParameterizationIndex
                 = NULL)
ViewExistingContrastsParameterization()
ViewExistingParameterization()
ViewRNATargets()
ViewSpotTypes()
}

\arguments{
	\item{BigfontsForlimmaGUIpresentation}{If set to TRUE, larger fonts are used.  However, some font
	sizes are not controlled by limmaGUI and so must be adjusted in the operating system, e.g.
	in the Control Panel in Windows under Display, Appearance.}

	\item{\dots}                         {HTMLplotUsingFunction:arg15,showTopTable:arg1}
	\item{AFile}                         {Flat-file of log-intensities output by marrayTools}
	\item{Align}                         {HTMLplotUsingFunction:arg8}
	\item{BG}                            {HTMLplotUsingFunction:arg13}
	\item{Caption}                       {HTMLplotUsingFunction:arg1}
	\item{cex}                           {ChoosePlotSymbolByClicking:arg2}
	\item{contrastsParameterizationIndex}{ViewDesignOrContrastsMatrixInTable:arg4,ViewDesignOrContrastsMatrixAsPairs:arg4}
	\item{Contrasts}                     {GetDesignOrContrasts:arg2}
	\item{CurrentNormexpOffsetValue}     {GetNormexpOffsetValue:arg1}
	\item{designOrContrastsList}         {ViewDesignOrContrastsMatrixInTable:arg2,ViewDesignOrContrastsMatrixAsPairs:arg2}
	\item{DesignOrContrasts}             {ViewDesignOrContrastsMatrixInTable:arg1,ViewDesignOrContrastsMatrixAsPairs:arg1}
	\item{Design}                        {GetDesignOrContrasts:arg1}
	\item{export}                        {showTopTable:arg2}
	\item{expr}                          {TryReadImgProcFile:arg1}
	\item{FileName}                      {A file name.}
	\item{File}                          {HTMLplotUsingFunction:arg2}
	\item{GraphAbsoluteDirectory}        {HTMLplotUsingFunction:arg4}
	\item{GraphBorder}                   {HTMLplotUsingFunction:arg7}
	\item{GraphFileName}                 {HTMLplotUsingFunction:arg5}
	\item{graphFileType}                 {GetJpegOrPngParams:arg1,GetJpegOrPngX11Params:arg1}
	\item{GraphRelativeDirectory}        {HTMLplotUsingFunction:arg3}
	\item{GraphSaveAs}                   {HTMLplotUsingFunction:arg6}
	\item{haystack}                      {nstrstr:arg1,strstr:arg1}
	\item{header}                        {See help for read.table}
	\item{Height}                        {HTMLplotUsingFunction:arg11}
	\item{img}                           {SetupPlotMenus:arg1,Resize:arg1,CopyGraph:arg1,SetupPlotKeyBindings:arg2}
	\item{index}                         {deleteItemFromList:arg1}
	\item{initialfile}                   {SaveGraphAsJpeg:arg1,SaveGraphAsPDF:arg1,SaveGraphAsPNG:arg1,SaveGraphAsPostscript:arg1,SetupPlotMenus:arg2}
	\item{itemName}                      {deleteItemFromList:arg1}
	\item{list1}                         {deleteItemFromList:arg1}
	\item{MFile}                         {Flat-file of log-ratios output by marrayTools}
	\item{needle}                        {nstrstr:arg2,strstr:arg2}
	\item{NumContrasts}                  {GetDesignOrContrasts:arg3}
	\item{p.value}                       {A p-value cutoff.}
	\item{parameterizationIndex}         {GetDesignOrContrasts:arg4,GetComponentsToExportInHTMLreport:arg1,GetDesignOrContrasts:arg4,ViewDesignOrContrastsMatrixInTable:arg3,ViewDesignOrContrastsMatrixAsPairs:arg3}
	\item{parameterizationTreeIndex}     {GetParametersAndOrContrasts:arg1,ChooseContrastsParameterization:arg1,ChooseSpotType:arg1,GetCoef:arg1,GetParameterNames:arg1,GetReducedDuplicateSpacing:arg1,GetContrastsParameterizationNames:arg1,GetSpotTypesIncludedNames:arg1}
	\item{parameterName}                 {HeatDiagramDialog:arg1}
	\item{path}                          {Path to the directory containing the MFile and AFile}
	\item{pkgName}                       {getPackageVersion:arg1}
	\item{pkg}                           {Require:arg1}
	\item{plotFunction}                  {SetupPlotMenus:arg1,HTMLplotUsingFunction:arg9,Resize:arg1,SaveGraphAsJpeg:arg2,SaveGraphAsPDF:arg2,SaveGraphAsPNG:arg2,SaveGraphAsPostscript:arg2}
	\item{plottitle}                     {GetPlotLabels:arg1,GetPlotTitle:arg1}
	\item{PointSize}                     {HTMLplotUsingFunction:arg12}
	\item{quote}                         {See help for read.table}
	\item{res}                           {HTMLplotUsingFunction:arg14}
	\item{sep}                           {Column separator. ("\t" for tab-delimited text)}
	\item{slidenum}                      {ImageArrayPlotDialog:arg1}
	\item{spotType}                      {ChoosePlotSymbolByClicking:arg1}
	\item{SpotTypes}                     {SelectPlotSymbols:arg1}
	\item{string}                        {fixSeps:arg1,SimplifyContrastsExpression:arg1,GetRNATypesFrom.ContrastsFromDropDowns.String:arg1}
	\item{tclPkg}                        {TclRequire:arg1}
	\item{tt}                            {SetupPlotKeyBindings:arg1,SetupPlotMenus:arg1}
	\item{verbose}                       {Optional diagnostic messages}
	\item{view}                          {limmaUsersGuide:arg1}
	\item{whatFor}                       {GetParametersAndOrContrasts:arg2}
	\item{whichCoef}                     {GetCoef:arg2}
	\item{Width}                         {HTMLplotUsingFunction:arg10}
	\item{xlabel}                        {GetPlotLabels:arg2}
	\item{xlim}                          {GetNEWxlim:arg1}
	\item{ylabel}                        {GetPlotLabels:arg3}
}

\details{
This function launches a Graphical User Interface for the limma package by Gordon Smyth.
The GUI uses Tk widgets (via the R TclTk interface by Peter Dalgaard) in order to provide
a simple interface to the limma functions for linear modelling of microarrays and
identification of differentially expressed genes.
}

\author{James Wettenhall}

\keyword{IO}
