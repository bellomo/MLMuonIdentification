# cleanup
rm(list = ls())

require(RootTreeToR)
require(neuralnet)

variables = c(
  #"EvtNumber",
  #"averageIntPerXing",
  #"LumiBlock",
  #"ChannelNumber",
  #"EventWeight",
  #"isRecoMuon",
"author",
"isMuTagIMO",
"isMuTag",
"muonType",
  #"nMuons",
  "TruthType",
  #"TruthOrigin",
  #"SelectorQuality",
  #"SelectorAccepted",
"passedIDCuts",
  #"passedMuonCuts",
  #"passedHighPt",
  #"passedLowPt",
  #"isBadMuon",
"isIsolated",
"hasIDTrack",
"hasMETrack",
"hasMSTrack",
"hasCBTrack",
  #"pt",
  #"ptTruth",
  #"etaTruth",
  #"phiTruth",
  #"chargeTruth",
"pt_ID",
"pt_CB",
"pt_ME",
"pt_MS",
  #"pt_ID_Corr",
  #"pt_ME_Corr",
  #"pt_CB_Corr",
  #"eta",
  #"eta_ID",
  #"eta_ME",
  #"eta_CB",
  #"eta_MS",
  #"phi",
  #"phi_ID",
  #"phi_ME",
  #"phi_CB",
  #"phi_MS",
"reducedChi2_CB",
"reducedChi2_ID",
"reducedChi2_ME",
"reducedChi2_MS",
  #"qOverP_CB",
  #"qOverP_ID",
  #"qOverP_ME",
  #"qOverP_MS",
  #"qOverP_CB_Corr",
  #"qOverP_ID_Corr",
  #"qOverP_ME_Corr",
  #"qOverPerr_CB",
  #"qOverPerr_ID",
  #"qOverPerr_ME",
  #"qOverPerr_MS",
"d0_ID",
"z0_ID",
  #"d0err_ID",
  #"z0err_ID",
  #"d0_ME",
  #"z0_ME",
  #"d0err_ME",
  #"z0err_ME",
  #"d0_CB",
  #"z0_CB",
  #"d0err_CB",
  #"z0err_CB",
  #"momentumBalanceSignificance",
"scatteringCurvatureSignificance",
"scatteringNeighbourSignificance",
  #"qOverPSignificance",
  #"qOverPSignificance_Corr",
  #"EnergyLossType",
"EnergyLoss",
  #"EnergyLoss_Par",
  #"EnergyLoss_Meas",
  #"EnergyLossSigma",
"nPrecLayers",
"nGoodPrecLayers",
"nPrecHoleLayers",
"nPhiLayers",
"nPhiHoleLayers",
"nTriggerEtaLayers",
"nTriggerEtaHoleLayers",
"nPixHits",
"nPixInnermostHits",
"nOfOutliersOnTrack",
  #"nUsedHitsdEdx",
  #"nIBLOverflowsdEdx",
  #"pixeldEdx",
"nSctHits",
"nTrtHits",
"nPixHoles",
"nSctHoles",
"nTrtOutliers",
  #"nPixDeadSensors",
  #"nSctDeadSensors",
"nTrtHoles",
"nTrtHighThresholdHits",
"nTrtHighThresholdOutliers",
  #"nTrtDeadStraws",
"nTrtTubeHits",
"nTrtXenonHits",
"nTrtSharedHits",
"innerSmallHits",
"innerLargeHits",
"middleSmallHits",
"middleLargeHits",
"outerSmallHits",
"outerLargeHits",
"extendedSmallHits",
"extendedLargeHits",
"phiLayer1Hits",
"phiLayer2Hits",
"phiLayer3Hits",
"phiLayer4Hits",
"innerSmallHoles",
"innerLargeHoles",
"middleSmallHoles",
"middleLargeHoles",
"outerSmallHoles",
"outerLargeHoles",
"extendedSmallHoles",
"extendedLargeHoles",
"phiLayer1Holes",
"phiLayer2Holes",
"phiLayer3Holes",
"phiLayer4Holes",
"innerClosePrecHits",
"middleClosePrecHits",
"outerClosePrecHits",
"extendedClosePrecHits",
"innerOutBoundsPrecisionHits",
"middleOutBoundsPrecisionHits",
"outerOutBoundsPrecisionHits",
"extendedOutBoundsPrecisionHits",
"combinedTrackOutBoundsPrecisionHits",
"segmentChi2OverDoF"
  #"spectrometerFieldIntegral",
  #"msInnerMatchChi2",
  #"msInnerMatchDOF",
  #"AllSegRedChi2"
)

# read data from TTree
t = openRootChain(files = "ttbar_IDTracks_R21.root", tree = "IDTracksTree")

# convert tree to R data frame
data = toR(rootChain = t, 
           columns = variables, 
           nEntries = 100000, 
           selection = "isRecoMuon==1")

# define a variable to identify prompt muons
IsPrompt = ifelse(data$TruthType==6, 1, 0)

# add the new variable to the data frame
data = data.frame(data, IsPrompt)

# remove not needed variables
drops <- c("TruthType","treeNum","globalEntry","localEntry")
data = data[ , !(names(data) %in% drops)]

# build the formula for the NN
predictorVars = variables[!variables%in%"TruthType"]
predictorVars = paste(predictorVars, collapse = "+")
form = as.formula(paste("IsPrompt~", predictorVars, collapse = "+"))

# divide data in train and test samples
indTrain = sample(1:nrow(data), nrow(data)/2)
trainData = data[indTrain,]
testData = data[-indTrain,]

# define proportions of prompt and non-prompt data in the training sample
trainDataPrompt = trainData[trainData$IsPrompt==1,]
trainDataNonPrompt = trainData[trainData$IsPrompt==0,]

# 1-frac P vs frac NP
frac = 0.5
indTrain = sample(1:nrow(trainDataNonPrompt), as.integer(nrow(trainDataPrompt)*((1-frac)/frac)))
trainData = rbind(trainDataPrompt, trainDataNonPrompt[indTrain,])

# train a deep NN
nn = neuralnet(formula = form, 
               hidden = c(9,3),
               linear.output = FALSE,
               err.fct = "ce",
               data = trainData)

# check train error
nn1 = ifelse(nn$net.result[[1]]>0.5,1,0)
mean(trainData$IsPrompt != nn1)

# datasets for prompt and non prompt observations
testDataPrompt = testData[testData$IsPrompt==1,]
testDataNonPrompt = testData[testData$IsPrompt==0,]

# remove prediction variable
drops <- c("IsPrompt")
testDataAllObs = testData[ , !(names(testData) %in% drops)]
testDataPromptObs = testDataPrompt[ , !(names(testDataPrompt) %in% drops)]
testDataNonPromptObs = testDataNonPrompt[ , !(names(testDataNonPrompt) %in% drops)]

# make predictions on test data
testDataAllObs.pred = compute(nn, covariate = testDataAllObs)
testDataPromptObs.pred = compute(nn, covariate = testDataPromptObs)
testDataNonPromptObs.pred = compute(nn, covariate = testDataNonPromptObs)

# look at results
# testDataAllObs.pred$net.result

# check test error
nn1.testAllObs = ifelse(testDataAllObs.pred$net.result>0.5,1,0)
misid.all = mean(testData$IsPrompt != nn1.testAllObs)

nn1.testPromptObs = ifelse(testDataPromptObs.pred$net.result>0.5,1,0)
misid.prompt = mean(testDataPrompt$IsPrompt != nn1.testPromptObs)

nn1.testNonPromptObs = ifelse(testDataNonPromptObs.pred$net.result>0.5,1,0)
misid.nonprompt = mean(testDataNonPrompt$IsPrompt != nn1.testNonPromptObs)

cat("Total MidIs       : ", misid.all)
cat("Prompt efficiency : ", 1-misid.prompt)
cat("Fake rejection    : ", 1-misid.nonprompt)