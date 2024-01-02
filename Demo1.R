rm(list = ls())
source("SimulatedAnneal.R")
source("matTransfer.R")
source("checkCircles.R")
library(bnlearn)
library(xlsx)
file = "10_1.xlsx"
xdata = read.xlsx(file, sheetIndex =1)
rownames(xdata) <- xdata[,1]
xdata = xdata[,-1]

xdata = t(xdata)
xdata = as.data.frame.array(xdata)

Goldmat = read.xlsx(file, sheetIndex = 2)
rownames(Goldmat) <- Goldmat[,1]
Goldmat = Goldmat[,-1]

xdata_Corr = read.xlsx(file, sheetIndex = 3)
rownames(xdata_Corr) <- xdata_Corr[,1]
xdata_Corr = xdata_Corr[,-1]

lassoCorr = read.xlsx(file, sheetIndex = 5)
rownames(lassoCorr) <- lassoCorr[, 1]
lassoCorr = lassoCorr[, -1]

NodeName = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10")
vertexNum = 10
res0 = SA(xdata, xdata_Corr, lassoCorr, NodeName, vertexNum, 2, 0.5)
