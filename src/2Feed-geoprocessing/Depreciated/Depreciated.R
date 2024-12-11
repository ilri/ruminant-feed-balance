funBurnGrass <- function(burnBin, grass, shrub, greenup, senesence, greenup2, senesence2, burndate) {
  funBurnGrass1 <- overlay(senesence, grass, shrub, fun = function(a, b, c) {return((a >= burndate) & ((b + c) > 0.25))})
  funBurnGrass2 <- overlay(greenup2, senesence2, grass, shrub, fun = function(d, e, f, g) {return((d <= burndate) & (e >= burndate) & ((f + g) > 0.25))})
  funBurnGrass3 <- funBurnGrass1 | funBurnGrass2
  result <- mask(burnBin, funBurnGrass3, maskvalue = 0, updatevalue = NA)
  return(result)
}

iBurnGrass <- stack(iBurnGrass, funBurnGrass(stBurn[[2]], stLU$LUgrass300, stLU$LUshrub300, stPhen$phenoGreenup1,stPhen$phenoSenescence1, stPhen$phenoGreenup2,stPhen$phenoSenescence2, datesBurndiff[i]))
