cup vol = \message -> message vol

getVol cup = cup (\vol -> vol)

drink aCup howMuch = cup newVol
    where newVol = (getVol aCup) - howMuch
