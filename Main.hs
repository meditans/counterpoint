import Music.Prelude

main :: IO ()
main = do
  -- writeLilypond "test.ly" cantus
  return ()

type Voice' = Voice Pitch

cantus :: Voice' 
cantus = [d, a, g, f, e, d, f, e, d] ^. voice
         
intervalsV :: Getter Voice' [Interval]
intervalsV = to (\v -> zipWith (.-.) (tail $ v ^. valuesV) (v ^. valuesV))

allWholeNotes :: Voice' -> Bool
allWholeNotes v = all (== 1) (v ^. durationsV)

voiceLength :: Voice' -> Bool
voiceLength v = (between 8 16) . length $ v ^. notes

between a b x = a <= x && x <= b


horizontalIntervals :: Voice' -> [Interval]
horizontalIntervals v = zipWith (.-.) (tail ns) ns
    where ns = v ^. valuesV

permittedMelodicIntervals :: Voice' -> Bool
permittedMelodicIntervals v = all (\i -> abs i `elem` permitted) (horizontalIntervals v)
  where
    permitted = [ perfect unison, major second, minor second
                , major third,    minor third,  perfect fourth
                , perfect fifth,  major sixth,  minor sixth
                , perfect octave]
 
maxAmbitus :: Voice' -> Bool
maxAmbitus v = escursione <= _M10
  where escursione = maximum cf .-. minimum cf
        cf = v ^. valuesV

cf_Climax :: Voice' -> Bool
cf_Climax cf = (==1) . length . filter (== climax) $ cf ^. valuesV
  where climax = maximum $ cf ^. valuesV

cf_NumberOfLeaps :: Voice' -> Bool
cf_NumberOfLeaps cf = (between 2 4) leaps
  where leaps = length . filter isLeap $ cf ^. intervalsV

-- Controlla che non ci siano piu' di due salti piu' grandi di una quarta
cf_LeapsBiggerThanFourth :: Voice' -> Bool
cf_LeapsBiggerThanFourth v = (<= 2) . length . filter (> fourth) . map number $ v ^. intervalsV

-- salti piu' grandi di una terza dovrebbero essere seguiti da
-- un'inversione del movimento, MEGLIO SE PER GRADO CONGIUNTO TODO!!!
cf_LeapThenInversion :: Voice' -> Bool
cf_LeapThenInversion cf = all inversione intornoSalto
  where 
    intornoSalto = filter ((>third) . number . fst) $ zip (cf ^. intervalsV) (tail $ cf ^. intervalsV)
    inversione (a,b) = if a >= perfect unison then b <= perfect unison
                                             else b >= perfect unison

-- No consecutive leaps in the same direction
cf_NoConsecutiveLeapsSameDirection :: Voice' -> Bool
cf_NoConsecutiveLeapsSameDirection cf = all inversione intornoSalto
  where 
    intornoSalto = filter ((>= third) . number . fst) $ zip (cf ^. intervalsV) (tail $ cf ^. intervalsV)
    inversione (a,b) = if a >= perfect unison then b <= perfect unison
                                             else b >= perfect unison

