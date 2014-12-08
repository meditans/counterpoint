import Music.Prelude
import Utils

main :: IO ()
main = do
  -- writeLilypond "test.ly" cantus
  return ()

type Voice' = Voice Pitch

cantus :: Voice' 
cantus = [d, a, g, f, e, d, f, e, d] ^. voice
         
allWholeNotes :: Voice' -> Bool
allWholeNotes v = all (== 1) (v ^. durationsV)

voiceLength :: Voice' -> Bool
voiceLength v = (between 8 16) (v ^. notes . to length)

intervals :: Voice' -> [Interval]
intervals v = zipWith (.-.) (tail ns) ns
    where ns = v ^. valuesV

permittedMelodicIntervals :: [Interval]
permittedMelodicIntervals = [ perfect unison, major second, minor second
                            , major third,    minor third,  perfect fourth
                            , perfect fifth,  major sixth,  minor sixth
                            , perfect octave]

correctMelodicIntervals :: Voice' -> Bool
correctMelodicIntervals v = all (`elem` permittedMelodicIntervals)
                                (map abs $ intervals v)
 
maxAmbitus :: Voice' -> Bool
maxAmbitus v = escursione <= _M10
  where escursione = maximum cf .-. minimum cf
        cf = v ^. valuesV

cf_Climax :: Voice' -> Bool
cf_Climax cf = (==1) . count (== climax) $ cf ^. valuesV
    where climax = cf ^. valuesV . to maximum

numberOfLeaps :: Voice' -> Bool
numberOfLeaps = (between 2 4) . leaps
  where leaps = count (> second) . map number . intervals

-- Diciamo che invece il punto e' che voglio focalizzare tutti i leap,
-- e contarli in un secondo momento. Come faccio a fare questo?

-- Controlla che non ci siano piu' di due salti piu' grandi di una quarta
cf_LeapsBiggerThanFourth :: Voice' -> Bool
cf_LeapsBiggerThanFourth v = (<= 2) . count (> fourth) . map number $ intervals $ v

-- salti piu' grandi di una terza dovrebbero essere seguiti da
-- un'inversione del movimento, MEGLIO SE PER GRADO CONGIUNTO TODO!!!
cf_LeapThenInversion :: Voice' -> Bool
cf_LeapThenInversion cf = all inversione intornoSalto
  where 
    intornoSalto = filter ((>third) . number . fst) . (zip <*> tail) . intervals $ cf
    inversione (a,b) = if a >= perfect unison then b <= perfect unison
                                              else b >= perfect unison

-- No consecutive leaps in the same direction
cf_NoConsecutiveLeapsSameDirection :: Voice' -> Bool
cf_NoConsecutiveLeapsSameDirection cf = undefined

-- Il vero punto di questa funzione e': come posso otterenere una
-- lista di intervalli consecutivi? Di salti consecutivi? Qual e' il

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Dobbiamo esplorare [(Aligned (Voice a))]. Questo ci permette
-- infatti anche un orientamento temporale. Allora
