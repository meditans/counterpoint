import Music.Prelude.Basic hiding  (Interval)
import Music.Pitch.Common.Interval (Interval)

main :: IO ()
main = openLilypond . showAnnotations' ""
     . intervalAnnotations (subjectDiff subject)
     . scat $ map reify subject

subject :: [BasicPitch]
subject = [c, d, f, e, f, g, a, g, e, d, c]

subjectDiff :: CantusFirmus -> [Interval]
subjectDiff = zipWith (.-.) (tail subject)

reify :: BasicPitch -> Score BasicNote
reify = (`up` c) . (.-. c)

intervalAnnotations :: [Interval] -> (Score BasicNote -> Score BasicNote)
intervalAnnotations = foldr1 (.) . zipWith notate (map spanify [0..])
  where
    spanify :: Duration -> Span
    spanify t = (origin .+^ t) >-> 1

    notate :: Span -> Interval -> (Score BasicNote -> Score BasicNote)
    notate s n = annotateSpan s ("       " ++ showIntervalName n)
 
    showIntervalName = filter (/= '_') . show

type CantusFirmus = [BasicPitch]

cf_Lunghezza :: [BasicPitch] -> Bool
cf_Lunghezza = (between 8 16) . length

between s t x = s <= x && x <= t

cf_IntervalliOrizzontali :: CantusFirmus -> Bool
cf_IntervalliOrizzontali cf = all (`elem` consentiti) $ subjectDiff cf
  where
    consentiti = [ perfect unison, major second, minor second, major third, minor third
                 , perfect fourth, perfect fifth, major sixth, minor sixth, perfect octave]

cf_EscursioneMassima :: CantusFirmus -> Bool
cf_EscursioneMassima cf = escursione <= 10
  where escursione = maximum cf .-. minimum cf

cf_Climax :: CantusFirmus -> Bool
cf_Climax cf = (==1) . length . filter (== climax) $ cf
  where climax = maximum cf

cf_NumberOfLeaps :: CantusFirmus -> Bool
cf_NumberOfLeaps cf = (between 2 4) leaps
  where leaps = length . filter isLeap $ subjectDiff cf

-- Controlla che non ci siano piu' di due salti piu' grandi di una quarta
cf_LeapsBiggerThanFourth :: CantusFirmus -> Bool
cf_LeapsBiggerThanFourth = (<= 2) . length . filter (> fourth) . map number . subjectDiff

-- salti piu' grandi di una terza dovrebbero essere seguiti da
-- un'inversione del movimento, MEGLIO SE PER GRADO CONGIUNTO TODO!!!
cf_LeapThenInversion :: CantusFirmus -> Bool
cf_LeapThenInversion cf = all inversione intornoSalto
  where 
    intornoSalto = filter ((>third) . number . fst) $ zip (subjectDiff cf) (tail $ subjectDiff cf)
    inversione (a,b) = if a >= perfect unison then b <= perfect unison
                                             else b >= perfect unison

-- No consecutive leaps in the same direction
cf_NoConsecutiveLeapsSameDirection :: CantusFirmus -> Bool
cf_NoConsecutiveLeapsSameDirection cf = all inversione intornoSalto
  where 
    intornoSalto = filter ((>= third) . number . fst) $ zip (subjectDiff cf) (tail $ subjectDiff cf)
    inversione (a,b) = if a >= perfect unison then b <= perfect unison
                                             else b >= perfect unison
