module Mini where

import Music.Prelude

-- Questo modulo ha come scopo il seguente esercizio: data una voce,
-- controllare tutti gli intervalli melodici, e segnare sullo spartito
-- quelli proibiti.

type Counterpoint = Aligned (Voice Pitch)

cantus :: Counterpoint 
cantus = [d, a, g, f, e, d, f, e, d] ^. voice . to (aligned 0 0)

lolus :: Counterpoint -> Score StandardNote
lolus = fmap (fromPitch' . pure) . renderAlignedVoice 

commentus :: Counterpoint -> Score StandardNote
commentus = showAnnotations' "" . annotateSpan (1 <-> 2) "prova" . lolus 

action :: IO ()
action = writeLilypond "cantus.ly" $ commentus cantus
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

permittedMelodicIntervals :: [Interval]
permittedMelodicIntervals = [ perfect unison, major second, minor second
                            , major third,    minor third,  perfect fourth
                            , perfect fifth,  major sixth,  minor sixth
                            , perfect octave]

--intervals :: Counterpoint -> [Interval]
--intervals v = zipWith (.-.) (tail ns) ns
--    where ns = v ^. valuesV




--correctMelodicIntervals :: Counterpoint -> Bool
--correctMelodicIntervals v = all (`elem` permittedMelodicIntervals)
--                                (map abs $ intervals v)
