module GlossyRuler where
import Graphics.Gloss

drawRuler :: (Int, Int) -> (Int, Int) -> Int -> Int -> Color -> Color -> Color -> Picture
drawRuler position dimensions numTickMarks masurement baseColor tickColor messurementColor = 
     pictures $ map (translate ipos) [base tickMarks massurement] 
    where
        ipos = position * (-1)
        dimx = fst dimensions
        dimy = snd dimensions
        base = color baseColor $ rectangleSolid dimensions
        tick = color tickColor $ rectangleSolid (floor (dimx / numTickMarks * 0.1), floor (dimy / numTickMarks * 0.1))
        tickStep = dimy / numTickMarks
        tickSteps = [tickStep .. tickStep * numTickMarks]
        tickMarks = map (\y -> translate 0 y tick) tickSteps
        massurement = translate (0, dimy) $ translate (0, -massurement) $ color massurementColor $ tick

