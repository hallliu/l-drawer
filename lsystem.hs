--lsystem.hs
--Author: Hall Liu
--Email: hallliu000@gmail.com
--
--Lindenmayer system visualizer based on material from lab 7 in CMSC16200

module Main where
import Text.ParserCombinators.ReadP
import System.IO
import System.Environment
import Data.Char
import qualified Data.Map as Map
import Data.List

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Matrix

data Lsystem = Lsystem {
    symbols :: [Char],
    rules :: Ruleset
}
type Ruleset = Map.Map Char [Char]

newtype ConfigData = ConfigData {
    mapfunc :: Map.Map String ConfigVal
} deriving Show

data ConfigVal = StringVal String
               | IntVal Int
               | FloatVal Double
    deriving Show

data DrawCommand = Forward Double
                 | Turn Double
                 | Push
                 | Pop
    deriving Show

data GfxState = GfxState {
    xPos :: Double,
    yPos :: Double,
    heading :: Double
} deriving Show

fread :: String -> Double --workaround to the fact that read for Double doesn't recognize strings terminating in a dot
fread s
    | '.' == last s = read $ init s
    | otherwise = read s


instance Read ConfigVal where
    --TODO: implement a good substitute for <++ that allows the commented-out code to work properly.
    --As of now, the int parser returns some results which doesn't allow <++ to pass to the double
    --parser when it hits a dot.
    readsPrec _ s
        --all digits
        | foldl (\x y -> isDigit y && x) True s = readP_to_S (int >>= (\x -> return (IntVal x))) s
        --digits with a dot
        | foldl (\x y -> (isDigit y || y == '.') && x) True s = readP_to_S (dec >>= (\x -> return (FloatVal x))) s
        --other characters
        | otherwise = readP_to_S (str >>= (\x -> return (StringVal x))) s
            where int = fmap read $ many1 (satisfy isDigit)
                  dec = fmap fread $ many1 (satisfy isDigit +++ char '.')
                  str = many1 (satisfy (\x -> not $ isSpace x))

    {-readP_to_S $
        (int >>= (\x -> return (IntVal x))) <++ (dec >>= (\x -> return (FloatVal x)))
            <++ (str >>= (\x -> return (StringVal x)))
            where int = fmap read $ many1 (satisfy isDigit)
                  dec = fmap fread $ many1 (satisfy isDigit +++ char '.')
                  str = many1 (satisfy (\x -> not $ isSpace x))-}

instance Read ConfigData where
    readsPrec _ = readP_to_S $ do
        param <- str1
        skipSpaces
        char '='
        skipSpaces
        val <- str2
        return (ConfigData $ Map.singleton param val)
            where str1 = many1 $ satisfy isAlpha
                  -- 'read' in next line is the read of ConfigVal
                  str2 = fmap read $ many1 $ satisfy (\x -> not $ isSpace x)

--Expand out the L-system to a specified depth using the internal rules
--If no rule found for a character, leave it alone (the terminals)
lExpand :: Lsystem -> Int -> Lsystem
lExpand ls 0 = ls
lExpand (Lsystem str rules) depth = lExpand (Lsystem str' rules) (depth-1)
    where str' = concat $ map (\x -> Map.findWithDefault [x] x rules) str

--two functions to split a string along a character
splitAlong :: Eq a => a -> [a] -> [[a]]
splitAlong s as = filter (not.null) $ reverse (map reverse (rSplitAlong [[]] s as))

rSplitAlong :: Eq a => [[a]] -> a -> [a] -> [[a]]
rSplitAlong (r:results) s (a:as)
    | s == a = rSplitAlong ([]:r:results) s as
    | True = rSplitAlong ((a:r):results) s as
rSplitAlong r _ [] = r

main ::IO ()
main = do
    args <- getArgs
    lSpecs <- readFile $ head args
    let specSectionLines = splitAlong "%%" $ splitAlong '\n' lSpecs
    let configInfo = Map.unions (map (mapfunc.read) $ head specSectionLines)

    --start making our L-system
    let ls = makeLsystem configInfo (specSectionLines !! 1)

    let (IntVal expansion) = Map.findWithDefault (IntVal 0) "limit" configInfo
    let s2 = lExpand ls expansion
    -- convert to a list of draw commands
    let drawTable = makeDrawTable (specSectionLines !! 2) configInfo
    let commandList = concat $ map ((Map.!) drawTable) (symbols s2)
    --GUI code
    initGUI
    window <- windowNew
    da <- drawingAreaNew
    windowSetDefaultSize window 700 700
    onExpose da $ const (updateWindow da commandList)
    onDestroy window mainQuit
    set window [containerChild := da]
    widgetShowAll window
    mainGUI

makeLsystem :: Map.Map String ConfigVal -> [String] -> Lsystem
makeLsystem configs rules = Lsystem startStr (recMakeLfn rules)
    where StringVal startStr = (Map.findWithDefault (StringVal "") "start" configs)

--since the format of the rule specs isn't that complex, opted to just parse it with list ops instead
--of parser combinators
recMakeLfn :: [String] -> Ruleset
recMakeLfn [] = Map.empty
recMakeLfn (s:ss) = Map.insert (head s) (filter (\x -> not $ (isSpace x || x == ';')) (drop 4 s)) (recMakeLfn ss)

--takes the third part of the config file and converts it into a map of L-system characters to drawing commands
makeDrawTable :: [String] -> Map.Map String ConfigVal -> Map.Map Char [DrawCommand]
--this takes care of our defined terminals
makeDrawTable [] configs = Map.unions [Map.singleton '+' [(Turn x)],
                                       Map.singleton '-' [(Turn (-x))],
                                       Map.singleton '[' [Push],
                                       Map.singleton ']' [Pop]]
                               where x' = Map.findWithDefault (FloatVal 60) "angle" configs
                                     x = case x' of
                                             FloatVal t -> t
                                             IntVal t -> fromIntegral t :: Double

makeDrawTable (s:ss) configs = Map.insert (head s) (fromTList s) (makeDrawTable ss configs)

fromTList :: String -> [DrawCommand]
--kludgey. Format is (character)=>"(command);(command);...;(command)";
fromTList s = map turtleToDrawCmd $ splitAlong ';' ((splitAlong '"' s) !! 1)

turtleToDrawCmd :: String -> DrawCommand
turtleToDrawCmd s = case (head sParts) of
                        "forward" -> Forward (read $ sParts !! 1)
                        "right" -> Turn (read $ sParts !! 1)
                        "left" -> Turn (-(read $ sParts !! 1))
                        "push" -> Push
                        "pop" -> Pop
                        where sParts = splitAlong ' ' s

--The code for drawing the DrawCommands with cairo
updateWindow :: DrawingArea -> [DrawCommand] -> IO Bool
updateWindow da cmds = do
    dWindow <- widgetGetDrawWindow da
    (width,height) <- widgetGetSize da
    renderWithDrawable dWindow (paintCommands cmds width height)
    return True

paintCommands :: [DrawCommand] -> Int -> Int -> Render ()
paintCommands cmds width height= do
    --set some scaling factors
    let (lineLength, xOff, yOff) = findNormalizations cmds width height
    --set the style
    setLineCap LineCapRound
    setLineJoin LineJoinBevel
    setSourceRGBA 0.6 0.6 0.1 1
    --flip the axes to correspond to regular cartesian
    transform $ Matrix 1 0 0 (-1) 0 (fromIntegral $ height) 
    paintCmd cmds lineLength [GfxState xOff yOff 90]

paintCmd :: [DrawCommand] -> Double -> [GfxState] -> Render ()
paintCmd (cmd:cmds) lineLength (s:ss) = case cmd of
    Forward l -> do
                    moveTo (xPos s) (yPos s)
                    let newX = xPos s + l*lineLength*(cos (pi*(heading s)/180.0))
                    let newY = yPos s + l*lineLength*(sin (pi*(heading s)/180.0))
                    lineTo newX newY
                    stroke
                    paintCmd cmds lineLength ((GfxState newX newY (heading s)):ss)
    Turn a -> do
                    let s' = GfxState {xPos = xPos s, yPos = yPos s, heading = heading s + a}
                    paintCmd cmds lineLength (s':ss)
    Push -> do
                    paintCmd cmds lineLength (s:s:ss)
    Pop -> do
                    paintCmd cmds lineLength ss

paintCmd [] _ _ = return ()


--tools to normalize the drawing to fit in the window
initGfxState :: GfxState
initGfxState = GfxState {xPos = 0, yPos = 0, heading = 90}

findNormalizations :: [DrawCommand] -> Int -> Int -> (Double, Double, Double)
findNormalizations cmds width height = (lineLength, xOff, yOff)
    where lineLength = min ((fromIntegral width)/(xMax-xMin)) ((fromIntegral height)/(yMax-yMin))
          xOff = (-xMin)*lineLength + (fromIntegral width - (xMax-xMin)*lineLength)*0.5
          yOff = (-yMin)*lineLength + (fromIntegral height - (yMax-yMin)*lineLength)*0.5
          ((xMin,xMax),(yMin,yMax)) = recFindLimits cmds [initGfxState] ((0,0),(0,0))

recFindLimits :: [DrawCommand] -> [GfxState] -> ((Double,Double),(Double,Double)) -> ((Double,Double),(Double,Double))
recFindLimits [] _ bounds = bounds
recFindLimits (cmd:cmds) states ((xMin,xMax),(yMin,yMax)) = recFindLimits cmds states' ((xMin',xMax'),(yMin',yMax'))
    where (s:ss) = states
          states' = case cmd of
                        Push -> s:s:ss
                        Pop -> ss
                        _ -> s':ss
          s' = cmdTransform s cmd
          xMin' = min xMin (xPos s')
          xMax' = max xMax (xPos s')
          yMin' = min yMin (yPos s')
          yMax' = max yMax (yPos s')

cmdTransform :: GfxState -> DrawCommand -> GfxState
cmdTransform s (Forward x) = GfxState {xPos = xPos s + x*(cos (pi*(heading s)/180.0)),
                                       yPos = yPos s + x*(sin (pi*(heading s)/180.0)),
                                       heading = heading s}
cmdTransform s (Turn x) = GfxState {xPos = xPos s, yPos = yPos s, heading = heading s + x}
cmdTransform s _ = s

instance Show Lsystem where
    show (Lsystem s f) = show s
