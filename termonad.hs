{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE RecordWildCards   #-}
-- {-# OPTIONs_GHC -Wall #-}

module Main where

import           Termonad.App
import           Termonad.Config
import           Termonad.Config.Colour
import           Termonad.Types
import           Termonad.Keys
import           Control.Concurrent.MVar
import           Control.Monad.Trans                       (lift)
import           Control.Monad.Trans.Maybe
import qualified Data.Colour                as Colour
import qualified Data.Colour.SRGB           as SRGB
import           Data.Functor                              (void)
import           Data.Maybe                                (fromMaybe,listToMaybe)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           GHC.Word                                  (Word8)
import qualified GI.Gdk.Objects.Screen      as Screen
import qualified GI.Gdk.Structs.EventKey    as EventKey
import qualified GI.Gdk.Flags               as Flags
import qualified GI.Gdk.Structs.RGBA        as RGBA
import qualified GI.Gio.Objects.Cancellable as Cancellable
import qualified GI.GLib.Flags              as Flags
import qualified GI.Gtk                     as Gtk
import qualified GI.Vte.Flags               as Flags
import qualified GI.Vte.Objects.Terminal    as Terminal
import Numeric                                             (readHex)
import System.Directory                                    (setCurrentDirectory)

import Options.Applicative

readHexColourCode :: ReadS (Word8, Word8, Word8, Word8)
readHexColourCode ('#':cc)               = readHexColourCode cc
readHexColourCode (r1:r2:g1:g2:b1:b2:xs) = do
  r <- readHex' [r1,r2]
  g <- readHex' [g1,g2]
  b <- readHex' [b1,b2]
  let (remain, o) = opacity
  return ((r, g, b, o), remain)
  where
    readHex' :: String -> [Word8]
    readHex' = fmap fst . filter ((== "") . snd) . readHex
    opacity :: (String, Word8)
    opacity = fromMaybe 255 . listToMaybe <$> case xs of
      o1:o2:remain -> (remain, readHex' [o1,o2])
      remain       -> (remain, [])
readHexColourCode _ = []

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

bgBaseColor :: Colour.Colour Double
bgBaseColor = SRGB.sRGB24 17 17 17

bgDefOpacity :: Double
bgDefOpacity = 0.7

colConf :: ColourConfig (AlphaColour Double)
colConf = defaultColourConfig
  { foregroundColour = Set $ opaque $ SRGB.sRGB24 227 234 252
  , backgroundColour = Set $ bgBaseColor `Colour.withOpacity` bgDefOpacity
  , palette          = fromMaybe NoPalette $ ExtendedPalette <$> standard <*> extended
  } where
    fromHexCC :: String -> Maybe (AlphaColour Double)
    fromHexCC = fmap (uncurry4 sRGB32 . fst) . listToMaybe . readHexColourCode
    standard :: Maybe (List8 (AlphaColour Double))
    standard = mkList8 =<< sequence
      [ fromHexCC "#263238"
      , fromHexCC "#FF5252"
      , fromHexCC "#68F3C9"
      , fromHexCC "#FEE94E"
      , fromHexCC "#2BCFF0"
      , fromHexCC "#F02BA2"
      , fromHexCC "#68B6F3"
      , fromHexCC "#ECEFF1"
      ]
    extended :: Maybe (List8 (AlphaColour Double))
    extended = mkList8 =<< sequence
      [ fromHexCC "#525252"
      , fromHexCC "#FF7281"
      , fromHexCC "#68F3C9"
      , fromHexCC "#FEE94E"
      , fromHexCC "#2BCFF0"
      , fromHexCC "#F02BA2"
      , fromHexCC "#68B6F3"
      , fromHexCC "#FFFFFF"
      ]

fontConf :: FontConfig
fontConf = defaultFontConfig
  { fontFamily = "Iosevka Custom"
  , fontSize   = FontSizePoints 12
  }

instance Semigroup ConfigHooks where
  ConfigHooks f <> ConfigHooks g = ConfigHooks $ \mvarTMState vteTerm -> do
    f mvarTMState vteTerm
    g mvarTMState vteTerm

instance Monoid ConfigHooks where
  mempty = ConfigHooks $ \_ _ -> return ()

mkTransparent :: ConfigHooks
mkTransparent = ConfigHooks $ \mvarTMState _ -> do
  win <- tmStateAppWin <$> readMVar mvarTMState
  Gtk.widgetSetAppPaintable win True
  Gtk.widgetSetVisual win =<< runMaybeT do
    screen <- MaybeT Screen.screenGetDefault
    lift $ setMenubarColor screen
    MaybeT $ Screen.screenGetRgbaVisual screen
  where
    setMenubarColor :: Screen.Screen -> IO ()
    setMenubarColor screen = do
      prv <- Gtk.cssProviderNew
      Gtk.cssProviderLoadFromData prv "menubar { background-color: #ffffff; }"
      let priority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_FALLBACK
      Gtk.styleContextAddProviderForScreen screen prv priority

setAllowBold :: Bool -> ConfigHooks
setAllowBold b = ConfigHooks $ \_ vteTerm -> Terminal.setTerminalAllowBold vteTerm b

setDefaultWinTitle :: T.Text -> ConfigHooks
setDefaultWinTitle title = ConfigHooks $ \mvarTMState _ -> do
  win <- tmStateAppWin <$> readMVar mvarTMState
  void $ Gtk.onWidgetShow win $ Gtk.windowSetTitle win title

execCommand :: Maybe String -> ConfigHooks
execCommand = maybe mempty $ \cmd -> ConfigHooks $ \_ vteTerm -> do
  void $ Terminal.terminalSpawnAsync
    vteTerm
    [Flags.PtyFlagsDefault]
    Nothing
    ["/usr/bin/env", "bash", "-c", cmd]
    Nothing
    ([Flags.SpawnFlagsDefault] :: [Flags.SpawnFlags])
    Nothing
    (-1)
    (Nothing :: Maybe Cancellable.Cancellable)
    Nothing

changeDir :: Maybe String -> ConfigHooks
changeDir = maybe mempty $ \dir -> ConfigHooks $ \mvarTMState vteTerm -> do
  setCurrentDirectory dir

colourToRgba :: Colour.Colour Double -> Double -> IO RGBA.RGBA
colourToRgba colour alpha' = do
  let SRGB.RGB red green blue = SRGB.toSRGB colour
  let alpha = case () of
        _ | alpha' < 0 -> 0
        _ | alpha' > 1 -> 1
        _              -> alpha'
  rgba <- RGBA.newZeroRGBA
  RGBA.setRGBARed   rgba red
  RGBA.setRGBAGreen rgba green
  RGBA.setRGBABlue  rgba blue
  RGBA.setRGBAAlpha rgba alpha
  return rgba

opacityDiff :: Double
opacityDiff = 0.025

bgOpacityInc :: Terminal.Terminal -> Colour.Colour Double -> IO Bool
bgOpacityInc vteTerm base = do
  opacity <- RGBA.getRGBAAlpha =<< Terminal.terminalGetColorBackgroundForDraw vteTerm
  colourToRgba base (opacity + opacityDiff)
    >>= Terminal.terminalSetColorBackground vteTerm
  return True

bgOpacityDec :: Terminal.Terminal -> Colour.Colour Double -> IO Bool
bgOpacityDec vteTerm base = do
  opacity <- RGBA.getRGBAAlpha =<< Terminal.terminalGetColorBackgroundForDraw vteTerm
  colourToRgba base (opacity - opacityDiff)
    >>= Terminal.terminalSetColorBackground vteTerm
  return True

bgOpacityIni :: Terminal.Terminal -> Colour.Colour Double -> IO Bool
bgOpacityIni vteTerm base = do
  colourToRgba base bgDefOpacity >>= Terminal.terminalSetColorBackground vteTerm
  return True

setCustomKeyMappings :: ConfigHooks
setCustomKeyMappings = ConfigHooks $ \mvarTMState vteTerm -> do
  void $ Gtk.onWidgetKeyPressEvent vteTerm $ \eventKey -> do
    keyval    <- EventKey.getEventKeyKeyval eventKey
    modifiers <- EventKey.getEventKeyState  eventKey
    let key = removeStrangeModifiers $ toKey keyval (Set.fromList modifiers)
    -- print key
    case key of
      Key 125 md | ctrlShft `Set.isSubsetOf` md -> bgOpacityInc vteTerm bgBaseColor
      Key 123 md | ctrlShft `Set.isSubsetOf` md -> bgOpacityDec vteTerm bgBaseColor
      Key  96 md | ctrlShft `Set.isSubsetOf` md -> bgOpacityIni vteTerm bgBaseColor
      _ -> return False
      where
        ctrlShft :: Set.Set Flags.ModifierType
        ctrlShft = Set.fromList [ Flags.ModifierTypeShiftMask
                                , Flags.ModifierTypeControlMask
                                ]

data CLArgs = CLArgs
  { title :: T.Text
  , mbCmd :: Maybe String
  }

clArgsParser :: Parser CLArgs
clArgsParser = CLArgs
  <$> strOption ( long    "title"
               <> short   't'
               <> help    "Window title"
               <> metavar "TITLE"
               <> value   "termonad_custom")
  <*> optional (strOption $ long    "exec"
                         <> short   'e'
                         <> help    "Command to execute"
                         <> metavar "COMMAND")

main :: IO ()
main = do
  CLArgs {..} <- execParser $ info (clArgsParser <**> helper) fullDesc
  colExt      <- createColourExtension colConf
  let termonadConf = TMConfig
        { options = defaultConfigOptions
            { fontConfig      = fontConf
            , confirmExit     = False
            , showMenu        = False
            , showScrollbar   = ShowScrollbarNever
            , cursorBlinkMode = CursorBlinkModeOff
            }
        , hooks = mconcat
            [ mkTransparent
            , setAllowBold False
            , setDefaultWinTitle title
            -- , changeDir $ Just "/home/"
            , execCommand mbCmd
            , setCustomKeyMappings
            ]
        }
        `addColourExtension` colExt
  -- defaultMain termonadConf
  start termonadConf
