import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS2

import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.PollingLabel

import System.Information.Memory
import System.Information.CPU

import qualified Graphics.UI.Gtk as Gtk

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Process (readProcess)
import Text.Read (readMaybe)

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

kbdCallback :: IO String
kbdCallback = do
  stdOut <- readProcess "xkb-switch" ["-p"] ""
  let layoutVariant = head $ lines stdOut
  let layout = takeWhile (/= '(') layoutVariant
  return layout

fgcolor :: String -> String -> String
fgcolor color contents =
  "<span fgcolor='" ++ color ++ "'>" ++ contents ++ "</span>"

main :: IO ()
main = do
  args <- getArgs
  kbd <- pollingLabelNew "us" 1 kbdCallback
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
  let clock = textClockNew Nothing (fgcolor "orange" "%a %b %_d %H:%M") 1
      pager = taffyPagerNew $ defaultPagerConfig { emptyWorkspace = fgcolor "gray" }
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      chosenMonitor = fromMaybe 0 $ readMaybe $ head args
      myTaffybarConfig = defaultTaffybarConfig { startWidgets = [ pager ]
                                               , endWidgets = (if chosenMonitor == 0 then [ tray ] else [])
                                                              ++ [ return kbd, clock, mem, cpu, mpris ]
                                               , monitorNumber = chosenMonitor
                                               }
  Gtk.widgetShowAll kbd
  defaultTaffybar myTaffybarConfig
