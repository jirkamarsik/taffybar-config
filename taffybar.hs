import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS2

import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

fgcolor :: String -> String -> String
fgcolor color contents =
  "<span fgcolor='" ++ color ++ "'>" ++ contents ++ "</span>"

main = do
  args <- getArgs
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
                                                              ++ [ clock, mem, cpu, mpris ]
                                               , monitorNumber = chosenMonitor
                                               }
  defaultTaffybar myTaffybarConfig
