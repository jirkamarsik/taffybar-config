import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS2

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.PollingLabel

import System.Information.Memory
import System.Information.CPU

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

volCallback :: IO Double
volCallback = do
  muteFile <- readFile "/home/jirka/.mute"
  let muteNum = fromMaybe 0 $ readMaybe muteFile
  let mute = if muteNum == 0 then False else True
  volumeFile <- readFile "/home/jirka/.volume"
  let volumeAbs = fromMaybe 0 $ readMaybe volumeFile
  return $ if mute then 0 else volumeAbs / 65535

fgcolor :: String -> String -> String
fgcolor color contents =
  "<span fgcolor='" ++ color ++ "'>" ++ contents ++ "</span>"

main :: IO ()
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
      volCfg = defaultBarConfig $ const (52 / 255, 187 / 255, 249 / 255)
  let clock = textClockNew Nothing (fgcolor "orange" "%a %b %_d %H:%M") 1
      pager = taffyPagerNew $ defaultPagerConfig { emptyWorkspace = fgcolor "gray" }
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      vol = pollingBarNew volCfg 1 volCallback
      tray = systrayNew
      chosenMonitor = fromMaybe 0 $ readMaybe $ head args
      myTaffybarConfig = defaultTaffybarConfig { startWidgets = [ pager ]
                                               , endWidgets = (if chosenMonitor == 0 then [ tray ] else [])
                                                              ++ [ clock, mem, cpu, vol, mpris ]
                                               , monitorNumber = chosenMonitor
                                               }
  defaultTaffybar myTaffybarConfig
