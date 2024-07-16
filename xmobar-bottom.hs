import Xmobar

config :: Config
config =
  defaultConfig
    { font = "Terminus 8",
      textOffset = 0,
      bgColor = "#121212",
      fgColor = "#cccccc",
      position = Bottom,
      template = " %disk-root%  %disk-home% <fc=#777777>|</fc> %local-ip% <fc=#777777>|</fc> %ssid% }{ %now-playing% <fc=#777777>|</fc> %volume% ",
      commands =
        [ Run XMonadLog,
          Run $ Com "monitors" ["local_ip"] "local-ip" 6000,
          Run $ Com "monitors" ["ssid"] "ssid" 6000,
          Run $ Com "monitors" ["disk", "/"] "disk-root" 6000,
          Run $ Com "monitors" ["disk", "/home"] "disk-home" 6000,
          Run $ PipeReader "/tmp/pipe-volume" "volume",
          Run $ PipeReader "/tmp/pipe-now-playing" "now-playing"
        ]
    }

main :: IO ()
main = xmobar config
