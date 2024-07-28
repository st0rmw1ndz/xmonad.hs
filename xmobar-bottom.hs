import Xmobar

config :: Config
config =
  defaultConfig
    { font = "Terminus 8",
      textOffset = 0,
      bgColor = "#121212",
      fgColor = "#cccccc",
      position = Bottom,
      template = " %disk-root% <fc=#777777>|</fc> %disk-home% <fc=#777777>|</fc> %status-redshift% <fc=#777777>|</fc> %status-udiskie% }{ %now-playing% <fc=#777777>|</fc> %volume% ",
      commands =
        [ Run XMonadLog,
          Run $ Com "monitors" ["disk", "/"] "disk-root" 35000,
          Run $ Com "monitors" ["disk", "/home"] "disk-home" 35000,
          Run $ Com "monitors" ["service", "redshift"] "status-redshift" 6000,
          Run $ Com "monitors" ["service", "udiskie"] "status-udiskie" 6000,
          Run $ PipeReader "/tmp/pipe-now-playing" "now-playing",
          Run $ PipeReader "/tmp/pipe-volume" "volume"
        ]
    }

main :: IO ()
main = xmobar config
