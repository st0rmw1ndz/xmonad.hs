import Xmobar

config :: Config
config =
  defaultConfig
    { font = "Terminus 8",
      textOffset = 0,
      bgColor = "#121212",
      fgColor = "#cccccc",
      position = Top,
      template = " %XMonadLog% }{ %volume% <fc=#777777>|</fc> %load% <fc=#777777>|</fc> %battery% <fc=#777777>|</fc> <fc=#f7e83b>%date%</fc> ",
      commands =
        [ Run XMonadLog,
          Run $ Com "monitors" ["battery"] "battery" 1200,
          Run $ Com "monitors" ["cpu"] "cpu" 100,
          Run $ Com "monitors" ["memory"] "memory" 100,
          Run $ Com "monitors" ["load"] "load" 100,
          Run $ PipeReader "/tmp/pipe-volume" "volume",
          Run $ Date "%m/%d/%Y %-I:%M %p" "date" 600
        ]
    }

main :: IO ()
main = xmobar config
