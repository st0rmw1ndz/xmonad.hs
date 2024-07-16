import Xmobar

config :: Config
config =
  defaultConfig
    { font = "Terminus 8",
      textOffset = 0,
      bgColor = "#121212",
      fgColor = "#cccccc",
      position = Top,
      template = " %XMonadLog% }{ \57958 %cpu% <fc=#777777>|</fc> \61381 %memory% <fc=#777777>|</fc> %load% <fc=#777777>|</fc> %battery% <fc=#777777>|</fc> <fc=#f7e83b>%date%</fc> ",
      commands =
        [ Run XMonadLog,
          Run $ PipeReader "/tmp/pipe-volume" "volume",
          Run $ Com "monitors" ["battery"] "battery" 600,
          Run $ Com "monitors" ["cpu"] "cpu" 50,
          Run $ Com "monitors" ["memory"] "memory" 50,
          Run $ Com "monitors" ["load"] "load" 100,
          Run $ Date "%m/%d/%Y %H:%M:%S" "date" 10
        ]
    }

main :: IO ()
main = xmobar config
