Config { font = "xft:Cousine:pixelsize=13"
       , bgColor = "black"
       , fgColor = "grey"
       , position = TopW L 100
       , lowerOnStart = True
       , commands = [   Run Weather "KPAO" ["-t","<tempF>F <skyCondition>","-L","64","-H","77","-n","#CEFFAC","-h","#FFB6B0","-l","#96CBFE"] 36000
                      , Run MultiCpu ["-t","Cpu: <total0> <total1> <total2> <total3>","-L","30","-H","60","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC","-w","3"] 10
                      , Run Memory ["-t","Mem: <usedratio>%","-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10
                      , Run Network "en0s31f6" ["-t","Net: <rx>, <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10
                      , Run Date "%a %b %_d %l:%M" "date" 10
                      , Run Volume
                      , Run UnsafeStdinReader
       ]
       , sepChar = "%",
       , alignSep = "}{",
       , template = "%UnsafeStdinReader% }{ %multicpu%   %memory%   %swap%  %en0s31f6%   Vol: <fc=#b2b2ff>%volume%</fc>   <fc=#FFFFCC>%date%</fc>"
}
