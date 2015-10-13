# Install #

  1. Download [![](http://www.r-project.org/Rlogo.jpg)](http://www.r-project.org/)
  1. Start R
  1. Go to **File->Change Dir** and point to the folder containing the scripts


# Usage #

**in the console**

`>source("RMT4-Back_StrategyAnalyzer.R")`

_To analyze one statement_:

`>result = MT4_StratRep_Analyzer("example_StrategyTester/INDRAFXSCALPING_V4.2 EU M5 2009.htm")`

_To analyze mutilple statement_:

`>result = MT4_StratRep_Analyzer("example_StrategyTester/Stat_Euclidean_Metric (Modified) EU H1 2-2.htm", "example_StrategyTester/Slope Direction Line EA EU H4.htm", "example_StrategyTester/INDRAFXSCALPING_V4.2 EU M5 2009.htm")`

# Output Object Format #

_Accessing the final balance_

`output = MT4_StratRep_Analyzer("example_StrategyTester/Stat_Euclidean_Metric (Modified) EU H1 2-2.htm", "example_StrategyTester/INDRAFXSCALPING_V4.2 EU M5 2009.htm")`

`> output[[3,2]]$Balance[length(output[[3,2]]$Balance)]`
  * > 25573.19

`> output[[3,1]]$Balance[length(output[[3,1]]$Balance)]`
  * > "74587.88"

`> output[[3,3]]$CumulativeBalance[length(output[[3,3]]$CumulativeBalance)]`
  * > (07/30/09 13:30:00) 90160.85

`> 25573.19+74585.88-10000`
  * > 90159.07

`> 90160.85-90159.07`
  * > 1.78 _corresponds to the error of Strategy Tester ending balance and the one of the cumulated testers. I have no explanation_