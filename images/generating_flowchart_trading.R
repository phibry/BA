##### Flowchart Trading ###### 

#install.packages("DiagrammeR")
library(DiagrammeR)

# Documentation: https://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html
grViz(diagram = "digraph flowchart {
  graph [compound = true, nodesep = 0.7, ranksep = 0.75, rankdir = LR]
  node [fontname = arial, shape = rectangle, color = Black, style = 'solid', fixedsize = true, width = 4]
  tab1 [label = '@@1']
  tab2 [label = '@@2']
  tab3 [label = '@@3']
  tab4 [label = '@@4']
  tab5 [label = '@@5']
  tab6 [label = '@@6']

  tab1 -> tab3 -> tab6;
  tab2 -> tab3;
  tab4 -> tab5 -> tab6;
}
  [1]: 'Prediction of log returns with neural network'
  [2]: 'Check stability with XAI'
  [3]: 'Trading output for neural network'    
  [4]: 'Prediction of volatility with (1,1)-GARCH' 
  [5]: 'Trading output for GARCH'
  [6]: 'Trading outputs combined'
  ")

