##### Flowchart Methodology ###### 

#install.packages("DiagrammeR")
library(DiagrammeR)

# Documentation: https://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html
grViz(diagram = "digraph flowchart {
  node [fontname = arial, shape = oval, color = Black, style = 'solid']
  tab1 [label = '@@1']
  tab2 [label = '@@2']
  tab3 [label = '@@3']
  tab4 [label = '@@4']
  tab5 [label = '@@5', shape = diamond]
  tab6 [label = '@@6']

  tab1 -> tab2 -> tab3 -> tab4 -> tab5 -> tab6;
  tab5 -> tab3 [label = '  if prediction inaccurate'];
}
  [1]: 'Data exploration'
  [2]: 'Definition of time splits'
  [3]: 'Evaluation of different network architectures'    
  [4]: 'Selection of a suitable neural network' 
  [5]: 'Conduct check for predictive accuracy'
  [6]: 'Implementation of trading strategy'
  ")

