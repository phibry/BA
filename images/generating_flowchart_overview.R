##### Flowchart Methodology ###### 

#install.packages("DiagrammeR")
library(DiagrammeR)

# Documentation: https://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html
grViz(diagram = "digraph flowchart {
  node [fontname = arial, shape = oval, color = Black, style = 'solid']
  tab1 [label = '@@1']
  tab2 [label = '@@2']
  tab3 [label = '@@3', shape = diamond]
  tab4 [label = '@@4']
  tab5 [label = '@@5']

  tab1 -> tab2 -> tab3 -> tab4 -> tab5;
  tab4 -> tab2 [label = '  if prediction inaccurate'];
}
  [1]: '1) Data exploration'
  [2]: '2) Selection of a suitable neural network'
  [3]: '3) Predictive accuracy'    
  [4]: '4) Explainable artificial intelligence (XAI)' 
  [5]: '5) Implementation trading strategy'
  ")
