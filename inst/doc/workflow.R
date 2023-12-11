## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, fig.align="center", out.width="85%"--------------------------
knitr::include_graphics("workflow.png")

## ----echo=FALSE, eval=FALSE---------------------------------------------------
#  g <- DiagrammeR::grViz("
#  digraph boxes_and_diamonds {
#  
#    # a 'graph' statement
#    graph [ordering = 'in']
#  
#    # several 'node' statements
#    node [fontsize = '14', fontname = 'Arial']
#  
#    node [shape = box, style = filled, fillcolor = '#d9d2e9'] 'Save analysis\n summary'; 'Cut data\n for analysis'; 'Analyze data';
#  
#    node [shape = diamond, style = filled, fillcolor = '#fff2cc'] 'Final analysis?'
#    node [shape = diamond, style = filled, fillcolor = '#fff2cc'] 'Finished with\n all trials?'
#  
#    node [shape = box, style = filled, fillcolor = '#e6b8af', label = 'Summarize across\n trials'] summarize
#  
#    node [shape = box, style = filled, fillcolor = '#d9ead3', label = 'Scenario'] scenario
#    node [shape = box, style = filled, fillcolor = '#cfe2f3', label = 'Generate enrollment\n times'] enrollment
#    node [shape = box, style = filled, fillcolor = '#cfe2f3', label = 'Generate strata'] strata
#    node [shape = box, style = filled, fillcolor = '#cfe2f3', label = 'Randomize patients'] randomize
#    node [shape = box, style = filled, fillcolor = '#cfe2f3', label = 'Generate outcomes'] outcomes
#    node [shape = box, style = filled, fillcolor = '#cfe2f3', label = 'Generate next\n trial'] next
#  
#    scenario -> enrollment -> strata -> randomize -> outcomes
#    outcomes -> 'Cut data\n for analysis' -> 'Analyze data'->'Save analysis\n summary'
#    'Save analysis\n summary' -> 'Final analysis?'
#    'Final analysis?' -> 'Cut data\n for analysis' [label = 'No', fontname = 'Arial']
#    'Final analysis?' -> 'Finished with\n all trials?' [label = 'Yes', fontname = 'Arial']
#    'Finished with\n all trials?' -> summarize [label = 'Yes', fontname = 'Arial']
#    'Finished with\n all trials?' -> next [label = 'No', fontname = 'Arial']
#  }
#  ")
#  
#  rsvg::rsvg_png(charToRaw(DiagrammeRsvg::export_svg(g)), file = file.path(tempdir(), "workflow.png"))

