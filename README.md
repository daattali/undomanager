# undomanager

General framework to manage the history of any object, allowing undo and redo operations.

### Example

```r
nums <- UndoManager$new("numeric")
nums$do(5)
nums$do(7)
nums$do(10)
nums$do(12)
nums$undo()
nums$undo()
nums$redo()
print(nums)
```

```
<UndoManager> of items of type <numeric> with 2 undos and 1 redo

### Current item ###
10 

### Undo stack ###
1. 7 
2. 5 

### Redo stack ###
1. 12 
```

You can also chain all the operations; the above is equivalent to:

```r
UndoManager$new("numeric")$do(5)$do(7)$do(10)$do(12)$undo()$undo()$redo()
```

### Using with shiny

{undomanager} can also be fully reactive and integrate with shiny smoothly. You just need to call `$reactive()` on the UndoManager object and use it as a reactive variable:

```r
library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  numericInput("num", "Choose a number", 5),
  actionButton("save", "Save"),
  actionButton("undo", NULL, icon = icon("undo"), title = "Undo"),
  actionButton("redo", NULL, icon = icon("redo"), title = "Undo"),
  actionButton("clear", NULL, icon = icon("refresh"), title = "Clear"),
  verbatimTextOutput("stack")
)

server <- function(input, output, session) {
  undoredo <- UndoManager$new(type = c("numeric", "integer"))$reactive()
  
  observeEvent(input$save, {
    undoredo()$do(input$num)
  })
  observeEvent(input$undo, {
    undoredo()$undo()
  })
  observeEvent(input$redo, {
    undoredo()$redo()
  })
  observeEvent(input$clear, {
    undoredo()$clear()
  })
  observe({
    shinyjs::toggleState("undo", undoredo()$can_undo > 0)
    shinyjs::toggleState("redo", undoredo()$can_redo > 0)
  })
  output$stack <- renderPrint({
    undoredo()
  })
}

shinyApp(ui, server)
```
