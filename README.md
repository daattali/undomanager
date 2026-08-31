<h3 align="center">undomanager</h3>
<h4 align="center">
  ↩️ Manage the history of any object with undo/redo operations
  <br><br>
  by <a href="https://deanattali.com">Dean Attali</a>
</h4>

<p align="center">
  <a href="https://github.com/daattali/undomanager/actions">
    <img src="https://github.com/daattali/undomanager/workflows/R-CMD-check/badge.svg" alt="R build status" />
  </a>
  <a href="https://cran.r-project.org/package=undomanager">
    <img src="https://www.r-pkg.org/badges/version/undomanager" alt="CRAN version" />
  </a>
  <a href="https://paypal.me/daattali/20">
    <img src="http://i.imgur.com/vCIGFrH.png" />
  </a>
</p>

---

<img src="inst/img/hex.png" width="170" align="right"/>

{undomanager} lets you track the history of any R object and move through it with undo and redo operations. Anything can be stored, from a single number to a data frame or an entire application state. A manager can restrict its history to specific classes and cap how many items it keeps, and it can be reactive to integrate with 'Shiny'.

**Need Shiny help? [I'm available for consulting](https://attalitech.com/).**<br/>
**If you find {undomanager} useful, please consider [supporting my work](https://github.com/sponsors/daattali)! ❤**

<p align="center">
  <a style="display: inline-block;" href="https://github.com/sponsors/daattali">
    <img height="35" src="https://i.imgur.com/034B8vq.png" />
  </a>
</p>

# Table of contents

- [Example](#example)
- [Installation](#install)
- [Chaining](#chaining)
- [Restricting the type of items](#types)
- [Undoing or redoing multiple steps](#multiple)
- [Storing `NULL`](#null)
- [Objects with reference semantics](#reference)
- [Using with shiny](#shiny)

<h2 id="example">Example</h2>

```r
nums <- UndoManager$new()
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
<UndoManager> of arbitrary items with 2 undos and 1 redo

### Current item ###
[1] 10

### Undo stack ###
1.
[1] 7

2.
[1] 5


### Redo stack ###
1.
[1] 12
```

<h2 id="install">Installation</h2>

**For most users:** To install the stable CRAN version:

```r
install.packages("undomanager")
```

**For advanced users:** To install the latest development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("daattali/undomanager")
```

<h2 id="chaining">Chaining</h2>

You can also chain all the operations; the above is equivalent to:

```r
UndoManager$new()$do(5)$do(7)$do(10)$do(12)$undo()$undo()$redo()
```

<h2 id="types">Restricting the type of items</h2>

By default an UndoManager accepts any object. Pass a `type` to `new()` to restrict it to one or more classes:

```r
nums <- UndoManager$new("numeric")
nums$do(5)
nums$do("a")
#> Error: do: The provided item must have class <numeric>
```

The type is matched against the same classes that R's S3 dispatch would use. That means `"numeric"` also accepts integers and numeric matrices, while rejecting things like logicals, factors, or dates. Pass several classes to allow any of them:

```r
items <- UndoManager$new(c("numeric", "character"))
items$do(5)
items$do("a")
items$do(TRUE)
#> Error: do: The provided item must have class <numeric>|<character>
```

<h2 id="multiple">Undoing or redoing multiple steps</h2>

`undo()` and `redo()` accept an `n` argument to move more than one step at a time. If `n` is larger than the number of available operations, they stop at the end of the history. Use `Inf` to go all the way.

```r
nums <- UndoManager$new()$do(5)$do(7)$do(10)$do(12)

nums$undo(2)$value
#> [1] 7

nums$redo(Inf)$value
#> [1] 12
```

<h2 id="null">Storing `NULL`</h2>

`NULL` is a value like any other, so it can be stored and undone:

```r
nums <- UndoManager$new()$do(5)$do(NULL)$do(10)

nums$undo()$value
#> NULL

nums$undo()$value
#> [1] 5
```

Because an empty manager also reports a `NULL` value, use `is_empty` to tell the two apart:

```r
UndoManager$new()$value
#> NULL
UndoManager$new()$is_empty
#> [1] TRUE

UndoManager$new()$do(NULL)$value
#> NULL
UndoManager$new()$do(NULL)$is_empty
#> [1] FALSE
```

When a `type` is set, storing `NULL` also requires setting `allow_null`:

```r
nums <- UndoManager$new("numeric", allow_null = TRUE)
nums$do(5)
nums$do(NULL)
```

<h2 id="reference">Objects with reference semantics</h2>

Items are stored exactly as they are given, without being copied. For most common objects (vectors, lists, data frames), R's copy-on-modify behaviour means the history is effectively a snapshot, so changing your own copy afterwards doesn't affect it.

Environments and R6 objects are different: they're stored by reference, which means modifying one after adding it also changes what the history holds.

```r
env <- new.env()
env$val <- "before"

hist <- UndoManager$new()$do(env)$do("something else")
env$val <- "after"

hist$undo()$value$val
#> [1] "after"
```

If you want the history to be a true snapshot of a reference object, store a copy of it yourself:

```r
hist$do(as.environment(as.list(env, all.names = TRUE)))  # environments
hist$do(obj$clone(deep = TRUE))                          # R6 objects
```

<h2 id="shiny">Using with shiny</h2>

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
  undoredo <- UndoManager$new(type = c("numeric"))$reactive()
  
  observeEvent(input$save, {
    req(input$num)
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
