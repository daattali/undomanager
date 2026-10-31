# Pixel Art: a small pixel art editor that shows off {undomanager}.
#
# The whole canvas is a single 16x16 character matrix, and every finished
# stroke is one item in the manager's history.

library(shiny)

SIZE <- 16
BLANK <- matrix("#FFFFFF", SIZE, SIZE)
PALETTE <- c(
  "Black" = "#000000",
  "Grey" = "#7F7F7F",
  "Red" = "#E63946",
  "Orange" = "#F4A261",
  "Yellow" = "#F1C40F",
  "Teal" = "#2A9D8F",
  "Blue" = "#457B9D",
  "Purple" = "#6A4C93"
)
TOOL_FILL <- "fill"
TOOL_ERASE <- "erase"
TOOL_PAINT <- "paint"

# Recolour the cell at (row, col) and every neighbouring cell of the same colour
flood_fill <- function(canvas, row, col, color, target = canvas[row, col]) {
  off_canvas <- row < 1 || row > SIZE || col < 1 || col > SIZE
  if (off_canvas || canvas[row, col] != target || target == color) {
    return(canvas)
  }

  canvas[row, col] <- color
  canvas <- flood_fill(canvas, row - 1, col, color, target)
  canvas <- flood_fill(canvas, row + 1, col, color, target)
  canvas <- flood_fill(canvas, row, col - 1, color, target)
  canvas <- flood_fill(canvas, row, col + 1, color, target)
  canvas
}

# The browser identifies cells by a 0-based index, counting left to right and top to bottom
cell_row <- function(i) i %/% SIZE + 1
cell_col <- function(i) i %% SIZE + 1

# Apply a stroke to the canvas. A stroke says which tool was used, the colour it
# painted with (white when erasing), and which cells the mouse went over
apply_stroke <- function(canvas, stroke) {
  cells <- unlist(stroke$cells)

  if (stroke$tool == TOOL_FILL) {
    flood_fill(canvas, cell_row(cells[1]), cell_col(cells[1]), stroke$color)
  } else {
    canvas[cbind(cell_row(cells), cell_col(cells))] <- stroke$color
    canvas
  }
}

share <- list(
  title = "{undomanager}",
  url = "https://github.com/daattali/undomanager",
  source = "https://github.com/daattali/undomanager",
  image = "https://github.com/daattali/undomanager/raw/master/inst/img/social-preview.png",
  description = "Manage the history of any object with undo/redo operations",
  twitter_user = "daattali"
)

ui <- fluidPage(
  title = paste0("{undomanager} ", as.character(utils::packageVersion("undomanager"))),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js"),

    # Favicon
    tags$link(rel = "shortcut icon", type = "image/x-icon", href = "https://daattali.com/shiny/img/favicon.ico"),
    # Facebook OpenGraph tags
    tags$meta(property = "og:title", content = share$title),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = share$url),
    tags$meta(property = "og:image", content = share$image),
    tags$meta(property = "og:description", content = share$description),

    # Twitter summary cards
    tags$meta(name = "twitter:card", content = "summary"),
    tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:title", content = share$title),
    tags$meta(name = "twitter:description", content = share$description),
    tags$meta(name = "twitter:image", content = share$image)
  ),

  div(
    id = "header",
    div(id = "pagetitle", share$title),
    div(id = "subtitle", share$description),
    div(
      id = "subsubtitle",
      "Created by",
      tags$a(href = "https://deanattali.com/", "Dean Attali"),
      HTML("&bull;"),
      "Available",
      tags$a(href = share$source, "on GitHub"),
      HTML("&bull;"),
      tags$a(href = "https://daattali.com/shiny/", "More apps"), "by Dean"
    )
  ),

  div(
    id = "main",
    p(
      id = "intro",
      "A pixel art editor built with ", code("{undomanager}"), ".",
      "The whole canvas is one matrix, and every stroke is one history item. ",
      "Draw something, then undo, redo, or jump to any point in the history."
    ),

    div(
      class = "pa-wrap",
      div(
        class = "pa-card pa-editor",
        div(
          class = "pa-controls",
          div(
            class = "pa-actions",
            actionButton("undo", "Undo", icon = icon("undo"), class = "btn-primary"),
            actionButton("redo", "Redo", icon = icon("redo"), class = "btn-primary"),
            actionButton("blank", "Blank canvas", icon = icon("file"),
                         title = "Add an empty canvas to the history (can be undone)")
          ),
          div(
            class = "pa-tools",
            radioButtons("tool", NULL, inline = TRUE,
                         c("Paint" = TOOL_PAINT, "Erase" = TOOL_ERASE, "Fill" = TOOL_FILL)),
            div(class = "pa-swatches",
              lapply(names(PALETTE), function(name) {
                tags$button(class = "pa-swatch", `data-color` = PALETTE[[name]],
                            title = name, style = paste0("background:", PALETTE[[name]]))
              })
            )
          )
        ),
        div(
          id = "pa-grid",
          style = paste0("--size: ", SIZE),
          lapply(seq_len(SIZE^2) - 1, function(i) div(class = "pa-cell", `data-i` = i))
        )
      ),

      div(class = "pa-card pa-history",
        h4("History"),
        p(class = "text-muted small",
          "Click a dot to jump to that step. The yellow dot is the current canvas,",
          "blue dots are undos, white dots are redos."),
        uiOutput("timeline"),
        div(class = "pa-toolbar",
          actionButton("start", "Revert to start", icon = icon("backward-fast"), class = "btn-sm"),
          actionButton("latest", "Jump to latest", icon = icon("forward-fast"), class = "btn-sm"),
          actionButton("clear", "Clear history", icon = icon("eraser"), class = "btn-sm",
                       title = "Forget the history but keep the current canvas")
        ),
        div(
          class = "pa-log",
          h4("Code log"),
          verbatimTextOutput("log")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  canvas_history <- undomanager::undomanager(type = "matrix")$
    do(BLANK)$
    reactive()

  code_log <- reactiveVal(c(
    'canvas <- undomanager("matrix")',
    'canvas$do(<blank 16x16 matrix>)'
  ))
  add_to_log <- function(line) code_log(c(code_log(), line))

  observeEvent(canvas_history(), {
    session$sendCustomMessage("pa-render", list(
      cells = as.vector(t(canvas_history()$value)),
      can_undo = canvas_history()$can_undo,
      can_redo = canvas_history()$can_redo
    ))
  })

  observeEvent(input$stroke, {
    before <- canvas_history()$value
    after <- apply_stroke(before, input$stroke)

    # A stroke that changes nothing, such as painting a cell its own colour,
    # isn't worth a history item
    if (identical(after, before)) return()

    canvas_history()$do(after)
    add_to_log(sprintf("canvas$do(<%s>)", input$stroke$tool))
  })

  move <- function(steps) {
    if (steps < 0) {
      canvas_history()$undo(-steps)
      add_to_log(sprintf("canvas$undo(%s)", -steps))
    } else if (steps > 0) {
      canvas_history()$redo(steps)
      add_to_log(sprintf("canvas$redo(%s)", steps))
    }
  }
  observeEvent(input$undo, move(-1))
  observeEvent(input$redo, move(1))
  observeEvent(input$start, move(-Inf))
  observeEvent(input$latest, move(Inf))
  observeEvent(input$jump, move(input$jump))  # clicking a dot in the timeline

  observeEvent(input$clear, {
    canvas_history()$clear()
    add_to_log("canvas$clear()")
  })
  observeEvent(input$blank, {
    canvas_history()$do(BLANK)
    add_to_log("canvas$do(<blank 16x16 matrix>)")
  })

  # One dot per history step: undos on the left, the current canvas, then redos
  output$timeline <- renderUI({
    steps <- seq(-canvas_history()$undo_size, canvas_history()$redo_size)

    dots <- lapply(steps, function(step) {
      type <- if (step < 0) "undo" else if (step > 0) "redo" else "current"
      label <- switch(type,
        undo = paste("Undo", -step),
        redo = paste("Redo", step),
        current = "Current"
      )
      tags$button(class = paste("pa-dot", type), title = label, `data-step` = step)
    })

    tagList(
      div(class = "pa-timeline", dots),
      p(strong(sprintf("Step %d of %d", canvas_history()$undo_size + 1, length(steps))))
    )
  })

  output$log <- renderText({
    paste(code_log(), collapse = "\n")
  })
}

shinyApp(ui, server)
