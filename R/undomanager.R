#' Undo/Redo manager
#'
#' @description
#' With the undo manager, you can manage the history of an object by
#' using undo and redo operations.
#'
#' @field value Get the value
#' @field undo_size Get the number of undo operations
#' @field redo_size Get the number of redo operations
#' @examples
#' TODO
#' @export
UndoManager <- R6::R6Class(
  "UndoManager",
  cloneable = TRUE,

  private = list(
    .type = NULL,
    .undo_stack = list(),
    .redo_stack = list(),
    .current = NULL
  ),

  active = list(

    value = function() {
      private$.current
    },

    undo_size = function() {
      length(private$.undo_stack)
    },

    redo_size = function() {
      length(private$.redo_stack)
    }

  ),

  public = list(

    #' @description
    #' TODO
    #' @param type The permitted classes of the objects (`NULL` to allow any object)
    #' @examples
    #' TODO
    #' @return TODO
    initialize = function(type = NULL) {
      if (!is.null(type) &&
          !checkmate::test_character(type, any.missing = FALSE, unique = TRUE,
                                     min.chars = 1, names = "unnamed")) {
        stop("UndoManager: `type` must either be `NULL` or an unnamed vector of strings",
             call. = FALSE)
      }
      private$.type <- type
      invisible(self)
    },

    #' @description
    #' TODO
    #' @param type The class of the object (`NULL` to allow any object)
    #' @examples
    #' TODO
    print = function() {
      cat("<UndoManager>")
      if (is.null(private$.type)) {
        cat(" of arbitrary items")
      } else {
        cat0(" of items of type ", paste0("<", private$.type, ">", collapse = "|"))
      }
      cat0(" with ")
      cat0(self$undo_size, if(self$undo_size == 1) " undo" else " undos", " and ")
      cat0(self$redo_size, if(self$redo_size == 1) " redo" else " redos", "\n")

      cat("\n#############")
      cat("\nCurrent item: ")
      if (is.atomic(private$.current)) {
        cat(private$.current, "\n")
      } else {
        cat("\n")
        print(private$.current)
      }

      if (self$undo_size > 0) {
        cat("\n###########")
        cat("\nUndo stack:\n")
        for (idx in seq_len(self$undo_size)) {
          idx_rev <- (self$undo_size - idx + 1)
          cat0(idx, ". ")
          if (is.atomic(private$.undo_stack[[idx_rev]])) {
            cat(private$.undo_stack[[idx_rev]], "\n")
          } else {
            cat("\n")
            print(private$.undo_stack[[idx_rev]])
          }
        }
      }

      if (self$redo_size > 0) {
        cat("\n###########")
        cat("\nRedo stack:\n")
        for (idx in seq_len(self$redo_size)) {
          idx_rev <- (self$redo_size - idx + 1)
          cat0(idx, ". ")
          if (is.atomic(private$.redo_stack[[idx_rev]])) {
            cat(private$.redo_stack[[idx_rev]], "\n")
          } else {
            cat("\n")
            print(private$.redo_stack[[idx_rev]])
          }
        }
      }
    },

    #' @description
    #' TODO
    #' @param type The class of the object (`NULL` to allow any object)
    #' @examples
    #' TODO
    #' @return TODO
    undo = function() {
      if (self$undo_size < 1) {
        stop("undo: There is nothing to undo", call. = FALSE)
      }
      private$.redo_stack <- append(private$.redo_stack, private$.current)
      private$.current <- tail(private$.undo_stack, 1)[[1]]
      private$.undo_stack <- head(private$.undo_stack, -1)
      invisible(self)
    },

    #' @description
    #' TODO
    #' @param type The class of the object (`NULL` to allow any object)
    #' @examples
    #' TODO
    #' @return TODO
    redo = function() {
      if (self$redo_size < 1) {
        stop("redo: There is nothing to redo", call. = FALSE)
      }
      private$.undo_stack <- append(private$.undo_stack, private$.current)
      private$.current <- tail(private$.redo_stack, 1)[[1]]
      private$.redo_stack <- head(private$.redo_stack, -1)
      invisible(self)
    },

    #' @description
    #' TODO
    #' @param type The class of the object (`NULL` to allow any object)
    #' @examples
    #' TODO
    #' @return TODO
    do = function(item) {
      if (is.null(item)) {
        stop("do: item must not be NULL", call. = FALSE)
      }
      if (!is.null(private$.type) && !inherits(item, private$.type)) {
        stop("do: The provided item must have class ",
             paste0("<", private$.type, ">", collapse = "|"),
             call. = FALSE)
      }

      if (is.null(private$.current)) {
        private$.undo_stack <- list()
      } else {
        private$.undo_stack <- append(private$.undo_stack, private$.current)
      }
      private$.current <- item
      private$.redo_stack <- list()
      invisible(self)
    }

  )
)

#' Undo/Redo manager
#'
#' With the undo manager, you can manage the history of an object by
#' using undo and redo operations.\cr\cr
#' This function is equivalent to calling `UndoManager$new()`
undomanager <- function(type = NULL) {
  UndoManager$new(type = type)
}
