undoredo_test <- function() {
  UndoManager$new()$do(5)$do(7)$do(3)$do(8)$do(1)
}

# undoredo_test() rewound to the beginning
undoredo_rewound <- function() {
  undoredo_test()$undo()$undo()$undo()$undo()
}

# count how many times the internal reactive counter has been incremented,
# as a proxy for how many times reactivity in shiny is triggered
count <- function(x) x$.__enclos_env__$private$.rx_count

test_that("UndoManager can store NULL items", {
  expect_error(UndoManager$new()$do(NULL), NA)
  expect_error(UndoManager$new()$do(1)$do(NULL), NA)
  expect_error(UndoManager$new()$do(1)$do(2), NA)

  expect_null(UndoManager$new()$do(NULL)$value)
  expect_null(UndoManager$new()$do(1)$do(NULL)$value)
  expect_identical(UndoManager$new()$do(1)$do(NULL)$undo_size, 1L)
  expect_false(UndoManager$new()$do(NULL)$can_undo)

  expect_identical(UndoManager$new()$do(1)$do(NULL)$undo()$value, 1)
  expect_null(UndoManager$new()$do(1)$do(NULL)$undo()$redo()$value)
  expect_null(UndoManager$new()$do(1)$do(NULL)$do(3)$undo()$value)
  expect_identical(UndoManager$new()$do(1)$do(NULL)$do(3)$undo(2)$value, 1)
  expect_identical(UndoManager$new()$do(NULL)$do(2)$undo_size, 1L)
  expect_identical(UndoManager$new()$do(NULL)$do(NULL)$undo_size, 1L)
})

test_that("UndoManager NULL needs allow_null when a type is given", {
  expect_error(UndoManager$new("numeric")$do(NULL), "must not be NULL")
  expect_error(UndoManager$new("numeric", allow_null = TRUE)$do(NULL), NA)
  expect_error(UndoManager$new("numeric", allow_null = TRUE)$do(5), NA)
  expect_error(UndoManager$new("numeric", allow_null = TRUE)$do("a"), "must have class")

  expect_error(UndoManager$new()$do(NULL), NA)
  expect_error(UndoManager$new(allow_null = FALSE)$do(NULL), NA)

  m <- UndoManager$new("numeric", allow_null = TRUE)$do(5)$do(NULL)$do(10)
  expect_identical(m$value, 10)
  expect_null(m$undo()$value)
  expect_identical(m$undo()$undo()$value, 5)
})

test_that("UndoManager rejects \"NULL\" as a type", {
  expect_error(UndoManager$new("NULL"), "cannot include")
  expect_error(UndoManager$new(c("numeric", "NULL")), "cannot include")
  expect_error(UndoManager$new("NULL"), "allow_null")
})

test_that("UndoManager validates allow_null", {
  expect_error(UndoManager$new("numeric", allow_null = "yes"), "allow_null")
  expect_error(UndoManager$new("numeric", allow_null = NA), "allow_null")
  expect_error(UndoManager$new("numeric", allow_null = NULL), "allow_null")
  expect_error(UndoManager$new("numeric", allow_null = c(TRUE, TRUE)), "allow_null")
})

test_that("UndoManager allow_null counts towards equality", {
  expect_false(isTRUE(all.equal(
    UndoManager$new("numeric", allow_null = TRUE)$do(1),
    UndoManager$new("numeric")$do(1)
  )))
})

test_that("An empty UndoManager is not the same as one holding NULL", {
  expect_false(isTRUE(all.equal(UndoManager$new(), UndoManager$new()$do(NULL))))
})

test_that("UndoManager with no type accepts any object", {
  expect_error(UndoManager$new()$do("a"), NA)
  expect_error(UndoManager$new()$do("a")$do("b"), NA)
  expect_error(UndoManager$new()$do(1), NA)
  expect_error(UndoManager$new()$do(1)$do(2), NA)
  expect_error(UndoManager$new()$do("a")$do(1)$do("b")$do(2), NA)
})

test_that("UndoManager with a type accepts that object", {
  expect_error(UndoManager$new("character")$do("a"), NA)
  expect_error(UndoManager$new("character")$do("a")$do("b"), NA)
  expect_error(UndoManager$new("numeric")$do(1), NA)
  expect_error(UndoManager$new("numeric")$do(1)$do(2), NA)
})

test_that("UndoManager with a type rejects different objects", {
  expect_error(UndoManager$new("character")$do(1), "must have class")
  expect_error(UndoManager$new("numeric")$do("a"), "must have class")
  expect_error(UndoManager$new("numeric")$do(1)$do("a"), "must have class")
  expect_error(UndoManager$new("character")$do("a")$do(1), "must have class")
})

test_that("UndoManager type rejects objects of other classes", {
  expect_error(UndoManager$new("factor")$do("a"), "must have class")
  expect_error(UndoManager$new("Date")$do(1), "must have class")
  expect_error(UndoManager$new("character")$do(factor("a")), "must have class")
  expect_error(UndoManager$new("list")$do(data.frame(a = 1)), "must have class")
  expect_error(UndoManager$new("data.frame")$do(matrix(1, 1)), "must have class")
  expect_error(UndoManager$new("integer")$do("a"), "must have class")
  expect_error(UndoManager$new("logical")$do(1), "must have class")
  expect_error(UndoManager$new("matrix")$do(1), "must have class")
  expect_error(UndoManager$new("character")$do(TRUE), "must have class")
})

test_that("UndoManager is left untouched when do() rejects an item", {
  x <- UndoManager$new("numeric")$do(1)$do(2)

  expect_error(x$do("nope"), "must have class")
  expect_identical(x$value, 2)
  expect_identical(x$undo_size, 1L)
  expect_identical(x$redo_size, 0L)
  expect_identical(count(x), 2)

  expect_error(x$do(NULL), "must not be NULL")
  expect_identical(x$value, 2)
  expect_identical(x$undo_size, 1L)
  expect_identical(count(x), 2)
})

test_that("UndoManager with multiple types", {
  expect_error(UndoManager$new(c("character", "numeric"))$do(1), NA)
  expect_error(UndoManager$new(c("character", "numeric"))$do("a"), NA)
  expect_error(UndoManager$new(c("character", "numeric"))$do("a")$do(1), NA)
  expect_error(UndoManager$new(c("character", "numeric"))$do("a")$do(1)$do(1L), NA)
  expect_error(UndoManager$new(c("character", "integer", "numeric"))$do("a")$do(1)$do(1L), NA)

  expect_error(UndoManager$new(c("character", "numeric"))$do(TRUE), "must have class")
  expect_error(UndoManager$new(c("character", "numeric"))$do(factor("a")), "must have class")
  expect_error(UndoManager$new(c("character", "numeric"))$do("a")$do(list(1)), "must have class")
})

test_that("UndoManager type matches the classes S3 dispatch would use", {
  expect_error(UndoManager$new("numeric")$do(1L), NA)
  expect_error(UndoManager$new("numeric")$do(1:3), NA)
  expect_error(UndoManager$new("numeric")$do(matrix(1.5, 1)), NA)
  expect_error(UndoManager$new("numeric")$do(array(1.5, c(1, 1, 1))), NA)

  expect_error(UndoManager$new("integer")$do(1L), NA)
  expect_error(UndoManager$new("matrix")$do(matrix(1.5, 1)), NA)
  expect_error(UndoManager$new("array")$do(matrix(1.5, 1)), NA)

  expect_error(UndoManager$new("double")$do(1), NA)
  expect_error(UndoManager$new("double")$do(matrix(1.5, 1)), NA)
  expect_error(UndoManager$new("integer")$do(matrix(1L, 1)), NA)
  expect_error(UndoManager$new("double")$do(1L), "must have class")

  expect_error(UndoManager$new("numeric")$do("a"), "must have class")
  expect_error(UndoManager$new("numeric")$do(TRUE), "must have class")
  expect_error(UndoManager$new("numeric")$do(factor("a")), "must have class")
  expect_error(UndoManager$new("numeric")$do(Sys.Date()), "must have class")
  expect_error(UndoManager$new("numeric")$do(data.frame(a = 1)), "must have class")
  expect_error(UndoManager$new("numeric")$do(list(1)), "must have class")
  expect_error(UndoManager$new("numeric")$do(1.5), NA)
  expect_error(UndoManager$new("integer")$do(1.5), "must have class")
})

test_that("UndoManager clear works", {
  expect_identical(
    UndoManager$new()$clear(),
    UndoManager$new()
  )

  expect_identical(
    UndoManager$new()$do(1)$do(2)$do(3)$do(4)$clear(),
    UndoManager$new()$do(4)
  )

  expect_identical(
    UndoManager$new()$do(1)$do(2)$do(3)$do(4)$clear(clear_value = TRUE),
    UndoManager$new()
  )
})

test_that("UndoManager clear keeps the current value but drops the history", {
  expect_identical(UndoManager$new()$do(1)$do(2)$do(3)$clear()$value, 3)
  expect_identical(UndoManager$new()$do(1)$do(2)$do(3)$clear()$undo_size, 0L)
  expect_identical(UndoManager$new()$do(1)$do(2)$do(3)$clear()$redo_size, 0L)
  expect_false(UndoManager$new()$do(1)$do(2)$do(3)$clear()$can_undo)
})

test_that("UndoManager clear also discards the redo branch", {
  expect_identical(UndoManager$new()$do(1)$do(2)$do(3)$undo()$clear()$value, 2)
  expect_identical(UndoManager$new()$do(1)$do(2)$do(3)$undo()$clear()$redo_size, 0L)
  expect_false(UndoManager$new()$do(1)$do(2)$do(3)$undo()$clear()$can_redo)
})

test_that("UndoManager clear with clear_value empties the manager", {
  expect_null(UndoManager$new()$do(1)$do(2)$clear(clear_value = TRUE)$value)
  expect_identical(UndoManager$new()$do(1)$do(2)$clear(clear_value = TRUE)$undo_size, 0L)
  expect_identical(UndoManager$new()$do(1)$do(2)$clear(clear_value = TRUE)$redo_size, 0L)
})

test_that("UndoManager clear on an empty manager is safe", {
  expect_null(UndoManager$new()$clear()$value)
  expect_null(UndoManager$new()$clear(clear_value = TRUE)$value)
  expect_identical(UndoManager$new()$clear()$undo_size, 0L)
  expect_identical(UndoManager$new()$clear()$redo_size, 0L)
})

test_that("UndoManager is still usable after clear", {
  expect_identical(UndoManager$new()$do(1)$do(2)$clear()$do(3)$value, 3)
  expect_identical(UndoManager$new()$do(1)$do(2)$clear()$do(3)$undo_size, 1L)
  expect_identical(UndoManager$new()$do(1)$do(2)$clear()$do(3)$undo()$value, 2)

  expect_identical(UndoManager$new()$do(1)$do(2)$clear(clear_value = TRUE)$do(3)$value, 3)
  expect_identical(UndoManager$new()$do(1)$do(2)$clear(clear_value = TRUE)$do(3)$undo_size, 0L)
})

test_that("UndoManager clear rejects a non-flag clear_value", {
  expect_error(UndoManager$new()$clear(NA), "clear_value")
  expect_error(UndoManager$new()$clear("yes"), "clear_value")
  expect_error(UndoManager$new()$clear(1), "clear_value")
  expect_error(UndoManager$new()$clear(c(TRUE, TRUE)), "clear_value")
  expect_error(UndoManager$new()$clear(NULL), "clear_value")
})

test_that("UndoManager can_undo and can_redo", {
  expect_false(UndoManager$new()$can_undo)
  expect_false(UndoManager$new()$do(1)$can_undo)
  expect_true(UndoManager$new()$do(1)$do(2)$can_undo)
  expect_false(UndoManager$new()$do(1)$do(2)$undo()$can_undo)
  expect_true(UndoManager$new()$do(1)$do(2)$undo()$redo()$can_undo)

  expect_false(UndoManager$new()$can_redo)
  expect_false(UndoManager$new()$do(1)$can_redo)
  expect_false(UndoManager$new()$do(1)$do(2)$can_redo)
  expect_true(UndoManager$new()$do(1)$do(2)$undo()$can_redo)
  expect_false(UndoManager$new()$do(1)$do(2)$undo()$redo()$can_redo)
})

test_that("UndoManager do works", {
  expect_identical(UndoManager$new()$do(5)$value, 5)
  expect_identical(UndoManager$new()$do(5)$undo_size, 0L)
  expect_identical(UndoManager$new()$do(5)$redo_size, 0L)

  expect_identical(UndoManager$new()$do(5)$do(7)$value, 7)
  expect_identical(UndoManager$new()$do(5)$do(7)$undo_size, 1L)
  expect_identical(UndoManager$new()$do(5)$do(7)$redo_size, 0L)

  expect_identical(UndoManager$new()$do(5)$do(7)$do(3)$value, 3)
  expect_identical(UndoManager$new()$do(5)$do(7)$do(3)$undo_size, 2L)
  expect_identical(UndoManager$new()$do(5)$do(7)$do(3)$redo_size, 0L)

  expect_identical(UndoManager$new()$do(5)$do(7)$do(3)$do(8)$do(1)$value, 1)
  expect_identical(UndoManager$new()$do(5)$do(7)$do(3)$do(8)$do(1)$undo_size, 4L)
  expect_identical(UndoManager$new()$do(5)$do(7)$do(3)$do(8)$do(1)$redo_size, 0L)
})

test_that("UndoManager undo/redo past the beginning/end is a silent no-op", {
  expect_error(UndoManager$new()$undo(), NA)
  expect_error(UndoManager$new()$redo(), NA)

  expect_error(UndoManager$new()$do(1)$undo(), NA)
  expect_error(UndoManager$new()$do(1)$redo(), NA)

  expect_error(UndoManager$new()$do(1)$do(2)$undo(), NA)
  expect_error(UndoManager$new()$do(1)$do(2)$undo()$undo(), NA)
  expect_error(UndoManager$new()$do(1)$do(2)$redo(), NA)
  expect_error(UndoManager$new()$do(1)$do(2)$undo()$redo(), NA)
  expect_error(UndoManager$new()$do(1)$do(2)$undo()$redo()$redo(), NA)

  expect_error(UndoManager$new()$do(1)$do(2)$do(3)$undo(), NA)
  expect_error(UndoManager$new()$do(1)$do(2)$do(3)$undo()$undo(), NA)
  expect_error(UndoManager$new()$do(1)$do(2)$do(3)$undo()$undo()$undo(), NA)
})

test_that("UndoManager no-op undo/redo leaves the state untouched", {
  expect_identical(UndoManager$new()$undo(), UndoManager$new())
  expect_identical(UndoManager$new()$redo(), UndoManager$new())

  expect_identical(UndoManager$new()$do(1)$undo(), UndoManager$new()$do(1))
  expect_identical(UndoManager$new()$do(1)$redo(), UndoManager$new()$do(1))

  expect_identical(
    UndoManager$new()$do(1)$do(2)$do(3)$undo()$undo()$undo()$undo(),
    UndoManager$new()$do(1)$do(2)$do(3)$undo()$undo()
  )

  expect_identical(
    UndoManager$new()$do(1)$do(2)$do(3)$undo()$redo()$redo(),
    UndoManager$new()$do(1)$do(2)$do(3)
  )
})

test_that("UndoManager undo/redo still return self when they are a no-op", {
  expect_identical(UndoManager$new()$undo()$do(1)$value, 1)
  expect_identical(UndoManager$new()$do(1)$redo()$do(2)$value, 2)
  expect_identical(UndoManager$new()$do(1)$do(2)$undo()$undo()$redo()$value, 2)
})

test_that("UndoManager no-op undo/redo does not trigger a reactive update", {
  x <- UndoManager$new()
  x$undo()
  x$redo()
  expect_identical(count(x), 0)
  x$do(1)
  expect_identical(count(x), 1)
  x$undo()
  x$redo()
  expect_identical(count(x), 1)
  x$do(2)
  expect_identical(count(x), 2)
  x$redo()
  expect_identical(count(x), 2)
  x$undo()
  expect_identical(count(x), 3)
  x$undo()
  expect_identical(count(x), 3)
  x$redo()
  expect_identical(count(x), 4)
  x$redo()
  expect_identical(count(x), 4)
  x$redo()
  expect_identical(count(x), 4)

})

test_that("UndoManager undo/redo works", {
  expect_identical(undoredo_test()$undo()$value, 8)
  expect_identical(undoredo_test()$undo()$undo_size, 3L)
  expect_identical(undoredo_test()$undo()$redo_size, 1L)

  expect_identical(
    undoredo_test()$undo()$redo()$value,
    undoredo_test()$value
  )
  expect_identical(
    undoredo_test()$undo()$redo()$undo_size,
    undoredo_test()$undo_size
  )
  expect_identical(
    undoredo_test()$undo()$redo()$redo_size,
    undoredo_test()$redo_size
  )

  expect_identical(undoredo_test()$undo()$undo()$value, 3)
  expect_identical(undoredo_test()$undo()$undo()$undo_size, 2L)
  expect_identical(undoredo_test()$undo()$undo()$redo_size, 2L)

  expect_identical(
    undoredo_test()$undo()$undo()$redo()$value,
    undoredo_test()$undo()$value
  )
  expect_identical(
    undoredo_test()$undo()$undo()$redo()$undo_size,
    undoredo_test()$undo()$undo_size
  )
  expect_identical(
    undoredo_test()$undo()$undo()$redo()$redo_size,
    undoredo_test()$undo()$redo_size
  )

  expect_identical(undoredo_test()$undo()$undo()$undo()$value, 7)
  expect_identical(undoredo_test()$undo()$undo()$undo()$undo_size, 1L)
  expect_identical(undoredo_test()$undo()$undo()$undo()$redo_size, 3L)

  expect_identical(
    undoredo_test()$undo()$undo()$undo()$redo()$value,
    undoredo_test()$undo()$undo()$value
  )
  expect_identical(
    undoredo_test()$undo()$undo()$undo()$redo()$undo_size,
    undoredo_test()$undo()$undo()$undo_size
  )
  expect_identical(
    undoredo_test()$undo()$undo()$undo()$redo()$redo_size,
    undoredo_test()$undo()$undo()$redo_size
  )
  expect_identical(
    undoredo_test()$undo()$undo()$redo()$undo()$redo()$value,
    undoredo_test()$undo()$value
  )
  expect_identical(
    undoredo_test()$undo()$undo()$redo()$undo()$redo()$undo_size,
    undoredo_test()$undo()$undo_size
  )
  expect_identical(
    undoredo_test()$undo()$undo()$redo()$undo()$redo()$redo_size,
    undoredo_test()$undo()$redo_size
  )

  expect_identical(undoredo_test()$undo()$undo()$undo()$undo()$value, 5)
  expect_identical(undoredo_test()$undo()$undo()$undo()$undo()$redo()$redo()$redo()$redo()$value, 1)
  expect_identical(undoredo_test()$undo()$redo()$undo()$redo()$undo()$redo()$undo()$redo()$value, 1)
  expect_identical(undoredo_test()$undo()$undo()$undo()$undo()$undo_size, 0L)
  expect_identical(undoredo_test()$undo()$undo()$undo()$undo()$redo_size, 4L)
})

test_that("UndoManager undo(n) matches n single-step undos", {
  expect_identical(undoredo_test()$undo(0)$value, 1)
  expect_identical(undoredo_test()$undo(1)$value, 8)
  expect_identical(undoredo_test()$undo(2)$value, 3)
  expect_identical(undoredo_test()$undo(3)$value, 7)
  expect_identical(undoredo_test()$undo(4)$value, 5)
  expect_identical(undoredo_test()$undo(5)$value, 5)

  expect_identical(undoredo_test()$undo(1)$value, undoredo_test()$undo()$value)
  expect_identical(undoredo_test()$undo(2)$value, undoredo_test()$undo()$undo()$value)
  expect_identical(undoredo_test()$undo(3)$value, undoredo_test()$undo()$undo()$undo()$value)
  expect_identical(undoredo_test()$undo(4)$value, undoredo_test()$undo()$undo()$undo()$undo()$value)

  expect_identical(undoredo_test()$undo(0)$undo_size, 4L)
  expect_identical(undoredo_test()$undo(1)$undo_size, 3L)
  expect_identical(undoredo_test()$undo(2)$undo_size, 2L)
  expect_identical(undoredo_test()$undo(3)$undo_size, 1L)
  expect_identical(undoredo_test()$undo(4)$undo_size, 0L)

  expect_identical(undoredo_test()$undo(0)$redo_size, 0L)
  expect_identical(undoredo_test()$undo(1)$redo_size, 1L)
  expect_identical(undoredo_test()$undo(2)$redo_size, 2L)
  expect_identical(undoredo_test()$undo(3)$redo_size, 3L)
  expect_identical(undoredo_test()$undo(4)$redo_size, 4L)
})

test_that("UndoManager redo(n) matches n single-step redos", {
  expect_identical(undoredo_rewound()$redo(0)$value, 5)
  expect_identical(undoredo_rewound()$redo(1)$value, 7)
  expect_identical(undoredo_rewound()$redo(2)$value, 3)
  expect_identical(undoredo_rewound()$redo(3)$value, 8)
  expect_identical(undoredo_rewound()$redo(4)$value, 1)

  expect_identical(undoredo_rewound()$redo(1)$value, undoredo_rewound()$redo()$value)
  expect_identical(undoredo_rewound()$redo(2)$value, undoredo_rewound()$redo()$redo()$value)
  expect_identical(undoredo_rewound()$redo(3)$value, undoredo_rewound()$redo()$redo()$redo()$value)
  expect_identical(undoredo_rewound()$redo(4)$value, undoredo_rewound()$redo()$redo()$redo()$redo()$value)

  expect_identical(undoredo_rewound()$redo(0)$redo_size, 4L)
  expect_identical(undoredo_rewound()$redo(1)$redo_size, 3L)
  expect_identical(undoredo_rewound()$redo(2)$redo_size, 2L)
  expect_identical(undoredo_rewound()$redo(3)$redo_size, 1L)
  expect_identical(undoredo_rewound()$redo(4)$redo_size, 0L)

  expect_identical(undoredo_rewound()$redo(0)$undo_size, 0L)
  expect_identical(undoredo_rewound()$redo(1)$undo_size, 1L)
  expect_identical(undoredo_rewound()$redo(2)$undo_size, 2L)
  expect_identical(undoredo_rewound()$redo(3)$undo_size, 3L)
  expect_identical(undoredo_rewound()$redo(4)$undo_size, 4L)
})

test_that("UndoManager undo(n)/redo(n) leave the stacks in the right order", {
  expect_identical(undoredo_test()$undo(3)$redo()$value, 3)
  expect_identical(undoredo_test()$undo(3)$redo()$redo()$value, 8)
  expect_identical(undoredo_test()$undo(3)$redo()$redo()$redo()$value, 1)

  expect_identical(undoredo_rewound()$redo(3)$undo()$value, 3)
  expect_identical(undoredo_rewound()$redo(3)$undo()$undo()$value, 7)
  expect_identical(undoredo_rewound()$redo(3)$undo()$undo()$undo()$value, 5)

  expect_identical(undoredo_test()$undo(4)$redo(4)$value, 1)
  expect_identical(undoredo_test()$undo(4)$redo(4)$undo_size, 4L)
  expect_identical(undoredo_test()$undo(4)$redo(4)$redo_size, 0L)
})

test_that("UndoManager undo(n)/redo(n) clamp instead of erroring", {
  five <- function() {
    x <- UndoManager$new()
    for (i in 1:5) x$do(i)
    x
  }

  expect_identical(five()$undo(99)$value, 1L)
  expect_identical(five()$undo(99)$undo_size, 0L)
  expect_identical(five()$undo(99)$redo_size, 4L)

  expect_identical(five()$undo(Inf)$value, 1L)
  expect_identical(five()$undo(Inf)$redo_size, 4L)

  expect_identical(five()$undo(Inf)$redo(99)$value, 5L)
  expect_identical(five()$undo(Inf)$redo(Inf)$value, 5L)
  expect_identical(five()$undo(Inf)$redo(Inf)$undo_size, 4L)
  expect_identical(five()$undo(Inf)$redo(Inf)$redo_size, 0L)

  expect_identical(five()$undo(Inf), five()$undo()$undo()$undo()$undo())
})

test_that("UndoManager undo(0)/redo(0) are no-ops", {
  expect_identical(
    UndoManager$new()$do(1)$do(2)$undo(0),
    UndoManager$new()$do(1)$do(2)
  )
  expect_identical(
    UndoManager$new()$do(1)$do(2)$undo()$redo(0),
    UndoManager$new()$do(1)$do(2)$undo()
  )
})

test_that("UndoManager undo(n)/redo(n) validate `n`", {
  bad_values <- list(-1, 1.5, NA, NA_real_, "2", c(1, 2), NULL, TRUE, -Inf, NaN)
  for (bad in bad_values) {
    expect_error(UndoManager$new()$do(1)$do(2)$undo(bad), "`n` must be")
    expect_error(UndoManager$new()$do(1)$do(2)$redo(bad), "`n` must be")
  }
})

test_that("UndoManager undo(n)/redo(n) invalidate exactly once", {
  x <- UndoManager$new()
  for (i in 1:5) x$do(i)

  expect_identical(count(x), 5)
  x$undo(2)$undo(2)
  expect_identical(count(x), 7)

  x$redo(99)
  expect_identical(count(x), 8)

  x$undo(0)
  x$redo(3)
  expect_identical(count(x), 8)

  x$undo(3)
  x$redo(0)
  expect_identical(count(x), 9)
})

test_that("UndoManager print shows an empty manager", {
  expect_snapshot(print(UndoManager$new()))
  expect_snapshot(print(UndoManager$new("numeric")))
  expect_snapshot(print(UndoManager$new(c("numeric", "character"))))
})

test_that("UndoManager print shows the current item and both stacks", {
  expect_snapshot(print(UndoManager$new()$do(1)))
  expect_snapshot(print(UndoManager$new("numeric")$do(1)$do(2)$do(3)))
  expect_snapshot(print(UndoManager$new()$do(1)$do(2)$do(3)$undo()))
})

test_that("UndoManager print pluralises undo and redo counts", {
  expect_snapshot(print(UndoManager$new()$do(1)$do(2)))
  expect_snapshot(print(UndoManager$new()$do(1)$do(2)$do(3)))
  expect_snapshot(print(UndoManager$new()$do(1)$do(2)$undo()))
  expect_snapshot(print(UndoManager$new()$do(1)$do(2)$do(3)$undo()$undo()))
})

test_that("UndoManager stores and restores any type of object", {

  expect_identical(UndoManager$new()$do(list(a = 1, b = 2))$do(0)$undo()$value, list(a = 1, b = 2))
  expect_identical(UndoManager$new()$do(data.frame(x = 1:2))$do(0)$undo()$value, data.frame(x = 1:2))
  expect_identical(UndoManager$new()$do(matrix(1:4, 2))$do(0)$undo()$value, matrix(1:4, 2))
  expect_identical(UndoManager$new()$do(factor("a"))$do(0)$undo()$value, factor("a"))
  expect_identical(UndoManager$new()$do(as.Date("2024-01-15"))$do(0)$undo()$value, as.Date("2024-01-15"))
  expect_identical(UndoManager$new()$do(list(a = 1))$do(0)$undo()$redo()$value, 0)
})

test_that("UndoManager type restriction works for non-scalar classes", {
  expect_error(UndoManager$new("list")$do(list(1)), NA)
  expect_error(UndoManager$new("data.frame")$do(data.frame(x = 1)), NA)
  expect_error(UndoManager$new("matrix")$do(matrix(1:4, 2)), NA)
  expect_error(UndoManager$new("factor")$do(factor("a")), NA)
  expect_error(UndoManager$new("Date")$do(as.Date("2024-01-15")), NA)
  expect_error(UndoManager$new("list")$do(1), "must have class")
  expect_error(UndoManager$new("data.frame")$do(list(1)), "must have class")
})

test_that("UndoManager active bindings cannot be assigned to", {
  x <- UndoManager$new()$do(1)

  expect_error(x$value <- 2)
  expect_error(x$undo_size <- 2)
  expect_error(x$redo_size <- 2)
  expect_error(x$can_undo <- TRUE)
  expect_error(x$can_redo <- TRUE)
})

test_that("UndoManager do requires an item", {
  expect_error(UndoManager$new()$do(), "must be provided")
  expect_error(UndoManager$new()$do(1)$do(), "must be provided")
  expect_error(UndoManager$new("numeric")$do(), "must be provided")
})

test_that("UndoManager is untouched when do() cannot evaluate its item", {
  x <- UndoManager$new()$do(1)$do(2)
  expect_error(x$do())
  expect_identical(x$value, 2)
  expect_identical(x$undo_size, 1L)
  expect_identical(x$redo_size, 0L)
  expect_error(print(x), NA)

  y <- UndoManager$new()$do(1)$do(2)
  expect_error(y$do(stop("boom")))
  expect_identical(y$value, 2)
  expect_identical(y$undo_size, 1L)
  expect_identical(y$redo_size, 0L)

  z <- UndoManager$new()$do(1)$do(2)
  expect_error(z$do(no_such_variable))
  expect_identical(z$value, 2)
  expect_identical(z$undo_size, 1L)
  expect_identical(z$redo_size, 0L)
})

test_that("UndoManager stores reference objects by reference", {
  # Environments (and R6 objects) are not copied when they are added, so
  # changing one afterwards also changes what the history holds.
  env <- new.env()
  env$foo <- "before"
  x <- UndoManager$new()$do(env)$do("other")
  env$foo <- "after"
  expect_identical(x$undo()$value$foo, "after")
})

test_that("all.equal ignores how a manager reached its state", {
  expect_true(isTRUE(all.equal(
    UndoManager$new()$do(1)$clear(),
    UndoManager$new()$do(1)
  )))
  expect_true(isTRUE(all.equal(
    UndoManager$new()$do(1)$do(2)$undo()$do(9),
    UndoManager$new()$do(1)$do(9)
  )))
  expect_true(isTRUE(all.equal(UndoManager$new(), UndoManager$new())))
  expect_true(isTRUE(all.equal(
    UndoManager$new()$do(1)$do(2),
    UndoManager$new()$do(1)$do(2)
  )))
})

test_that("all.equal compares the value, the history and the type", {
  expect_false(isTRUE(all.equal(UndoManager$new()$do(1), UndoManager$new()$do(2))))
  expect_false(isTRUE(all.equal(
    UndoManager$new()$do(1)$do(2)$undo(),
    UndoManager$new()$do(1)$do(7)$undo()
  )))

  expect_false(isTRUE(all.equal(UndoManager$new()$do(1)$do(2), UndoManager$new()$do(2))))
  expect_false(isTRUE(all.equal(
    UndoManager$new()$do(1)$do(2)$do(3),
    UndoManager$new()$do(1)$do(2)$do(3)$undo()
  )))

  expect_false(isTRUE(all.equal(UndoManager$new("numeric")$do(1), UndoManager$new()$do(1))))
  expect_false(isTRUE(all.equal(UndoManager$new(), UndoManager$new()$do(1))))
})

test_that("all.equal rejects a non-UndoManager", {
  expect_false(isTRUE(all.equal(UndoManager$new(), list())))
  expect_match(all.equal(UndoManager$new(), list()), "not an", fixed = TRUE)
})

test_that("identical() still tells separate managers apart", {
  a <- UndoManager$new()$do(1)
  b <- UndoManager$new()$do(1)

  expect_true(all.equal(a, b))
  expect_false(identical(a, b))
  expect_true(identical(a, a))
})

test_that("UndoManager is_empty distinguishes an empty manager from a NULL value", {
  expect_true(UndoManager$new()$is_empty)
  expect_false(UndoManager$new()$do(NULL)$is_empty)
  expect_null(UndoManager$new()$value)
  expect_null(UndoManager$new()$do(NULL)$value)

  expect_false(UndoManager$new()$do(1)$is_empty)
  expect_false(UndoManager$new()$do(1)$do(2)$undo()$is_empty)
  expect_false(UndoManager$new()$do(1)$clear()$is_empty)

  expect_true(UndoManager$new()$do(1)$clear(clear_value = TRUE)$is_empty)
  expect_true(UndoManager$new()$undo()$is_empty)
  expect_false(UndoManager$new()$do(1)$clear(clear_value = TRUE)$do(NULL)$is_empty)
})

test_that("UndoManager is_empty is read-only", {
  x <- UndoManager$new()
  expect_error(x$is_empty <- TRUE)
})

test_that("UndoManager instances do not share state", {
  a <- UndoManager$new()
  b <- UndoManager$new()

  a$do(1)$do(2)
  expect_true(b$is_empty)
  expect_null(b$value)
  expect_identical(b$undo_size, 0L)

  b$do("x")
  expect_identical(a$value, 2)
  expect_identical(a$undo_size, 1L)
  expect_identical(b$value, "x")
  expect_identical(b$undo_size, 0L)

  a$undo()
  expect_identical(a$value, 1)
  expect_identical(b$value, "x")
})

test_that("UndoManager print shows NULL items", {
  expect_snapshot(print(UndoManager$new()$do(NULL)))
  expect_snapshot(print(UndoManager$new()$do(1)$do(NULL)$do(3)$undo()))
})

test_that("UndoManager reactive returns a cached reactive for the manager", {
  skip_if_not_installed("shiny")

  x <- UndoManager$new()$do(1)
  rx <- x$reactive()

  expect_true(is.function(rx))
  expect_s3_class(rx, "reactive")
  expect_identical(x$reactive(), x$reactive())
  expect_identical(x$reactive(), rx)
  shiny::isolate({
    expect_identical(rx(), x)
    expect_identical(rx()$value, 1)
  })

  y <- UndoManager$new()$do(1)$do(2)$undo()
  shiny::isolate(expect_identical(y$reactive()()$value, 1))
})

test_that("UndoManager reactive notifies observers when the value changes", {
  skip_if_not_installed("shiny")

  runs <- function(action) {
    m <- UndoManager$new()$do(1)$do(2)
    r <- m$reactive()
    n <- 0
    shiny::testServer(
      function(input, output, session) {
        shiny::observe({ r(); n <<- n + 1 })
      },
      {
        session$flushReact()
        n <<- 0
        action(m)
        session$flushReact()
      }
    )
    n
  }

  expect_identical(runs(function(m) m$do(3)), 1)
  expect_identical(runs(function(m) m$undo()), 1)
  expect_identical(runs(function(m) m$clear()), 1)
  expect_identical(runs(function(m) m$undo(2)), 1)

  # No-ops must not notify
  expect_identical(runs(function(m) m$redo()), 0)
  expect_identical(runs(function(m) m$undo(0)), 0)
  expect_identical(runs(function(m) m$redo(99)), 0)
})

test_that("UndoManager reactive drives an output", {
  skip_if_not_installed("shiny")

  shiny::testServer(function(input, output, session) {
    m <- UndoManager$new()$do("first")
    r <- m$reactive()
    output$txt <- shiny::renderPrint(r()$value)

    session$flushReact()
    expect_identical(output$txt, '[1] "first"')

    m$do("second")
    session$flushReact()
    expect_identical(output$txt, '[1] "second"')

    m$undo()
    session$flushReact()
    expect_identical(output$txt, '[1] "first"')
  }, {})
})

test_that("UndoManager reactives are independent between managers", {
  skip_if_not_installed("shiny")

  a <- UndoManager$new()$do(1)
  b <- UndoManager$new()$do(1)
  ra <- a$reactive()
  rb <- b$reactive()
  na <- 0
  nb <- 0

  shiny::testServer(
    function(input, output, session) {
      shiny::observe({ ra(); na <<- na + 1 })
      shiny::observe({ rb(); nb <<- nb + 1 })
    },
    {
      session$flushReact()
      na <<- 0
      nb <<- 0
      a$do(2)
      session$flushReact()
    }
  )

  expect_identical(na, 1)
  expect_identical(nb, 0)
})
