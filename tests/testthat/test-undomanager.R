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

test_that("UndoManager cannot do NULL items", {
  expect_error(UndoManager$new()$do(NULL))
  expect_error(UndoManager$new()$do(1)$do(2), NA)
  expect_error(UndoManager$new()$do(1)$do(2)$do(NULL))
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
  expect_error(UndoManager$new("character")$do(1))
  expect_error(UndoManager$new("numeric")$do("a"))
  expect_error(UndoManager$new("numeric")$do(1)$do("a"))
  expect_error(UndoManager$new("character")$do("a")$do(a))
})

test_that("UndoManager with multiple types", {
  expect_error(UndoManager$new(c("character", "numeric"))$do(1), NA)
  expect_error(UndoManager$new(c("character", "numeric"))$do("a"), NA)
  expect_error(UndoManager$new(c("character", "numeric"))$do("a")$do(1), NA)
  expect_error(UndoManager$new(c("character", "numeric"))$do("a")$do(1)$do(1L))
  expect_error(UndoManager$new(c("character", "integer", "numeric"))$do("a")$do(1)$do(1L), NA)
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
