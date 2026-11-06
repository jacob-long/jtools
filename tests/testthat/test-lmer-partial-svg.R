test_that("the lmer partial plot tolerates only rounding of point heights", {
  point <- "<circle cx='3' cy='123.04' fill='black'/>"
  svg <- function(content) paste0("<svg>", content, "<text>Anger</text></svg>")
  reference <- svg(point)
  compare <- function(actual) compare_lmer_partial_svg(reference, actual)

  expect_true(compare(reference))
  expect_true(compare(sub("123.04", "123.05", reference, fixed = TRUE)))
  expect_true(compare(sub("123.04", "123.03", reference, fixed = TRUE)))
  expect_false(compare(sub("123.04", "123.06", reference, fixed = TRUE)))
  expect_false(compare(sub("cx='3'", "cx='3.01'", reference, fixed = TRUE)))
  expect_false(compare(sub("black", "red", reference, fixed = TRUE)))
  expect_false(compare(sub("Anger", "Height", reference, fixed = TRUE)))
  expect_false(compare(svg("")))
  expect_false(compare(svg(paste0(point, point))))
})
