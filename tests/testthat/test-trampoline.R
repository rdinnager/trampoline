test_that("errors are thrown correctly", {

  expect_error(trampoline(2), "must be a call to a function or generator")
  expect_error(trampoline(NULL), "must be a call to a function or generator")

})

test_that("simple recursive count works before and after yield", {

  before <- 0
  after <- 0

  count <- coro::generator(function(n) {
    if(n <= 0) {
      return(trm_return("Done"))
    }
    before <<- before + 1
    n <- yield(count(n - 1))
    after <<- after + 1
    return(n)
  })

  n <- trampoline(count(30))

  expect_identical(before, 30)
  expect_identical(after, 30)
  expect_identical(n, "Done")


})

test_that("tail call recursion works", {

  factorial1 <- coro::generator(function(n, x = 1) {
    force(x) ## necessary thanks to R's lazy evaluation
    if(n <= 1) {
      return(trm_return(x))
    }
    val <- trm_tailcall(factorial1(n - 1, x * n))
    return(val)
  })

  factorial <- function(n) {
    if(n <= 1) {
      return(1)
    }
    val <- factorial(n - 1)
    return(val * n)
  }

  t1 <- trampoline(factorial1(5000))
  t2 <- trampoline(factorial1(10))
  t3 <- factorial(10)

  expect_equal(t1, Inf)
  expect_equal(t2, t3)

})

test_that("named arguments for trampoline work", {

  expect_output(
    trampoline(print_numbers(5),
               print_numbers = coro::generator(function(n) {
                 if(n >= 1) {
                   yield(print_numbers(n - 1))
                   print(n)
                  }
                })
               ),
    "\\[1\\] 1\\n\\[1\\] 2\\n\\[1\\] 3\\n\\[1\\] 4\\n\\[1\\] 5"
  )

  print_nums <- coro::generator(function(n) {
    if(n >= 1) {
      yield(print_nums(n - 1))
      print(n)
    }
  })

  expect_output(
    trampoline(print_nums(5),
               print_nums = print_nums),
    "\\[1\\] 1\\n\\[1\\] 2\\n\\[1\\] 3\\n\\[1\\] 4\\n\\[1\\] 5"
  )

  expect_true(
    trampoline(even(10),
               even = function(n) {
                 if (n == 0) trm_return(TRUE) else yield(odd(n - 1))
                },
               odd = function(n) {
                 if (n == 0) trm_return(FALSE) else yield(even(n - 1))
                })
  )

  expect_false(
    trampoline(even(11),
               even = function(n) {
                 if (n == 0) trm_return(TRUE) else yield(odd(n - 1))
               },
               odd = function(n) {
                 if (n == 0) trm_return(FALSE) else yield(even(n - 1))
               })
  )

  expect_true(
    trampoline(odd(11),
               even = function(n) {
                 if (n == 0) trm_return(TRUE) else yield(odd(n - 1))
               },
               odd = function(n) {
                 if (n == 0) trm_return(FALSE) else yield(even(n - 1))
               })
  )

  expect_false(
    trampoline(odd(10),
               even = function(n) {
                 if (n == 0) trm_return(TRUE) else yield(odd(n - 1))
               },
               odd = function(n) {
                 if (n == 0) trm_return(FALSE) else yield(even(n - 1))
               })
  )

})
