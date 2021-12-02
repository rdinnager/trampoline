#' Make a trampoline.
#'
#' This function takes a call to a generator factory, created by [coro::generator()] and
#' runs it as a trampoline, which allows any recursion in the generator function to
#' recurse theoretically forever (but usually just more than can be handled by R's
#' default call stack limits).
#'
#' @param call A call to a function or generator function. The function can be
#' one defined already in the calling environment or higher or can be defined as an argument
#' to `trampoline()`, see `...` argument.
#' @param ... A named list of functions or generator functions. Named arguments are function or
#' generator function definitions where the name of the argument should be the desired name of
#' the function (that is referred to also within the function for recursion, see examples to
#' get a clearer idea of what this means). Passing multiple named arguments is possible and
#' allows specification of functions that can be used within the generator function that is
#' called in `call` (again, the examples might make this clearer).
#'
#' @return If [trm_return()] or [trm_tailcall()] is called within the recursive generator
#' function, `trampoline()` will return the final return value from the final recursion.
#' Otherwise it will return `NULL` invisibly (in case the recursion is only for its
#' side-effects). See the examples for how this works.
#' @export
#' @importFrom compiler enableJIT
#' @examples
#' ## standard recursive function exhausts stack:
#' print_numbers <- function(n) {
#'   if(n >= 1) {
#'     print_numbers(n - 1)
#'     print(n)
#'   }
#' }
#' try(print_numbers(5000))
#'
#' ## use trampoline with a coro generator instead
#' print_numbers <- coro::generator(function(n) {
#'   if(n >= 1) {
#'     yield(print_numbers(n - 1))
#'     print(n)
#'   }
#' })
#' nums <- capture.output(
#'   trampoline(print_numbers(5000))
#' )
#' cat(tail(nums))
#'
#' ## Or just use a plain function (but still use yield())
#' print_numbers <- function(n) {
#'   if(n >= 1) {
#'     yield(print_numbers(n - 1))
#'     print(n)
#'   }
#' }
#'
#' trampoline(print_numbers(5))
#'
#' ## use an alias or another
#' tramampoline(print_numbers(5))
#' trambopoline(print_numbers(5))
#'
#' ## use multiple mutually recursive functions
#' even <- function(n) {
#'   if (n == 0) trm_return(TRUE) else yield(odd(n - 1))
#' }
#'
#' odd <- function(n) {
#'   if (n == 0) trm_return(FALSE) else yield(even(n - 1))
#' }
#'
#' ## doesn't work (you must pass odd in because trampoline
#' ## only converts first called function to generator by default)
#' try(trampoline(even(100)))
#'
#' ## does work
#' trampoline(even(100), odd = odd)
#'
#' ## you can specify your recursive function in the trampoline
#' ## call if you want.
#' ## Return a value using trm_return():
#' trampoline(factorial(13),
#'            factorial = function(n) {
#'              if(n <= 1) {
#'                return(trm_return(1))
#'              }
#'              val <- yield(factorial(n - 1))
#'              return(val * n)
#'            })
#'
#' ## convert to using tail call optimization by wrapping
#' ## recursive call in trm_tailcall()
#' trampoline(factorial(13),
#'            factorial = function(n, x = 1) {
#'              force(x) ## necessary thanks to R's lazy evaluation
#'              if(n <= 1) {
#'                return(trm_return(x))
#'              }
#'              val <- trm_tailcall(factorial(n - 1, x * n))
#'              return(val)
#'            })
trampoline <- function(call, ...) {
  ## need this to disable byte compilation temporarily, because without disabling
  ## R attempts to compile every generator instance and it creates massive overhead
  old_jit <- compiler::enableJIT(0)
  on.exit(compiler::enableJIT(old_jit), add = TRUE)

  dots <- rlang::enexprs(..., .check_assign = TRUE, .named = TRUE)
  call <- rlang::enquo(call)

  for(i in seq_along(dots)) {

    func <- eval(dots[[i]], envir = rlang::caller_env())

    if(!rlang::is_function(func)) {
     stop("the value of a named argument to trampoline must be a function or generator")
    }
    if(!inherits(func, "coro_generator")) {
      func <- do.call(coro::generator,
                      list(
                        rlang::expr(
                          !!rlang::call2(
                            "function",
                            rlang::fn_fmls(func),
                            rlang::fn_body(func)
                            )
                          )
                        )
      )
    } else {
      ## note to remember why I did this because it was a bit tricky
      ## I need to make sure that any calls to coro::generator in the arguments
      ## are evaluated inside this function so it can find the binding to its
      ## recursive call, which is also in this environment.
      ## But if a generator has been passed as a symbol binding a generator
      ## in the calling environment, we want it evaluated in the calling
      ## environment so that it can find the binding to its recursive call there
      ## So that is why we reevaluate here if we have a call to generator()
      if(rlang::is_call(dots[[i]], "generator")) {
        func <- eval(dots[[i]])
      }
    }
    if(!inherits(func, "coro_generator")) {
     stop("recursive function must be a generator")
    }
    rlang::env_poke(nm = names(dots)[i],
                     value = func,
                    env = rlang::current_env())

  }

  if(rlang::quo_is_call(call)) {
    expr <- rlang::quo_get_expr(call)
    fn_string <- rlang::as_string(expr[[1]])
    if(!fn_string %in% ls()) {
      func <- rlang::env_get(nm = fn_string, default = NULL, inherit = TRUE, env = rlang::caller_env())
      if(!rlang::is_function(func)) {
        stop("call must be a call to a function or generator")
      }
      if(!inherits(func, "coro_generator")) {
        func <- do.call(coro::generator,
                        list(
                          rlang::expr(
                            !!rlang::call2(
                              "function",
                              rlang::fn_fmls(func),
                              rlang::fn_body(func)
                            )
                          )
                        )
        )
      }
      if(!inherits(func, "coro_generator")) {
        stop("recursive function must be a generator")
      }
      rlang::env_poke(nm = rlang::as_string(expr[[1]]),
                      value = func,
                      env = rlang::current_env())

    }
  } else {
    stop("call must be a call to a function or generator")
  }

  main_call <- eval(rlang::quo_get_expr(call))

  ## trampoline starts here:--------------------------------------

  stack <- fastmap::faststack()
  stack$push(main_call)
  retval <- NULL

  while(stack$size() > 0) {
    if(is.null(retval)) {
      res <- stack$peek()()
    } else {
      value <- retval
      retval <- NULL
      res <- stack$peek()(value)
    }

    if(!rlang::is_function(res)) {
      if(!coro::is_exhausted(res)) {
        if(inherits(res, "trampoline_return")) {
          retval <- res
        }
      }
      invisible(stack$pop())
    } else {
      if(inherits(res, "trampoline_tailcall")) {
        invisible(stack$pop())
      }
      stack$push(res)
    }

  }

  if(is.null(retval)) {
    return(invisible(retval))
  } else {
    class_stripped <- class(retval)
    class_stripped <- class_stripped[class_stripped != "trampoline_return"]
    if(length(class_stripped) > 0) {
      class(retval) <- class_stripped
    } else {
      retval <- unclass(retval)
    }
    return(retval)
  }


}

#' @rdname trampoline
#' @export
tramampoline <- function(call, ...) {
  trampoline(call = {{ call }}, ...)
}

#' @rdname trampoline
#' @export
trambopoline <- function(call, ...) {
  trampoline(call = {{ call }}, ...)
}

#' Flag a tail call
#'
#' If you can specify your recursive function such that the
#' recursive call is in 'tail position' (that is, the very
#' last operation in your function), you can take advantage
#' of tail call optimization. Just wrap your recursive call
#' in `trm_tailcall()`
#'
#' @param x A recursive call within generator fed to [trampoline()]
#'
#' @return `x` with added class attribute 'trampoline_tailcall'
#' @export
#'
#' @examples
#' trampoline(factorial(13),
#'            factorial = function(n, x = 1) {
#'              force(x) ## necessary thanks to R's lazy evaluation
#'              if(n <= 1) {
#'                return(trm_return(x))
#'              }
#'              val <- trm_tailcall(factorial(n - 1, x * n))
#'              return(val)
#'            })
trm_tailcall <- function(x) {
  structure(x,
            class = "trampoline_tailcall")
}

#' Flag a return value
#'
#' Wrap a return value in your recursive function with `trm_return()`
#' to have it passed along and returned by your final recursion.
#'
#' @param x A value to be returned at the end of all recursions
#'
#' @return `x` with added class attribute 'trampoline_return'
#' @export
#'
#' @examples
#' trampoline(factorial(13),
#'            factorial = function(n) {
#'              if(n <= 1) {
#'                return(trm_return(1))
#'              }
#'              val <- yield(factorial(n - 1))
#'              return(val * n)
#'            })
trm_return <- function(x) {
  structure(x,
            class = c(class(x), "trampoline_return"))
}



