#' Make a trampoline.
#'
#' This function takes a call to a generator factory, created by [coro::generator()] and
#' runs it as a trampoline, which allows any recursion in the generator function to
#' recurse theoretically forever (but usually just more than can be handled by R's
#' default call stack limits).
#'
#' @param call A call to a generator function. The generator function can be
#' one defined already in the calling environment or higher (using [coro::generator()]),
#' or can be defined as an argument to `trampoline()`, see `...` argument.
#' @param ... A named list of generator functions. Named arguments are generator function
#' definitions where the name of the argument should be the desired name of the function
#' (that is referred to also within the function for recursion, see examples to get a clearer
#' idea of what this means). Passing multiple named arguments is possible and
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
trampoline <- function(call, ...) {
  ## need this to disable byte compilation temporarily, because without disabling
  ## R attempts to compile every generator instance and it creates massive overhead
  old_jit <- compiler::enableJIT(0)
  on.exit(compiler::enableJIT(old_jit), add = TRUE)

  dots <- rlang::enquos(..., .check_assign = TRUE, .named = TRUE)
  call <- rlang::enquo(call)

  for(i in seq_along(dots)) {

    func <- rlang::eval_bare(rlang::quo_get_expr(dots[[i]]))
    if(!rlang::is_function(func)) {
     stop("the value of a named argument to trampoline must be a function or generator")
    }
    if(!inherits(func, "coro_generator")) {
     func <- coro::generator(rlang::call2(
       "function",
       rlang::fn_fmls(func),
       rlang::fn_body(func)
     ))
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
        func <- coro::generator(rlang::call2(
          "function",
          rlang::fn_fmls(func),
          rlang::fn_body(func)
        ))
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

  main_call <- rlang::eval_bare(rlang::quo_get_expr(call))

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

    #print(res)

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
    return(unclass(retval))
  }


}

#' @inherit trampoline
#' @export
tramampoline <- function(...) {
  trampoline(...)
}

#' @inherit trampoline
#' @export
trambopoline <- function(...) {
  trampoline(...)
}

#' Flag a tail call
#'
#' @param x A recursive call within generator fed to [trampoline()]
#'
#' @return `x` with added class attribute 'trampoline_tailcall'
#' @export
#'
#' @examples
trm_tailcall <- function(x) {
  structure(x,
            class = "trampoline_tailcall")
}

#' Flag a return value
#'
#' @param x A value to be returned at the end of all recursions
#'
#' @return `x` with added class attribute 'trampoline_return'
#' @export
#'
#' @examples
trm_return <- function(x) {
  structure(x,
            class = "trampoline_return")
}



