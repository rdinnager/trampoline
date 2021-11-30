#' Title
#'
#' @param ...
#'
#' @return
#' @export
#' @importFrom compiler enableJIT
#' @examples
trampoline <- function(...) {
  ## need this to disable byte compilation temporarily, because without disabling
  ## R attempts to compile every generator instance and it creates massive overhead
  old_jit <- compiler::enableJIT(0)
  on.exit(compiler::enableJIT(old_jit), add = TRUE)

  dots <- rlang::enquos(..., .check_assign = TRUE)
  funcs <- list()
  for(i in seq_along(dots)) {
    if(names(dots)[i] == "") {
      if(rlang::quo_is_call(dots[[i]])) {
        expr <- rlang::quo_get_expr(dots[[i]])
        func <- rlang::env_get(nm = rlang::as_string(expr[[1]]), inherit = TRUE)
        if(!rlang::is_function(func)) {
          stop("unnamed arguments to trampoline must be a call to a function or generator")
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
                        value = func)
      } else {
        stop("unnamed arguments to trampoline must be a call to a function or generator")
      }
    } else {
      func <- rlang::eval_tidy(dots[[i]])
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
                      value = func)
    }
    if(i == 1) {
      main_call <- rlang::eval_tidy(dots[[i]])
    }
  }

  #print(factorial1)
  #print(ls())

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

tramampoline <- function(...) {
  trampoline(...)
}

trambopoline <- function(...) {
  trampoline(...)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
trm_tailcall <- function(x) {
  structure(x,
            class = "trampoline_tailcall")
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
trm_return <- function(x) {
  structure(x,
            class = "trampoline_return")
}



