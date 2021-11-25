#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
trampoline <- function(...) {
  ## need this to disable byte compilation temporarily, because without disabling
  ## R attempts to compile every generator instance and it creates massive overhead
  local_compiler_level(0)

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
          cl <- rlang::call2(
            "function",
            rlang::fn_fmls(func),
            rlang::fn_body(func)
          )
          func <- coro::generator()
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
      rlang::env_poke(nm = names(dots)[i],
                      value = func)
    }
    if(i == 1) {
      main_call <- rlang::eval_tidy(dots[[i]])
    }
  }

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

    #print(class(res))

    if(!rlang::is_function(res)) {
      if(!coro::is_exhausted(res)) {
        if(inherits(res, "trampoline_tail_return")) {
          retval <- res
        }
      }
      invisible(stack$pop())
    } else {
      stack$push(res)
    }

  }
)


  if(is.null(retval)) {
    return(invisible(retval))
  } else {
    retval
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
tail_call <- function(x) {
  structure(x,
            class = "trampoline_tail_call")
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
tail_return <- function(x) {
  structure(x,
            class = "trampoline_tail_return")
}

#' @importFrom compiler enableJIT
local_compiler_level <- withr::local_(
  function(level) {
    compiler::enableJIT(level)
  },
  function(old_level) {
    compiler::enableJIT(old_level)
  }
)

