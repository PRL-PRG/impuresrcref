options(keep.source=T)

counter <- 42

walk <- function(e) {
  if (is.call(e)) {
    if (identical(e[[1]], quote(`if`))) {
      if (length(e) == 4) {
        new <- substitute(
          if({E}) {C} else {A}, 
          list(
            E=e[[2]],
            C=e[[3]],
            A=e[[4]]
          )
        )
        for (i in 2:4) {
          counter <<- counter + 1
          srcref <- counter #c(46,7,46,7,7,7,46,46)
          class(srcref) <- "srcref"
          attr(new[[i]], "srcref") <- list(counter, counter)
        }
        new
      }
    } else {
      as.call(lapply(e, walk))
    }
  } else {  
    e
  }
}

print_srcrefs <- function(expr) {
  if (is.function(expr)) {
    expr <- body(expr)
  }
  
  walk_ast <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], quote(`{`))) {
        sr <- attr(e, "srcref")
        
        if (!is.null(sr)) {
          cat(deparse(e), " :: ", sr[[1]], "\n")
        }
      }
      
      for (i in seq_along(e)) {
        walk_ast(e[[i]])
      }
      
    } else if (is.expression(e) || is.list(e) || is.pairlist(e)) {
      for (i in seq_along(e)) {
        walk_ast(e[[i]])
      }
    }
  }
  
  walk_ast(expr)
  invisible(NULL)
}

options(keep.source = TRUE)

# Helper to check if an expression is already braced
is_braced <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`{`))
}

# Helper to check if an expression is an `else if` chain
is_else_if <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`if`))
}

# Helper to safely wrap an expression in {} and impute a real srcref
make_braced <- function(expr, real_srcref) {
  new_expr <- call("{", expr)
  
  if (!is.null(real_srcref)) {
    # A `{` call expects its srcref attribute to be a list of srcrefs.
    # We reuse the real_srcref for both the brace itself and the expression inside
    # to maintain accurate mapping back to the original source line.
    attr(new_expr, "srcref") <- list(real_srcref, real_srcref)
  }
  
  new_expr
}

morph <- function(f) {
  B <- walk(body(f))
  attributes(B) <- attributes(body(f))
  body(f) <- B
  f
}

f <- function(x) {
  if (x) x - 1 else x + 1
}

g <- morph(f)

str(compiler::disassemble(compiler::cmpfun(f)))
cat("**** NEW\n")
G <- compiler::disassemble(compiler::cmpfun(g))
print_srcrefs(g)


