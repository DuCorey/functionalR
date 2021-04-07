#' Functional patterns in R

#' packages
library(pryr)
library(purrr)

#' functions
"%c%" <- pryr::compose


quoted_call <- function(fun, ..., dots = NULL) {
    #' do.call but always quoted
    do.call(fun, enlist(..., dots = dots), quote = TRUE)
}


#' State/event pattern
update_state <- function(current_state, event, how = c("apply", "parallel", "call"), ...) {
    #' Apply the event function to the current state of object
    #' Can be computed
    how <- match.arg(how, c("apply", "parallel", "call"))

    res <- switch(how, parallel = {
        mclapply(current_state, event, ...)
    }, apply = {
        lapply(current_state, event, ...)
    }, call = {
        quoted_call(event, current_state, dots = ...)
    })

    return(res)
}


unzip_update_state <- function(current_state, event) {
    #' Unzip the event args which are a list and call the update_state function
    #' with the all args passed to update state.
    #' If no args passed than it will use the default update_state args
    return(do.call(update_state, c(list(current_state = current_state), event), quote = TRUE))
}


end_state <- function(start_state, events) {
    #' Events are a list of two elements which contain the event function and
    #' the args to be given to the update_state function

    return(Reduce(unzip_update_state, init = start_state, x = events))
}


cross_apply <- function(l1, l2, FUN) {
    #' Apply a function over two list where every element is applied to function
    #' crosswise
    #' Example
    #' l1 <- c(A, B)
    #' l2 <- c(C, D)
    #' will be evaluated
    #' f(A,C), f(A,D), f(B,C), f(B,D)
    return(sapply(l1, function(x) sapply(l2, function(y) FUN(x, y))))
}


filtered_substitute <- function(expr, env) {
    #' Works exactly like substitute but removes any arguments from the expression
    #' that are NULL from the environment. Currently it works only with NULL keyword
    #' Could be extended to custom keyword.
    #' The expression must be quoted to be passed properly.

    #' Example
    #' f1 <- function(x=1,y=1,z=1) x+y+z
    #' expr <- quote(f1(x = a, y = 2, z = b))
    #' env <- list(a = 1, b = NULL)
    #' filted_substitute(expr, env)
    #' f1(x=1, y=2)

    ## Determine which elements in our environment are NULL
    null_ind <- names(env)[sapply(env, is.null)]

    ## Convert the expression into a named list
    list_expr <- as.list(expr)

    ## Remove from the named expr list the elements which are from our null list
    ## Convert the list back into a function call
    new_expr <- as.call(list_expr[!(list_expr %in% null_ind)])

    ## Substitute the new expression using the envrionment
    sub_new_expr <- do.call("substitute", args = list(expr=new_expr, env = env))
    return(sub_new_expr)
}

#' purrr improvements
reduce_subset_call <- getFromNamespace("reduce_subset_call", "purrr")


force_assign_in <- function(x, where, value) {
    #' Does the same as assign_in or pluck<- but doesn't check if the where
    #' exists in the x object. If the indexes don't exist they are created
    #' during the assignment. This function is a lot more dangerous than the
    #' normal assign_in use with caution.
    call <- reduce_subset_call(quote(x), as.list(where))
    call <- call("<-", call, value)
    eval(call)

    return(x)
}


pluck_list <- function(l, ...) {
    #' Apply plus over a list of object, plucing each ones with args
    return(lapply(l, function(x) purrr::pluck(x, ...)))
}
