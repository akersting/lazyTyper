# Get the Value of the \code{named} Field of a Variable
#
# @param varname the name of the variable as a character string.
# @param env the environment of the variable.
#
# @note If the variable is a promise, this will return \code{named} of the
#   promise itself and not of the value of the promise.
#
# @return the value of the \code{named} field (currently either 0L, 1L or 2L).
getNamed <- function(varname, env = parent.frame()) {
  .Call(C_getNamed, varname = varname, env = env)
}


# Set the Value of the \code{named} Field of a Variable
#
# @param varname the name of the variable as a character string.
# @param named 0, 1 or 2.
# @param env the environment of the variable.
#
# @note If the variable is a promise, this will set \code{named} of the
#   promise itself and not of the value of the promise.
#
# @return NULL (invisibly).
setNamed <- function(varname, named, env = parent.frame()) {
  invisible(.Call(C_setNamed, varname = varname, named = named, env = env))
}


# Evaluate an Expression and Save the Result Without Increasing \code{named}
#
# @param expr the expression to evaluate.
# @param eval_env the environment where to evaluate the expression.
# @param reference_name the name of the reference to create to the return value
#   of the expression.
# @param reference_env the environment where to assign the reference.
#
# @details Compared to base \code{eval}, this does not create a context. This
#   matters if \code{expr} contains calls to function to access the function
#   call stack.
#
# @return \code{expr} (evaluated).
eval2Reference <- function(expr, reference_name, eval_env = parent.frame(),
                           reference_env = parent.frame()) {
  .Call(C_eval2Reference, expr = expr, reference_name = reference_name,
        eval_env = eval_env, reference_env = reference_env)
}


# Evaluate the Code of a Promise Without Setting \code{named} to 2
#
# Evaluate the code of a promise in the appropriate environment and create a
# reference to the return value in the specified environment without increasing
# \code{named} and without actually forcing the promise.
#
# @param promise_name the name of the promise as a character string.
# @param promise_emv the environment of the promise, i.e. where it is assigned.
# @param reference_name the name of the reference to create to the value of the
#   promise as a character string.
# @param reference_env the environment where to assign the reference.
#
# @details If the promise indeed is a promise and this promise had not been
#   forced so far, then the promise code is evaluated in the appropriate
#   environment, the return value is assigned to the reference and is then
#   returned. Note that this does not force the promise, i.e. accessing it
#   afterwards will re-evalute the promise code (which generally is a bad
#   idea).
#
#   If the promise had been evaluated before, then the promise value is
#   assigned to the reference and then returned.
#
#   If \code{promise_name} actually does not refer to a promise but rather to
#   some other object, its value is assigned to the reference and then
#   returned. This is crucial since objects which are promises in plain R code
#   might no longer be promises in byte-compiled code; they might have been
#   replaced with their (constant) value.
#
# @return the value of the promise/object.
evalPromiseCode <- function(promise_name, promise_env = parent.frame(),
                            reference_name = promise_name,
                            reference_env = promise_env) {
  .Call(C_evalPromiseCode, promise_name = promise_name,
        promise_env = promise_env, reference_name = reference_name,
        reference_env = reference_env)
}


# Get the Value of a Variable
#
# @param varname the name of the variable as a character string.
# @param env the environment of the variable.
#
# @details Differences to base \code{get}:
#   - never searches the enclosing frames of \code{env} for the variable
#   - does not touch the \code{named} field
#   - does not fail if the variable is the missing argument / empty name
#
# @param the value of \code{varname} in \code{env}.
simpleGet <- function(varname, env = parent.frame()) {
  .Call(C_simpleGet, varname = varname, env = env)
}
