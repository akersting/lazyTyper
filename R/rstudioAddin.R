# nocov start
insertTypedAssignAddin <- function() {
  rstudioapi::insertText(" %<-% ")
}
insertTypedAssignWOSpaceAddin <- function() {
  rstudioapi::insertText("%<-% ")
}
insertSecureTypedAssignAddin <- function() {
  rstudioapi::insertText(" %<-s% ")
}
insertSecureTypedAssignWOSpaceAddin <- function() {
  rstudioapi::insertText("%<-s% ")
}
# nocov end
