# nocov start

shiftCursorInLine <- function(offset) {
  adc <- rstudioapi::getActiveDocumentContext()
  adc$selection[[1]]$range$start[2] <- adc$selection[[1]]$range$start[2] +
    offset
  adc$selection[[1]]$range$end[2] <- adc$selection[[1]]$range$end[2] + offset
  rstudioapi::setCursorPosition(adc$selection[[1]]$range)
}

insertTypedAssignAddin <- function() {
  rstudioapi::insertText(" %<-% .()")
  shiftCursorInLine(-1)
}
insertTypedAssignWOSpaceAddin <- function() {
  rstudioapi::insertText("%<-% .()")
  shiftCursorInLine(-1)
}
insertSecureTypedAssignAddin <- function() {
  rstudioapi::insertText(" %<-s% .()")
  shiftCursorInLine(-1)
}
insertSecureTypedAssignWOSpaceAddin <- function() {
  rstudioapi::insertText("%<-s% .()")
  shiftCursorInLine(-1)
}

insertScopeAddin <- function() {
  rstudioapi::insertText("SCOPE %->% {}")
  shiftCursorInLine(-1)
}
# nocov end
