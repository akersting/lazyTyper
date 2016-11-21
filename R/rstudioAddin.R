# nocov start

shiftCursorInLine <- function(offset, adc) {
  adc$selection[[1]]$range$start[2] <- adc$selection[[1]]$range$start[2] +
    offset
  adc$selection[[1]]$range$end[2] <- adc$selection[[1]]$range$end[2] + offset
  rstudioapi::setCursorPosition(adc$selection[[1]]$range)
}

isSpace2Left <- function(adc) {
  line <- adc$contents[adc$selection[[1]]$range$start[1]]
  char2left <- substr(line, adc$selection[[1]]$range$start[2] - 1,
                      adc$selection[[1]]$range$start[2] - 1)
  if (nchar(char2left) > 0) {
    length(grep("[[:space:]]", char2left)) > 0
  } else {
    TRUE  # we are a the very beginning of a line
  }
}

insertTypedAssignAddin <- function() {
  adc <- rstudioapi::getActiveDocumentContext()
  if (isSpace2Left(adc)) {
    rstudioapi::insertText("%<-% .()")
    # 8 is nchar("%<-% .()")
    adc$selection[[1]]$range$start[2] <- adc$selection[[1]]$range$start[2] + 8
    adc$selection[[1]]$range$end[2] <- adc$selection[[1]]$range$start[2]
  } else {
    # 9 is nchar(" %<-% .()")
    rstudioapi::insertText(" %<-% .()")
    adc$selection[[1]]$range$start[2] <- adc$selection[[1]]$range$start[2] + 9
    adc$selection[[1]]$range$end[2] <- adc$selection[[1]]$range$start[2]
  }

  shiftCursorInLine(-1, adc)
}

insertSecureTypedAssignAddin <- function() {
  adc <- rstudioapi::getActiveDocumentContext()
  if (isSpace2Left(adc)) {
    rstudioapi::insertText("%<-s% .()")
    # 9 is nchar("%<-s% .()")
    adc$selection[[1]]$range$start[2] <- adc$selection[[1]]$range$start[2] + 9
    adc$selection[[1]]$range$end[2] <- adc$selection[[1]]$range$start[2]
  } else {
    rstudioapi::insertText(" %<-s% .()")
    # 10 is nchar(" %<-s% .()")
    adc$selection[[1]]$range$start[2] <- adc$selection[[1]]$range$start[2] + 10
    adc$selection[[1]]$range$end[2] <- adc$selection[[1]]$range$start[2]
  }

  shiftCursorInLine(-1, adc)
}

insertScopeAddin <- function() {
  adc <- rstudioapi::getActiveDocumentContext()
  rstudioapi::insertText("SCOPE %->% {}")
  # 13 is nchar("SCOPE %->% {}")
  adc$selection[[1]]$range$start[2] <- adc$selection[[1]]$range$start[2] + 13
  adc$selection[[1]]$range$end[2] <- adc$selection[[1]]$range$start[2]
  shiftCursorInLine(-1, adc)
}
# nocov end
