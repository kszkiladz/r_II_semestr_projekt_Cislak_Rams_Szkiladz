#' Operator potoku (pipe)
#'
#' Re-eksport operatora `\%>\%` z pakietu \pkg{magrittr}, zgodnie z
#' konwencja tidyverse stosowana na zajeciach.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs wartosc przekazywana do `rhs`.
#' @param rhs funkcja, do ktorej trafia `lhs`.
#' @return Wynik wywolania `rhs(lhs)`.
NULL
