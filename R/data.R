#' Campsis model suite.
#'
#' A library of models of all kinds, ready to be simulated in Campsis. These model
#' templates are sorted into the following categories: pharmacokinetic (PK), pharmacodynamic (PD),
#' target-mediated drug disposition (TMDD), NONMEM, literature  and other (custom models).
#'
#' @format A list with all the models:
#' \describe{
#'    \item{pk}{extensive list of pharmacokinetic (PK) model templates}
#'    \item{pd}{list of pharmacodynamic (PD) model templates, to be plugged into any pharmacokinetic (PK) model}
#'    \item{tmdd}{extensive list of target-mediated drug disposition (TMDD) model templates}
#'    \item{nonmem}{list of model templates translated from standard NONMEM control streams}
#'    \item{literature}{a couple of models coming from the literature}
#'    \item{other}{a couple of custom models}
#' }
#' @source \url{https://campsisverse.github.io/campsis.doc/}
#' @source \url{https://www.iconplc.com/solutions/technologies/nonmem/}
"model_suite"
