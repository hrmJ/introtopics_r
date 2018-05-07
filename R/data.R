#' A set of Finnish and Russian adverbials in SVO sentences
#'
#' A dataset containing the adverbials eilen/vchera and viime vuonna / v proshlom godu in different syntactic positions
#'
#' @format A tibble with n rows and y variables:
#' \describe{
#'   \item{sent}{The sentence in which the adverbial occures}
#'   ...
#' }
#' @source The Aranea corpora \url{http://ella.juls.savba.sk/aranea/index.html}, The finnish newspaper collection \url{http://urn.fi/urn:nbn:fi:lb-201405276}, The Russian National Corpus (newspapers),  \url{http://ruscorpora.ru/search-paper.html}

"adverbials"


#' Frequency table of head verbs having count > 5
#'
#' A dataset for determining wether or not a sentence with an adverbial has a 'presentational' main verb (eg. publish)
#'
#' @format A tibble with n rows and y variables:
#' \describe{
#'   \item{lang}{language of the verb}
#'   \item{headverb}{lemma of the verb}
#'   \item{n}{frequency of the lemma}
#'   \item{is_presentational}{y(es) / n(o)}
#'   ...
#' }

"headverbs"
