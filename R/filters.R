
#' A 'depsearcheR' filter for getting certain kinds of ASVO sentences
#' 
#' @param sent input sentence in conll format
#' 
#' @export

GetAsvo <- function (sent)
{
    options(conll_cols = c("tokenid", "token", "lemma", "pos", "pos2", "feat", "head", "dep", "null1", "null2"))
    locs <- list(S = NA, V = NA, O = NA, A = NA)
    finverbs <- FilterConllRows(sent, "lemma", "вчера") %>%
        GetHeads(sent) %>% FilterConllRows("feat", "VerbForm=Fin", T)
    deps_of_finverbs <- finverbs %>% GetDeps(sent)
    subj <- deps_of_finverbs %>% FilterConllRows("dep", "nsubj")
    obj <- deps_of_finverbs %>% FilterConllRows("dep", c("obl", "obj")) %>%
        FilterConllRows("pos", "ADP", is_negative = T)
    adv <- deps_of_finverbs %>% FilterConllRows("lemma", "вчера")
    if (nrow(finverbs)) {
        locs$V = finverbs$tokenid[1]
    }
    if (nrow(subj)) {
        locs$S = subj$tokenid[1]
    }
    if (nrow(obj)) {
        locs$O = obj$tokenid[1]
    }
    if (nrow(adv)) {
        locs$A = adv$tokenid[1]
    }
    if (!is.na(locs$A) & !is.na(locs$S) & !is.na(locs$V) & !is.na(locs$O)) {
        if (locs$A < locs$S & locs$S < locs$V & locs$V < locs$O) {
            return(finverbs)
        }
    }
    return(sent %>% FilterConllRows("lemma", "lakjdslakjsdlaksd"))
}
