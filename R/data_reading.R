
#' Reads a folder of json files to a tibble
#' 
#' @param sourcefolder the folder containing the files (must end with '/')
#' 
#' @importFrom jsonlite read_json
#' @importFrom pbapply pblapply
#' @importFrom dplyr  %>% as_tibble
#' @export

TibbleFromJson <- function(sourcefolder){
#cat *.json  | jq  '[.[] | {year:.year, match:.match, fp:.firstpara, sp:.secondpara, parsed:.parsed_match}]' > combined1.json }]]
    mydata <- pblapply(list.files(sourcefolder),
           function(f){
               read_json(paste0(sourcefolder, f)) %>% 
                   lapply(function(x)as_tibble(x)) %>% 
                   do.call(rbind,.)
           })  %>% do.call(rbind,.)
    return (mydata)
}

