# exploRail project : Functions 


#' Create a session on trainline.com
#'
#' @param origin Origin city
#' @param destination Destination city
#' @param sess LiveHTML object with trainline session
#'
#' @return session
session_configuration <- function(origin, destination, sess){
  
  # origin and destination
  sess$type('input[id="from.search"', origin); sess$press('input[id="from.search"', "Enter")
  Sys.sleep(1)
  sess$type('input[id="to.search"', destination); sess$press('input[id="to.search"', "Enter")
  
  # other options like 'direct' or 'carte jeune' (young card) ?
  
  # validation
  sess$click("._1o7t43lNaN")
  
  # # click on 'plus tard' (later) button # mettre des try(sess$click si trop rapide)
  Sys.sleep(1); sess$click("._aho4gji"); Sys.sleep(5); sess$click("._aho4gji")
  Sys.sleep(5); sess$click("._aho4gji"); Sys.sleep(5); sess$click("._aho4gji")
  Sys.sleep(5)
  if(!is.na(sess %>% html_element("._aho4gji"))) sess$click("._aho4gji")
  if(any(c(origin, destination) %in% c("Lyon", "Marseille"))){
    # more trains for Lyon and Marseille
    Sys.sleep(5)
    if(!is.na(sess %>% html_element("._aho4gji"))) sess$click("._aho4gji")
  }
  
  return(sess)
  
}


#' Extract time for a trip
#'
#' @param train train
#'
#' @return date and time of a train
get_time <- function(train){
  
  res <- train %>%
    html_element("._zg4afg") %>%
    html_elements("time") %>%
    html_attr("datetime") %>%
    as.POSIXct(tryFormats = c("%Y-%m-%dT%H:%M:%OS"))
  
  return(res)
}


#' Extract prices for a trip
#'
#' @param train train
#'
#' @return prices of a train
get_price <- function(train){
  
  if(!is.na(train %>% html_element("._iejdyhf"))){
    
    df_price <- data.frame(class = c("2nde", "1ere"),
                           price = c("not_yet_for_sale", NA))
    
  }else{
    
    text <- train %>%
      html_element("._tyd8k6") %>%
      html_elements("._1e867qro") %>%
      html_text()
    
    if(identical(text, character(0))){
      
      df_price <- data.frame(class = c("2nde", "1ere"),
                             price = NA)
      
    }else{
      
      class <- str_extract(text, pattern = "\\w+(?=\\s+classe)") %>% stri_trans_general("Latin-ASCII")
      price <- str_extract(text, pattern = ".+?(?=€)") %>% str_trim()
      
      df_price <- data.frame(class = class,
                             price = price)
      
    }
  }
  
  df_type <- data.frame(class = c("2nde", "1ere"))
  res <- left_join(df_type,
                   df_price,
                   by = "class")
  
  res <- res %>%
    pivot_wider(names_from = "class",
                values_from = "price",
                names_prefix = "prix_")
  
  return(res)
}


#' Extract type train for a trip
#' 
#' @description Like "inoui" or "OUIGO"
#'
#' @param train train
#'
#' @return type train
get_type_train <- function(train){
  
  type_train <- train %>%
    html_elements(".icons1") %>%
    html_attr("data-testid")
  
  if(identical(type_train, character(0))){
    type_train <- "train_connection"
  }
  return(type_train)
}


# Wrapper with all extractions in a data frame
get_informations <- function(train){
  
  # time train
  times <- get_time(train)
  
  # type train
  type_train <- get_type_train(train)
  
  # price
  prices <- get_price(train)
  
  res <- data.frame(departure = times[1],
                    arrival = times[2],
                    type_train = type_train,
                    price_2nde = prices$prix_2nde,
                    price_1ere = prices$prix_1ere)
  
  return(res)
}


wrapper <- function(origin, destination, session, local_save, save_date){
  
  time_scrap <- Sys.time()
  
  active_session <- session_configuration(origin = origin,
                                          destination = destination,
                                          sess = session)
  
  # Like a filter on the chosen day in date input
  Sys.sleep(2)
  trains_of_the_days <- active_session %>%
    html_elements("._5l6ub9") %>%
    html_children() %>%
    html_children() %>%
    html_element(xpath = '//div[@role="tabpanel"]') %>%
    html_children()
  
  trains_informations <- lapply(trains_of_the_days, get_informations)
  results <- do.call("rbind", trains_informations)
  
  results <- results %>%
    mutate(origin = origin,
           destination = destination,
           time_scrap = time_scrap) %>%
    relocate(any_of(c("origin", "destination")), .before = 1) %>%
    mutate(across(starts_with("price"), ~replace(., is.na(.), "complet")))
  
  results$price_1ere[results$type_train %in% c("OUIGO", "ouigo_train_classique", "SNCF")] = "no_first_class"
  
  if(local_save == TRUE){
    if(!dir.exists("data_temp")) dir.create("data_temp")
    scrap_hour = ifelse(as.numeric(format(Sys.time(), "%H")) < 15, "10h", "22h")
    filename = paste0(paste(origin, destination, save_date, Sys.Date(), scrap_hour, sep = "_"), ".rds")
    results %>%
      saveRDS(file = file.path("data_temp", filename))
  }
  
  return(results)
  
}

# date config
maxi_wrap <- function(dates, origin, destination, session, local_save){
  
  date = dates %>% as.Date()
  
  month = format(date, format = "%m") %>% as.numeric()
  day = format(date, format = "%d") %>% as.numeric()
  css_date <- paste0('button[id="page.journeySearchForm.outbound.title', month, '-', day, '"') ### mettre une condition si ce n'est pas le bon mois
  
  # click on calendar
  if(!is.na(session$html_elements("._uolhtnNaN"))){
    session$click("._uolhtnNaN") 
  }else{
    session$click("._t0wlmwNaN") 
  }
  
  Sys.sleep(0.3)
  session$click(css_date)
  
  results <- pmap(list(origin, destination),
                  .f = possibly(wrapper),
                  session = session,
                  local_save = local_save,
                  save_date = dates)
  
  return(results)
  
}

# Get information for all trains in a selected day
# date au format date
get_trains <- function(origin, destination, date, local_save = FALSE, view_current_session = FALSE){ # , session = NULL
  
  # checks arguments
  if(length(origin) != length(destination)) stop("origin and destination have different length")
  if(is.character(date[1])) stop("'date' argument is on character format. Try with date format %Y-%m-%d")
  
  # order date
  
  # open session (with the first date of 'date' vector)
  # url <- paste0("https://www.thetrainline.com/book/results?journeySearchType=single&origin=urn%3Atrainline%3Ageneric%3Aloc%3A5097&destination=urn%3Atrainline%3Ageneric%3Aloc%3A4920&outwardDate=",
  #               date[1],
  #               "T00%3A00%3A26&outwardDateType=departAfter&inwardDateType=departAfter&selectedTab=train&selectExactTime=true&splitSave=true&lang=fr&transportModes[]=mixed&dpiCookieId=YWKQGT428W51RKN4OK1STWT9M&partnershipType=accommodation&partnershipSelection=true")
  
  url <- paste0("https://www.thetrainline.com/book/results?journeySearchType=single&origin=urn%3Atrainline%3Ageneric%3Aloc%3A4916&destination=urn%3Atrainline%3Ageneric%3Aloc%3A5097&outwardDate=",
                date[1],
                "T00%3A00%3A43&outwardDateType=departAfter&inwardDateType=departAfter&selectedTab=train&splitSave=true&lang=fr&transportModes[]=mixed&dpiCookieId=JAQR52NULVCEY9HK2KWEIPPUU&partnershipType=accommodation&partnershipSelection=true&selectedOutward=or57ZCqIISA%3D%3Aou4gfFrOW%2Bg%3D%3AStandard")
  
  sess <- read_html_live(url)
  
  # session check (if no bot)
  if(identical(is.na(sess$html_elements("._ai52v9NaN")), logical(0))){
    sess$click("._13sgtmyNaN")
    message("no session")
  }
  
  if(view_current_session == TRUE) sess$view()
  
  # close pop-up window
  Sys.sleep(2)
  if(!is.na(sess %>% html_element(".onetrust-close-btn-handler.banner-close-button.ot-close-link"))){
    sess$click(".onetrust-close-btn-handler.banner-close-button.ot-close-link")
  }
  
  # human simulation
  Sys.sleep(1)
  sess$scroll_by(top = sample(100:1000,1))
  
  # # human simulation à mettre dans la boucle ? ou montrer pate blanche avant
  # acion_prob <- sample(c(rep(TRUE,5), rep(FALSE, 45)), 1)
  # if(acion_prob){
  #   Sys.sleep(3)
  #   # scrol
  #   Sys.sleep(3)
  # }
  
  # sort dates
  date_char = date[order(date)] %>% as.character()
  
  results <- map(date_char,
                 .f = possibly(maxi_wrap),
                 origin = origin,
                 destination = destination,
                 session = sess,
                 local_save = local_save,
                 .progress = TRUE)
  
  sess$session$close()
  rm(sess)
  
  return(results)
  
}
