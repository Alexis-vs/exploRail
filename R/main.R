# exploRail project : Functions 

# Create a session on trainline.com
session_configuration <- function(origin, destination, date){
  
  url <- paste0("https://www.thetrainline.com/book/results?journeySearchType=single&origin=urn%3Atrainline%3Ageneric%3Aloc%3A5097&destination=urn%3Atrainline%3Ageneric%3Aloc%3A4920&outwardDate=",
                date,
                "T00%3A00%3A26&outwardDateType=departAfter&inwardDateType=departAfter&selectedTab=train&selectExactTime=true&splitSave=true&lang=fr&transportModes[]=mixed&dpiCookieId=YWKQGT428W51RKN4OK1STWT9M&partnershipType=accommodation&partnershipSelection=true")
  sess <- read_html_live(url)
  
  # close pop-up window
  Sys.sleep(2)
  if(!is.na(sess %>% html_element(".onetrust-close-btn-handler.banner-close-button.ot-close-link"))){
    sess$click(".onetrust-close-btn-handler.banner-close-button.ot-close-link")
  }
  
  # origin and destination
  sess$type("._ai52v9NaN", origin); sess$press("._ai52v9NaN", "Enter")
  sess$type("._1qvaqukNaN", destination); sess$press("._1qvaqukNaN", "Enter")
  
  # other options like 'direct' or 'carte jeune' (young card) ?
  
  # validation
  sess$click("._1o7t43lNaN")
  
  # click on 'plus tard' (later) button
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

# Extract time for a trip
get_time <- function(train){
  
  res <- train %>%
    html_element("._zg4afg") %>%
    html_elements("time") %>%
    html_attr("datetime") %>%
    as.POSIXct(tryFormats = c("%Y-%m-%dT%H:%M:%OS"))
  
  return(res)
}

# Extract prices for a trip
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


# Extract type train for a trip like "inoui" or "OUIGO"
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

# Get information for all trains in a selected day
get_trains <- function(origin, destination, date){
  
  time_scrap <- Sys.time()
  session <- session_configuration(origin = origin,
                                   destination = destination,
                                   date = date)
  
  # Like a filter on the chosen day in date input
  Sys.sleep(2)
  trains_of_the_days <- session %>%
    html_elements("._5l6ub9") %>%
    html_children() %>%
    html_children() %>%
    html_element(xpath = '//div[@role="tabpanel"]') %>%
    html_children()
  
  rm(session)
  
  trains_informations <- lapply(trains_of_the_days, get_informations)
  results <- do.call("rbind", trains_informations)
  
  results <- results %>%
    mutate(origin = origin,
           destination = destination,
           time_scrap = time_scrap) %>%
    relocate(any_of(c("origin", "destination")), .before = 1) %>%
    mutate(across(starts_with("price"), ~replace(., is.na(.), "complet")))
  
  results$price_1ere[results$type_train %in% c("OUIGO", "ouigo_train_classique", "SNCF")] = "no_first_class"
  
  return(results)
  
}
