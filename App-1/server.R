

#source('D:/My Data/UIUC/CS598_Practical_Statistical_Learning/Project4/functions/exploratory_data_analysis.R')
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)
library(readr)
library(tidyr)
#----------------------------------------global Variables ----------------------------------------------------------------

#path = 'D:/My Data/UIUC/CS598_Practical_Statistical_Learning/Project4/'
small_image_url = "https://liangfgithub.github.io/MovieImages/"


ratings = read_csv(paste0('data/','ratings.csv'))
movies = read_csv(paste0('data/','movies.csv'))
# users = read_csv(paste0('data/','users.csv'))
genre_matrix = as.matrix(read_csv(paste0('data/','genre_matrix.csv')))

# num_of_movies = 3681

ratings = ratings %>%
  select(c("UserID","MovieID","Rating"))

rating_candidates = ratings %>%
                    group_by(MovieID) %>%
                    summarize(ratings_per_movie = n(), 
                              ave_ratings = round(mean(Rating), dig=3))%>%
                    inner_join(movies, by = 'MovieID')%>%
                    select(c("MovieID","Title",'ratings_per_movie',"ave_ratings"))%>%
                    filter(ratings_per_movie > 1000)%>%
                    arrange(desc(ave_ratings))

# 207 movies
# train test
set.seed(100)
train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.8)
train = ratings[train.id, ]
test = ratings[-train.id, ]

#--------------------------------------functions Start------------------------------------------------------------------

get_recomendation_df = function(demo_genre,total_recommendation=5){  # genre
  
  tmp = ratings %>%
    group_by(MovieID) %>%
    summarize(ratings_per_movie = n(), 
              ave_ratings = round(mean(Rating), dig=3))%>%
    inner_join(movies, by = 'MovieID')%>%
    inner_join(data.frame(MovieID = movies$MovieID, genre_matrix), 
               by = "MovieID") %>%
    select(c("MovieID","Title",demo_genre,'ratings_per_movie',"ave_ratings"))%>%
    filter(!!as.symbol(demo_genre)!=0)%>%
    filter(ratings_per_movie > 1000)%>%
    top_n(10, ave_ratings)%>%
    arrange(desc(ave_ratings))

  if (dim(tmp)[1]<total_recommendation){
    recommended_movie_ids = tmp$MovieID
  }else{    
    recommended_movie_ids = sample(tmp$MovieID, size=total_recommendation, replace=FALSE)
  }
  
  result = tmp[which(tmp$MovieID%in%recommended_movie_ids),]
  
}

##recommendation algorithm

get_rmat<- function(train){

  
  i = paste0('u', train$UserID)
  j = paste0('m', train$MovieID)
  x = train$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  
}

#Rmat = get_rmat(train)

get_rating_matrix <- function(value_list){
  value_list= value_list[grep("select_",names(value_list))]
  dat <- data.table(MovieID = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    Rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == "", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat <- dat[Rating > 0]

  current_user = max(unique(ratings$UserID))+1
  # cat("current_user: ", current_user,"\n")
  new_user = data.frame(UserID = current_user, MovieID = dat$MovieID, Rating = dat$Rating )
  #cat("new_user: \n")
  #print(new_user)
  
  # cat("check identical: ","\n")
  SD_zero_rating = length(unique(as.list(new_user$Rating))) == 1
  # print(SD_zero_rating)
  
  ratings = rbind(new_user,ratings)
  # cat("dimention of ratings:" ,dim(ratings),"\n")
  dimension_names <- list(UserID = sort(unique(ratings$UserID)), MovieID = sort(unique(ratings$MovieID)))
  ratingmat <- spread(select(ratings,  UserID, MovieID, Rating), MovieID, Rating) %>% select(-UserID)
  ratingmat <- as.matrix(ratingmat)
  dimnames(ratingmat) <- dimension_names
  rated_items = which(!is.na((as.data.frame(ratingmat[nrow(ratingmat),]))))
  selected_users <- names(which(apply(!is.na(ratingmat[ ,rated_items]), 1, sum) >= 2))
  rmat <- ratingmat[selected_users, ]

  user_mean_ratings <- rowMeans(rmat,na.rm=T)

  rmat <- rmat - user_mean_ratings
  return_list = list("rmat" = rmat, "current_user" = current_user, "SD_zero_rating" = SD_zero_rating)
  return (return_list)
}


get_similarities<- function(rmat,current_user,std_zero){
  
  
  if (std_zero==TRUE){
    #cat("std_zero: True","\n")
    trimed_rmat <- rmat[,which(!is.na((as.data.frame(rmat[nrow(rmat),]))))]
    
    trimed_rmat_mean <- rowMeans(trimed_rmat,na.rm=T)
    
    trimed_rmat_mean <- abs(trimed_rmat_mean - trimed_rmat_mean[length(trimed_rmat_mean)])

    sorted_trimed_rmat_mean = sort(trimed_rmat_mean)
    
    top10 = sorted_trimed_rmat_mean[names(sorted_trimed_rmat_mean)!=current_user]
    top10 = top10[1:10]
    recomment_5 = sample(top10,5)
    similar_users <- names(recomment_5)

    
  }
  else{
    #cat("std_zero: False","\n")
    similarities <- cor(t(rmat[rownames(rmat)!=current_user, ]), rmat[rownames(rmat)==current_user, ], use = 'pairwise.complete.obs')

    sim <- as.vector(similarities)
    names(sim) <- rownames(similarities)
    res <- sort(sim, decreasing = TRUE)
    similar_users <- names(res[1:5])
    
  }
  
  return (similar_users)
  
}




get_user_ratings <- function(value_list) {
  data<-get_rating_matrix(value_list)
  
  rmat<-data$rmat
  current_user<-data$current_user
  std_zero <- data$SD_zero_rating
  
  similar_users = get_similarities(rmat,current_user,std_zero)
  
  similar_users_ratings <- data.frame(item = rep(colnames(rmat), length(similar_users)), 
                                      rating = c(t(as.data.frame(rmat[similar_users,])))) %>% 
                                      filter(!is.na(rating))
  
  current_user_ratings <- data.frame(item = colnames(rmat), rating = rmat[rownames(rmat)==current_user,]) %>% filter(!is.na(rating))
  # 
  # 
  predictions <- similar_users_ratings %>%
    filter(!(item %in% current_user_ratings$item)) %>%
    group_by(item) %>% summarize(mean_rating = mean(rating))
  
  recom_results= predictions %>% 
    arrange(-mean_rating) %>% top_n(5, mean_rating) %>%
    mutate(MovieID = as.numeric(as.character(item))) %>%
    left_join(movies, by = 'MovieID') %>%
    select("MovieID","Title")
  recom_results = recom_results[1:5,]
  

  recom_results <- data.table(MovieID = recom_results$MovieID,
                              Title = recom_results$Title)
  
}





#--------------------------------------------functions END ------------------------------------------------------

#--------------------------------------------server Start--------------------------------------------------------
server <- function(input, output) {
  
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", {
      # hide the rating container
      #useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      res <- get_recomendation_df(input$genre_select,5)
      
      recom_results <- data.table(MovieID = res$MovieID, 
                                  Title = res$Title, 
                                  ave_ratings = res$ave_ratings)
      
    })# still busy
  }) # clicked on button
  
  
  output$results <- renderUI({ 
    #paste("You have selected", input$genre_select)
    
    
    num_rows <- 1
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                img(src = paste0(small_image_url,recom_result$MovieID[(i - 1) * num_movies + j],'.jpg?raw=true"'),height = '150px', width = '100px') 
                
            ),
            div(style = "text-align:center; color: #999999; font-size: 80%", 
                recom_result$ave_ratings[(i - 1) * num_movies + j]
            ),
            div(style="text-align:center; font-size: 80%", 
                strong(recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
  })
  
  
  
  # rating section starts here
  
  
  
  
  df2 <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", {
      # hide the rating container
      
      #jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      jsCode <- "document.querySelector('btn btn-box-tool').click();"
      #jsCode <- "document.getElementsByClassName('btn').click();"
      runjs(jsCode)
      #runjs("var today = new Date(); alert(today);")
      # get the user's rating data
      
      value_list <- reactiveValuesToList(input)
      #print(value_list)
      recom_results <- get_user_ratings(value_list)
      
      
    }) # still busy
    
  }) # clicked on button    
  
  
  
  output$ratings <- renderUI({
    num_rows <- 34
    num_movies <- 6 # books per row
    lapply(1:num_rows, function(ii) {
      list(fluidRow(lapply(1:num_movies, function(jj) {

        list(box(width = 2,
                 div(style = "text-align:center",
                     img(src = paste0(small_image_url,rating_candidates$MovieID[(ii - 1) * num_movies + jj],'.jpg?raw=true"'),height = '150px', width = '100px')

                 ),
                 div(style = "text-align:center; font-size: 100%; color: #f0ad4e;", ratingInput(paste0("select_", rating_candidates$MovieID[(ii - 1) * num_movies + jj]), label = " ", dataStop = 5))

                 )
             ) #00c0ef
      })))
    })
  })
  
  

  
  
  
  output$results2 <- renderUI({
    
    num_rows <- 1
    num_movies <- 5
    recom_result <- df2()
    
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                img(src = paste0(small_image_url,recom_result$MovieID[(i - 1) * num_movies + j],'.jpg?raw=true"'),height = '150px', width = '100px') 
                
            ),
            div(style = "text-align:center; color: #999999; font-size: 80%", 
                recom_result$ave_ratings[(i - 1) * num_movies + j]
            ),
            div(style="text-align:center; font-size: 80%", 
                strong(recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
    
  })
  
  
  
  
  
  
  
  
  
  
}

#------------------------------------------------server END------------------------------------------------------------------




