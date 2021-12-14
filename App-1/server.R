

#source('D:/My Data/UIUC/CS598_Practical_Statistical_Learning/Project4/functions/exploratory_data_analysis.R')
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)
#----------------------------------------global Variables ----------------------------------------------------------------

#path = 'D:/My Data/UIUC/CS598_Practical_Statistical_Learning/Project4/'
small_image_url = "https://liangfgithub.github.io/MovieImages/"


ratings = read_csv(paste0('data/','ratings.csv'))
movies = read_csv(paste0('data/','movies.csv'))
users = read_csv(paste0('data/','users.csv'))
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



get_user_ratings <- function(value_list) {

  value_list= value_list[grep("select_",names(value_list))]
  #print(value_list)
  dat <- data.table(MovieID = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    Rating = unlist(as.character(value_list)))
  
  dat <- dat[!is.null(Rating) & !is.na(MovieID)]
  
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat <- dat[Rating > 0]
  print(dat)
  
  UserID = 6051
  
  # dat$UserID = rep(6051,nrow(dat))
  # dat = dat[,c(3,1,2)]
  # print(dat)
  # 
  # all_dat = rbind(dat,train)

  MovieID = train$MovieID

  new_user = data.frame(UserID = 6051, MovieID = train$MovieID )

  new_user$Rating = ifelse(new_user$MovieID == dat$MovieID, dat$Rating, NA)

  #print(new_user)

  i = paste0('u', new_user$UserID)
  j = paste0('m', new_user$MovieID)
  x = new_user$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  user_Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)

  rownames(user_Rmat) = levels(tmp$i)
  colnames(user_Rmat) = levels(tmp$j)
  user_Rmat = new('realRatingMatrix', data = user_Rmat)
  # print(as(user_Rmat, 'matrix')[, 1:10])
  # print(dim(user_Rmat))
  
}





#--------------------------------------------functions END ------------------------------------------------------

#--------------------------------------------server Start--------------------------------------------------------
server <- function(input, output) {
  
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", {
      # hide the rating container
      useShinyjs()
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
    
    #ids = get_recomendation_list(input$genre_select,5)
    #print(ids)
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
  
  output$ratings <- renderUI({
    num_rows <- 34
    num_movies <- 6 # books per row
    lapply(1:num_rows, function(ii) {
      list(fluidRow(lapply(1:num_movies, function(jj) {

        #h4('aaa')
        #print(rating_candidates$Title[(ii - 1) * num_movies + jj])
        
        list(box(width = 2,
                 div(style = "text-align:center",
                     img(src = paste0(small_image_url,rating_candidates$MovieID[(ii - 1) * num_movies + jj],'.jpg?raw=true"'),height = '150px', width = '100px')

                 ),
                 # div(style="text-align:center; font-size: 80%",
                 #     rating_candidates$Title[(ii - 1) * num_movies + jj]),

                 div(style = "text-align:center; font-size: 100%; color: #f0ad4e;", ratingInput(paste0("select_", rating_candidates$MovieID[(ii - 1) * num_movies + jj]), label = " ", dataStop = 5))

                 )
             ) #00c0ef
      })))
    })
  })
  
  
  
  
  df2 <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", {
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      print(dim(user_ratings))
      Rmat = get_rmat(train)
      

      #rbind(user_ratings,Rmat)
      # 
      # print(dim(Rmat))
      # 
      # print(as(Rmat, 'matrix')[1, 1:20])
      # 
      # first = as(Rmat, 'matrix')[nrow(Rmat), 1:20]
      # 
      # print(sum(which(first!='NA')))

      
      rec_UBCF = Recommender(Rmat, method = 'UBCF',
                             parameter = list(normalize = 'Z-score',
                                              method = 'Cosine',
                                              nn = 25))
      # print(rec_UBCF@model)
      recom = predict(rec_UBCF,
                      user_ratings, type = 'ratings')
      #print(as(recom, 'matrix')[, 1:20])
      
    }) # still busy
    
  }) # clicked on button    
  
  
  
  output$results2 <- renderUI({
    
    recom_result <- df2()
    
    
  })
  
  
  
  
  
  
  
  
  
  
}

#------------------------------------------------server END------------------------------------------------------------------




