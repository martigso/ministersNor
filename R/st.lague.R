#' Cabinet portfolio proportionality
#' 
#' A function used to calculate the proportionality of portfolio allocation using the Saint Lague method with first divider being 1.
#' 
#' @param seats Vector containing the seats each party has in cabinet
#' @param portfolios Vector containing the total amount of portfolios in the cabinet
#' @param party Vector identifying the party (either id or party name)
#' 
#' @return Returns a vector of perfect proportionality portfolio allocation estimates to be compared with acctual portfolio allocation
#' 
#' 
#' 
#' @examples 
#' norPortf <- ministersNor::norPortf
#' 
#' nor_stlague<-norPortf %>%
#'  group_by(cabinet_name) %>%
#'  mutate(stlague_posts = st.lague(seats, total_cab_posts[1], party_name))
#'  
#' @export
#' 
#' @import dplyr 
#' @import uacd

st.lague<-function(seats, n.posts, n.parties){
  
  posts_now = rep(0, length(n.parties)) # start with 0 placed posts
  
  
  seats_now = seats # seats in parliament
  
  
  while(n.posts > sum(posts_now)){
    seats_now = seats # it is important to not write over the seats vector, as it is reset after each loop
    
    seats_now = ifelse(posts_now==1, seats_now/3, seats_now)
    seats_now = ifelse(posts_now==2, seats_now/5, seats_now)
    seats_now = ifelse(posts_now==3, seats_now/7, seats_now)
    seats_now = ifelse(posts_now==4, seats_now/9, seats_now)
    seats_now = ifelse(posts_now==5, seats_now/11, seats_now)
    seats_now = ifelse(posts_now==6, seats_now/13, seats_now)
    seats_now = ifelse(posts_now==7, seats_now/15, seats_now)
    seats_now = ifelse(posts_now==8, seats_now/17, seats_now)
    seats_now = ifelse(posts_now==9, seats_now/19, seats_now)
    seats_now = ifelse(posts_now==10, seats_now/21, seats_now)
    seats_now = ifelse(posts_now==11, seats_now/23, seats_now)
    seats_now = ifelse(posts_now==12, seats_now/25, seats_now)
    seats_now = ifelse(posts_now==13, seats_now/27, seats_now)
    seats_now = ifelse(posts_now==14, seats_now/29, seats_now)
    seats_now = ifelse(posts_now==15, seats_now/31, seats_now)
    seats_now = ifelse(posts_now==16, seats_now/33, seats_now)
    seats_now = ifelse(posts_now==17, seats_now/35, seats_now)
    seats_now = ifelse(posts_now==18, seats_now/37, seats_now)
    seats_now = ifelse(posts_now==19, seats_now/39, seats_now)
    seats_now = ifelse(posts_now==20, seats_now/41, seats_now)
    seats_now = ifelse(posts_now==21, seats_now/43, seats_now)
    seats_now = ifelse(posts_now==22, seats_now/45, seats_now)
    seats_now = ifelse(posts_now==23, seats_now/47, seats_now)
    seats_now = ifelse(posts_now==24, seats_now/49, seats_now)
    seats_now = ifelse(posts_now==25, seats_now/51, seats_now)
    seats_now = ifelse(posts_now==26, seats_now/53, seats_now)
    seats_now = ifelse(posts_now==27, seats_now/55, seats_now)
    seats_now = ifelse(posts_now==28, seats_now/57, seats_now)
    seats_now = ifelse(posts_now==29, seats_now/59, seats_now)
    seats_now = ifelse(posts_now==30, seats_now/61, seats_now)
    seats_now = ifelse(posts_now==31, seats_now/63, seats_now)
    seats_now = ifelse(posts_now==32, seats_now/65, seats_now)
    seats_now = ifelse(posts_now==33, seats_now/67, seats_now)
    seats_now = ifelse(posts_now==34, seats_now/69, seats_now)
    seats_now = ifelse(posts_now==35, seats_now/71, seats_now)
    seats_now = ifelse(posts_now==36, seats_now/73, seats_now)
    seats_now = ifelse(posts_now==37, seats_now/75, seats_now)
    seats_now = ifelse(posts_now==38, seats_now/77, seats_now)
    seats_now = ifelse(posts_now==39, seats_now/79, seats_now)
    seats_now = ifelse(posts_now==40, seats_now/81, seats_now)
    
    posts_now = ifelse(seats_now==max(seats_now), posts_now+1, posts_now)
  }
  
  posts_now
}