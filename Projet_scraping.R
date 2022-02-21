install.packages('XML')
install.packages('stringr')
install.packages('httr')

library(XML)
library(stringr)
library(httr)



setwd("C:/Users/dasou/Desktop/Cours M1 ENSP/U2/Webscrap/Projet Webscraping")


###
# Récupération automatique
###

# Déclarer le tableau pour pouvoir collecter les données
tableau_film <- data.frame(titre=NA,url_film=NA,nb_vue=NA,nb_vote=NA,dure_annonce=NA,date_annonce=NA,realisateur=NA,acteurs=NA,date_sortie=NA,genre=NA,pays=NA)[0,]

# Lister les url à parcourir
list_url <- paste("https://www.bandes-annonces.fr/Toutes-les-bandes-annonces_r1.html?start=",seq(0,1280,by=20),sep="")


# Correction spéciale pour la page 1
list_url[1] <- "https://www.bandes-annonces.fr/Toutes-les-bandes-annonces_r1.html"

j=0
# Boucle sur chaque url
for (url in list_url){
  j=j+1
  # Donner le % d'extraction
  print(paste("url n°",which(url == list_url)," sur ",length(list_url), sep=""))
  
  # Extraire la page web des annonces (pour obtenir les annonces des films)
  page_html <- try(htmlParse(GET(url),encoding = "UTF-8"),silent=TRUE)
  
  if(class(page_html)[1] != "try-error"){
    
    # Lister les articles
    liste_annonce <- getNodeSet(page_html,"//div[@class='content']")
    
    # Boucle sur chaque article
    for (id_annonce in 1:length(liste_annonce)){
      
      # J'ai fais cette condition pour contourner une annonce qui bloque le programme
      if(j==2 & id_annonce==7){
        id_annonce=6
      }
      
      # Extraire la node de l'article
      annonce <- liste_annonce[[id_annonce]]
      
      # Extraire le titre
      titre <- xmlValue(getNodeSet(annonce,"//div[@class='titre']")[[id_annonce]])
      
      # Extraire le lien
      url_film <- xmlAttrs(getNodeSet(annonce,".//div[@class='titre']/a")[[1]])[['href']]
      
      # Extraire le nombre de vote
      vote <- xmlValue(getNodeSet(annonce,".//div[@class='infos']")[[2]])
      # Nettoyer l'info
      nb_vote <- gregexpr("[0-9\\.]+" , vote )
      nb_vote <- sapply(regmatches(vote , nb_vote) , as.numeric)
      
      # Extraire la page web de l'annonce (pour obtenir les détails sur le film)
      page_film <- try(htmlParse(GET(url_film),encoding = "UTF-8"),silent=TRUE)
      
      if(class(page_film)[1] != "try-error"){
        
        # Extraire le nombre de vues
        vue <- xmlValue(getNodeSet(page_film,"//div[@class='texte_video']/strong")[[1]])
        # Nettoyage
        nb_vue <- gregexpr("[0-9\\.]+" , vue )
        nb_vue <- sapply(regmatches(vue , nb_vue) , as.numeric)
        
        # Extraire les information sur l'annonce
        texte <- xmlValue(getNodeSet(page_film,"//div[@class='infos_video']")[[1]])
        
        # Extraction des nombre
        tps <- gregexpr("[0-9\\.]+" , texte )
        tps <- sapply(regmatches(texte , tps) , as.numeric)
        
        # Calcul de la durée de l'annonce en seconde
        dure_annonce <-tps[1,]*60 + tps[2,]
        
        # Création de la date d'annonce
        jr1 <- str_detect(texte, "jour")
        jr2 <- str_detect(texte, "semaine")
        jr3 <- str_detect(texte, "mois")
        jr4 <- str_detect(texte, "an")
        if(jr1 == TRUE){
          date_annonce <- paste(tps[3,], "jours")
        }else if (jr2 == TRUE){
          date_annonce <- paste(tps[3,], "semaines")
        }else if (jr3 == TRUE){
          date_annonce <- paste(tps[3,], "mois")
        }else if (jr4 == TRUE){
          date_annonce <- paste(tps[3,], "ans")
        } else{date_annonce <- paste(tps[3,], "heures")}
        
        # Extraction des information sur le film
        infos <- xmlValue(getNodeSet(page_film,"//div[@class ='texte_video']/font"))
        
        if(length(infos)==6){
          
          # Réalisateur
          realisateur <- str_remove(infos[2],"Réalisation : ")
          
          # Acteurs
          acteurs <- str_remove(infos[3],"Interprétation : ")
          
          # Date de sortie
          date_sortie <- str_remove_all(infos[4],"Sortie France : ")
          
          # Genre du film
          genre <- str_remove_all(infos[5],"Genre : ")
          
          # Pays de réalisation
          pays <- str_remove_all(infos[6],"Nationalité : ")
          
        }else if(length(infos)==5){
          
          # Réalisateur
          realisateur <- str_remove(infos[2],"Réalisation : ")
          
          act <- str_detect(infos[3],"Interprétation")
          if(act == TRUE){
            
            # Acteurs
            acteurs <- str_remove(infos[3],"Interprétation : ")
            
            # Date de sortie
            date_sortie <- str_remove_all(infos[4],"Sortie France : ")
            
            # Genre du film
            genre <- str_remove_all(infos[5],"Genre : ")
            
            # Pays de réalisation
            pays <- NA
            
          } else{
            
            # Acteurs
            acteurs <- NA
            
            # Date de sortie
            date_sortie <- str_remove_all(infos[3],"Sortie France : ")
            
            # Genre du film
            genre <- str_remove_all(infos[4],"Genre : ")
            
            # Pays de réalisation
            pays <- str_remove_all(infos[5],"Nationalité : ")
            
          }
          
        } else if(length(infos)==4){
          
          # Réalisateur
          realisateur <- str_remove(infos[2],"Réalisation : ")
          
          # Acteurs
          acteurs <- str_remove(infos[3],"Interprétation : ")
          
          # Date de sortie
          date_sortie <- str_remove_all(infos[4],"Sortie France : ")
          
          # Genre du film
          genre <- NA
          
          # Pays de réalisation
          pays <- NA
        }
        
        # Insérer une nouvelle ligne dans le tableau
        tableau_film[nrow(tableau_film)+1,] <-  list(titre,url_film,nb_vue,nb_vote,dure_annonce,date_annonce,realisateur,acteurs,date_sortie,genre,pays)
      }
      
      # Attendre, pour éviter tout blocage
      Sys.sleep(2)
    }
  }
}

###
# Sauvegarde des résultats
###

save(tableau_film,file = 'tableau_film')
write.csv2(tableau_film,file = 'tableau_films.csv')

###
# Chargement des bases de données
###

load('tableau_films')
tableau_films <- read.csv2('tableau_films.csv')


summary(tableau_films)

 
###
#Analyse du texte en nuage de mots
###


# Installer
install.packages("tm")  # pour le text mining
install.packages("SnowballC") # pour le text stemming
install.packages("wordcloud") # générateur de word-cloud 
install.packages("RColorBrewer") # Palettes de couleurs
# Charger
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

nuage_mots <- function(texte,titre=""){
  
  # Suppression des accents
  texte <- enc2native(texte)
  texte <- gsub("['`^~\"]", " ", texte)
  texte <- iconv(texte, to="ASCII//TRANSLIT//IGNORE")
  texte <- gsub("['`^~\"]", "", texte)
  
  # Charger les données comme un corpus
  docs <- Corpus(VectorSource(texte))
  
  # Correction du texte
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Convertir le texte en minuscule
  docs <- tm_map(docs, content_transformer(tolower))
  # Supprimer les nombres
  docs <- tm_map(docs, removeNumbers)
  # Supprimer les mots vides anglais
  docs <- tm_map(docs, removeWords, stopwords("french"))
  # Supprimer les ponctuations
  docs <- tm_map(docs, removePunctuation)
  # Supprimer les espaces vides supplémentaires
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  docs <- tm_map(docs, stemDocument)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
          col ="lightblue", main =paste(titre,"\nFr�quence des mots",sep=''),
          ylab = "Fr�quence des mots")
  
  wordcloud(words = d$word, freq = d$freq, min.freq = 2,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  title(main=titre)
}

nuage_mots(tableau_films[,'genre'])
nuage_mots(tableau_films[,'pays'])
