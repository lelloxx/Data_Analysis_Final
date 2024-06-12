df= read.table('/Users/alessandroausteri/Desktop/dab_project/BigMartSales.csv',header=TRUE, sep=",")

library(corrplot)
#first of all we need to fix the feature "Item_fat_content", since it contains different names for the same variable.

item_fat_content_uniques <- unique(df$Item_Fat_Content)#unique values of item_fat_content

print(item_fat_content_uniques)

#now, since all values that start with r are in regular fat and others are with low fat, we will
#change all values that start with r with Regular Fat and others with Low fats.

df$Item_Fat_Content <- ifelse(grepl("^[Rr]", df$Item_Fat_Content), "Regular Fat", "Low Fat")

#check if now there are just "Low Fat" and "Regular Fat"

item_fat_content_uniques <- unique(df$Item_Fat_Content)

print(item_fat_content_uniques)

#_________________________________________________#

#handling with missing values

#column "Outlet_Size"

#conteggio records senza Outlet_Size:
null<-sum(nchar(df$Outlet_Size) ==0)

#numero di records
total<- nrow(df)

#percentuale di righe senza outlet_size
percentuale_stringhe_vuote <- (null / total) * 100

#stampa percentuale di righe senza outlet_size

print(paste("Percentuale di records con stringhe vuote in 'Outlet_Size':", percentuale_stringhe_vuote, "%"))

#visto che la percentuale è molto alta (28%, dobbiamo necessariamente trovare un modo per sistemare questa colonna)
#poichè la variabile è categorica, possiamo provare a stampare una correlation matrix per vedere con quali altre variabili
#è strettamente legata "Outlet_Store" e provare a fare una prediction


#per fare ciò dobbiamo fattorizzare i valori di outlet store, small=1, medium=2, high=3
# Converti 'Outlet_Size' in numerico
df$Outlet_Size_Factor <- as.numeric(factor(df$Outlet_Size))

#fattorizza anche item_fat_content mettendo low fat=0, regular=1

df$Item_Fat_Content_Numeric <- ifelse(df$Item_Fat_Content == "Low Fat", 0, 
                                      ifelse(df$Item_Fat_Content == "Regular Fat", 1, NA))

#fattorizza outlet_type con Grocery Store=0, Supermarket Type1=1, Type2=2, Type3=3
# Fattorizza 'Outlet_Type' con livelli specifici
df$Outlet_Type_Factor <- factor(df$Outlet_Type, 
                                levels = c("Grocery Store", "Supermarket Type1", "Supermarket Type2", "Supermarket Type3"))

# Converti i fattori in valori numerici
df$Outlet_Type_Numeric <- as.numeric(df$Outlet_Type_Factor) - 1


df$Outlet_Size_Factor <- as.numeric(factor(df$Outlet_Size))

#fattorizza anche item_fat_content mettendo low fat=0, regular=1

df$Item_Fat_Content_Numeric <- ifelse(df$Item_Fat_Content == "Low Fat", 0, 
                                      ifelse(df$Item_Fat_Content == "Regular Fat", 1, NA))

#fattorizza outlet_type con Grocery Store=0, Supermarket Type1=1, Type2=2, Type3=3
# Fattorizza 'Outlet_Type' con livelli specifici
df$Outlet_Type_Factor <- factor(df$Outlet_Type, 
                                levels = c("Grocery Store", "Supermarket Type1", "Supermarket Type2", "Supermarket Type3"))

# Converti i fattori in valori numerici
df$Outlet_Type_Numeric <- as.numeric(df$Outlet_Type_Factor) - 1


# Seleziona solo colonne numeriche per la matrice di correlazione
numeric_data <- df[sapply(df, is.numeric)]

# Calcola la matrice di correlazione utilizzando solo dati numerici
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")  # Gestisce i valori mancanti calcolando la correlazione dove possibile

# Visualizza la matrice di correlazione
corrplot(cor_matrix, method = "color", 
         type = "upper",        # Mostra solo la parte superiore della matrice di correlazione
         order = "hclust",      # Ordina le variabili in base ai cluster gerarchici
         tl.col = "black",      # Imposta il colore delle etichette a nero
         tl.srt = 45,           # Ruota le etichette di 45 gradi
         addCoef.col = "black") # Aggiunge i coefficienti di correlazione ai quadrati colorati

# Stampa anche la matrice di correlazione in formato testuale, se necessario
print(cor_matrix)



#possiamo vedere che l'unica relazione interessante di outlet_size è con outlet_type


