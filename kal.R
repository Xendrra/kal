Calendar<-function(dat){ 
  #funkce Calendar vrati den v tydnu a informaci o prestupnem roku pro zadane datum
  #vstup...retezec s datem ve formatu 'DD.MM.RRRR' ('DD.MM.YYYY')
  #vystup...list, 1. element den v tydnu, 2. element logicka hodnota: True=prestupny, False=neprestupny rok
  dat=gsub("[.]","",dat)                      #odstrani tecky
  cisla=c(0:99)                               #pomocny vektor cisel 
  temp=numeric()                              #iniciace promennych                              
  prestup=FALSE 
  for (i in 1:nchar(dat)){ 
    temp[i]=cisla[which(cisla==substring(dat,i,i))]  #prevod hodnot z characteru na vector
  }                                                 
  q=temp[1]*10+temp[2]                               #prevod z vektoru na jedno cislo, ulozeni do promennych pro zelleruv algoritmus
  m=temp[3]*10+temp[4]
  j=(temp[5]*10+temp[6])%%100
  k=temp[7]*10+temp[8]
  rok=temp[5]*1000+temp[6]*100+temp[7]*10+temp[8]
  if (m==1){                                         #vyjmka pro leden a unor dle zellerova algoritmu
    m=13
    k=k-1}
  else if(m==2){ 
    m=14
    k=k-1}
  h=(q+floor(13*(m+1)/5)+k+floor(k/4)+floor(j/4)-(2*j))%%7    #zelleruv algoritmus
  dny=c('Saturday','Sunday','Monday','Tuesday','Wednesday','Thursday','Friday') 
  den=dny[h+1]                                                 #prirazeni daneho dne z vektoru dny dle vysledku zellera
  if (rok/4==floor(rok/4)){                                    #pokud je rok delitelny 4, je prestupny
    prestup=TRUE
  }
  vysledek=list(den,prestup)
  return(vysledek)
}
