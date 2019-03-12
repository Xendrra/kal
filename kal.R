kal<-function(dat){ 
  logic=FALSE
  dat=gsub("[.]","",dat)
  cisla=c(0:99)
  temp=numeric()
  for (i in 1:nchar(dat)){ 
    temp[i]=cisla[which(cisla==substring(dat,i,i))]
  }
  print(temp)
  q=temp[1]*10+temp[2]
  m=temp[3]*10+temp[4]
  j=(temp[5]*10+temp[6])%%100
  k=temp[7]*10+temp[8]
  rok=temp[5]*1000+temp[6]*100+temp[7]*10+temp[8]
  print(rok)
  if (m==1){
    m=13
    k=k-1}
  else if(m==2){ 
    m=14
    k=k-1}
  h=(q+floor(13*(m+1)/5)+k+floor(k/4)+floor(j/4)-(2*j))%%7
  dny=c('Saturday','Sunday','Monday','Tuesday','Wednesday','Thursday','Friday')
  den=dny[h+1]
  print(den)
  if (rok/4==floor(rok/4)){
    logic=TRUE
  }
  print(logic)
}
