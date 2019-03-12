kal<-function(dat){ 
  dat=gsub("[.]","",dat)
  cisla=c(0:99)
  temp=numeric()
  for (i in 1:nchar(dat)){ 
    temp[i]=cisla[which(cisla==substring(dat,i,i))]
  }
  print(temp)
  q=temp[1]*10+temp[2]
  m=temp[3]*10+temp[4]
  if (m==1){
    m=13}
  else if(m==2){ 
    m=14
  }
  j=(temp[5]*10+temp[6])%%100
  k=temp[7]*10+temp[8]
  h=(q+floor(13*(m+1)/5)+k+floor(k/4)+floor(j/4)-(2*j))%%7
  print(c(q,m,k,j))
  print(h)
}
