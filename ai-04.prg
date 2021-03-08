&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
&& calcula a media por cada uma das 4 variaveis e calcula a variancia &&
&& 4 variaveis para 41 ranges                                         &&
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
Set talk off
close all
exclusao=0.20 && 6%
SELECT 1
USE d:\ai\energia\table0
go bottom
store recno() to x
dimension var(x,5)
y=0
go top
do while .not. eof()
 y=y+1
 store precipitac to varx(y1,1)
 store ttmax      to varx(y1,2)
 store ttmin      to varx(y1,3)
 store sol        to varx(y1,4)
 store ttmed      to varx(y1,5)
 store ttumid     to varx(y1,6)

 skip
enddo
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
&& SELECIONA MAIOR E MENOR DE CADA ATRIBUTO          &&
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
&& definição de limites superior e inferior por categoria nao normalizado da base aberta
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

dimension maiorx(40,4)
dimension menorx(40,4) &&& quarenta sao as categoria e quatro as variaveis
for ss=1 to 40
 for ss1=1 to 6
  maiorx(ss,ss1)=0.00
  menorx(ss,ss1)=1000000.000
 endfor
endfor
? y
wait
&&
dimension totalx(41,4)
dimension valor(41,4)
dimension media(41,4)

for zx1=1 to 40
 for zx2=1 to 6
    &&? zx1,zx2
    totalx(zx1,zx2)=0
    valor(zx1,zx2)=0
    media(zx1,zx2)=0
 endfor
endfor

for z=1 to y
&&? "Maior e menor por tipo de servico geral normalizado "
&&? var1(z,1),"-",var1(z,2),"-",var1(z,3),"-",var1(z,4),"-",var(z,5),"-", 
&&wait

for zxx1=1 to 40

if zxx1<10
 store "Range "+str(zxx1,1,0) to rangexxx
 tamanhox1=7
else
 store "Range "+str(zxx1,2,0) to rangexxx
 tamanhox1=8
endif
tamanhox=len(rtrim(var(z,5)))
&&? var(z,5),rangexxx
if val(substr(var(z,5),7,(tamanhox-6)))=val(substr(rangexxx,7,(tamanhox1-6)))
&&  ? tamanhox, tamanhox1,zxx1,var(z,5),"*",rangexxx,substr(var(z,5),7,(tamanhox-6)),substr(rangexxx,7,(tamanhox1-6))
  && wait
  totalx(zxx1,1)=totalx(zxx1,1)+1
  totalx(zxx1,2)=totalx(zxx1,2)+1
  totalx(zxx1,3)=totalx(zxx1,3)+1
  totalx(zxx1,4)=totalx(zxx1,4)+1
  valor(zxx1,1)=valor(zxx1,1)+var(z,1)
  valor(zxx1,2)=valor(zxx1,2)+var(z,2)
  valor(zxx1,3)=valor(zxx1,3)+var(z,3)
  valor(zxx1,4)=valor(zxx1,4)+var(z,4)

 if var(z,1)>=maiorx(zxx1,1)
  maiorx(zxx1,1)=var(z,1)
 endif
 if var(z,1)<menorx(zxx1,1)
  menorx(zxx1,1)=var(z,1)
 endif
 &&wait
 if var(z,2)>=maiorx(zxx1,2)
  maiorx(zxx1,2)=var(z,2)
 endif
 if var(z,2)<menorx(zxx1,2)
  menorx(zxx1,2)=var(z,2)
 endif
 
 if var(z,3)>=maiorx(zxx1,3)
  maiorx(zxx1,3)=var(z,3)
 endif
 if var(z,3)<menorx(zxx1,3)
  menorx(zxx1,3)=var(z,3)
 endif
 
 if var(z,4)>=maiorx(zxx1,4)
  maiorx(zxx1,4)=var(z,4)
 endif
 if var(z,4)<menorx(zxx1,4)
  menorx(zxx1,4)=var(z,4)
 endif
 && ? maior(1,1)
 && wait
endif
endfor



endfor

for mediaxxx= 1 to 41
Media(mediaxxx,1)=valor(mediaxxx,1)/totalx(mediaxxx,1)
Media(mediaxxx,2)=valor(mediaxxx,2)/totalx(mediaxxx,1)
Media(mediaxxx,3)=valor(mediaxxx,3)/totalx(mediaxxx,1)
Media(mediaxxx,4)=valor(mediaxxx,4)/totalx(mediaxxx,1)
endfor





&&? media
for zx= 1 to 41
  ? "Menor e maior por tipo e por especie"
  ? zx,1,"- Menor - ", menorx(zx,1)," - Maior - ",maiorx(zx,1)
  ? zx,2,"- Menor - ", menorx(zx,2)," - Maior - ",maiorx(zx,2)
  ? zx,3,"- Menor - ", menorx(zx,3)," - Maior - ",maiorx(zx,3)
  ? zx,4,"- Menor - ", menorx(zx,4)," - Maior - ",maiorx(zx,4)

  wait
  for zx1=1 to 4
   ? "Valor : ",valor(zx,zx1)
   ? "Quant : ",totalx(zx,zx1)
   ? "Media : ",Media(zx,zx1)
  endfor
endfor
go top
do while .not. eof()

  for zxxx3= 1 to 41
     if zxxx3<10
    varxx3="Range "+str(zxxx3,1,0)

    else
      varxx3="Range "+str(zxxx3,2,0)

    endif
  

   if species=varxx3
    replace sepal1 with (sepallengt-media(zxxx3,1))/media(1,1)
    replace sepal2 with (sepalwidth-media(zxxx3,2))/media(1,2)
    replace petal1 with (petallengt-media(zxxx3,3))/media(1,3)
    replace petal2 with (petalwidth-media(zxxx3,4))/media(1,4)
   endif
  endfor
    
   skip
enddo

mediageral=abs(petal1)+abs(petal2)+abs(sepal1)+abs(sepal2)
COPY TO d:\ai\energia\tablex0 for abs(petal1)<exclusao .and. abs(petal2)<exclusao .and. abs(sepal2)<exclusao .and. abs(sepal1)<exclusao
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
&&& Selecionamos o subset de amostras que sao consideradas validas como base de analise &&
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

select 2
use d:\ai\energia\tablex0
go bottom
store recno() to yy1
? yy1
wait
dimension varx(yy1,5)
go top
y1=0
go top
do while .not. eof()
 y1=y1+1
 store sepallengt to varx(y1,1)
 store sepalwidth to varx(y1,2)
 store petallengt to varx(y1,3)
 store petalwidth to varx(y1,4)
 store species    to varx(y1,5)

 skip
enddo

dimension maiorx(41,4)
dimension menorx(41,4) &&& tres sao as categoria e quatro as variaveis
for ss=1 to 41
 for ss1=1 to 4
  maiorx(ss,ss1)=0.00
  menorx(ss,ss1)=10000000000.000
 endfor
endfor


for z=1 to y1
 &&? "Maior e menor por tipo de servico geral normalizado "
 &&? var1(z,1),"-",var1(z,2),"-",var1(z,3),"-",var1(z,4),"-",var(z,5),"-", 
 &&wait
 
 for zxx1=1 to 41

if zxx1<10
 store "Range "+str(zxx1,1,0) to rangexxx
 tamanhox1=7
else
 store "Range "+str(zxx1,2,0) to rangexxx
 tamanhox1=8
endif
tamanhox=len(rtrim(var(z,5)))
&&? var(z,5),rangexxx
if val(substr(var(z,5),7,(tamanhox-6)))=val(substr(rangexxx,7,(tamanhox1-6)))
&&  ? tamanhox, tamanhox1,zxx1,var(z,5),"*",rangexxx,substr(var(z,5),7,(tamanhox-6)),substr(rangexxx,7,(tamanhox1-6))
  && wait
  totalx(zxx1,1)=totalx(zxx1,1)+1
  totalx(zxx1,2)=totalx(zxx1,2)+1
  totalx(zxx1,3)=totalx(zxx1,3)+1
  totalx(zxx1,4)=totalx(zxx1,4)+1
  valor(zxx1,1)=valor(zxx1,1)+var(z,1)
  valor(zxx1,2)=valor(zxx1,2)+var(z,2)
  valor(zxx1,3)=valor(zxx1,3)+var(z,3)
  valor(zxx1,4)=valor(zxx1,4)+var(z,4)

 if var(z,1)>=maiorx(zxx1,1)
  maiorx(zxx1,1)=var(z,1)
 endif
 if var(z,1)<menorx(zxx1,1)
  menorx(zxx1,1)=var(z,1)
 endif
 &&wait
 if var(z,2)>=maiorx(zxx1,2)
  maiorx(zxx1,2)=var(z,2)
 endif
 if var(z,2)<menorx(zxx1,2)
  menorx(zxx1,2)=var(z,2)
 endif
 
 if var(z,3)>=maiorx(zxx1,3)
  maiorx(zxx1,3)=var(z,3)
 endif
 if var(z,3)<menorx(zxx1,3)
  menorx(zxx1,3)=var(z,3)
 endif
 
 if var(z,4)>=maiorx(zxx1,4)
  maiorx(zxx1,4)=var(z,4)
 endif
 if var(z,4)<menorx(zxx1,4)
  menorx(zxx1,4)=var(z,4)
 endif
 && ? maior(1,1)
 && wait
endif
endfor



endfor

for mediaxxx= 1 to 41
Media(mediaxxx,1)=valor(mediaxxx,1)/totalx(mediaxxx,1)
Media(mediaxxx,2)=valor(mediaxxx,2)/totalx(mediaxxx,1)
Media(mediaxxx,3)=valor(mediaxxx,3)/totalx(mediaxxx,1)
Media(mediaxxx,4)=valor(mediaxxx,4)/totalx(mediaxxx,1)
endfor


 
 
 
 










&&? media
for zx= 1 to 41
  ? "Menor e maior por tipo e por especie"
  ? zx,1,"- Menor - ", menorx(zx,1)," - Maior - ",maiorx(zx,1)
  ? zx,2,"- Menor - ", menorx(zx,2)," - Maior - ",maiorx(zx,2)
  ? zx,3,"- Menor - ", menorx(zx,3)," - Maior - ",maiorx(zx,3)
  ? zx,4,"- Menor - ", menorx(zx,4)," - Maior - ",maiorx(zx,4)
 && wait

endfor

&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&[
&& Verificando quantos matches tem nos quatro itens
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
select 3
use d:\ai\energia\table1
go bottom
store recno() to xx
dimension tabela(41,4)
dimension vary(xx,5)
y2=0
go top
do while .not. eof()
 y2=y2+1
 store sepallengt to vary(y2,1)
 store sepalwidth to vary(y2,2)
 store petallengt to vary(y2,3)
 store petalwidth to vary(y2,4)
 store species    to vary(y2,5)

 skip
enddo

go top
do while .not. eof()

for ssx1=1 to 41
  if ssx1<=9
    varxx1="var0"+str(ssx1,1,0)+"x01"
     varxx2="var0"+str(ssx1,1,0)+"x02"
      varxx3="var0"+str(ssx1,1,0)+"x03"
       varxx4="var0"+str(ssx1,1,0)+"x04"
  else
    varxx1="var"+str(ssx1,2,0)+"x01"
     varxx2="var"+str(ssx1,2,0)+"x02"
      varxx3="var"+str(ssx1,2,0)+"x03"
       varxx4="var"+str(ssx1,2,0)+"x04"
  
  endif
    &&  ?  varxx1
 

      replace &varxx1 with "NAO"
      replace &varxx2 with "NAO"
      replace &varxx3 with "NAO"
      replace &varxx4 with "NAO"
 
 endfor
 store recno() to ssx3
 store species to tespecie


 
for ssx1 = 1 to 41

for ssx2 = 1 to 4
   
 


   

         
  && ? vary(ssx3,ssx2),menorx(ssx1,ssx2),vary(ssx3,ssx2),maiorx(ssx1,ssx2)
  && wait
  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  && setosa
  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  &&? ssx3,ssx2
  if ssx1<10
    varxx1="var0"+str(ssx1,1,0)+"x01"
     varxx2="var0"+str(ssx1,1,0)+"x02"
      varxx3="var0"+str(ssx1,1,0)+"x03"
       varxx4="var0"+str(ssx1,1,0)+"x04"
  else
    varxx1="var"+str(ssx1,2,0)+"x01"
     varxx2="var"+str(ssx1,2,0)+"x02"
      varxx3="var"+str(ssx1,2,0)+"x03"
       varxx4="var"+str(ssx1,2,0)+"x04"
  
  endif
  
    if  vary(ssx3,ssx2)>=menorx(ssx1,ssx2) .and. vary(ssx3,ssx2)<=maiorx(ssx1,ssx2)
      if ssx2=1
       replace &varxx1 with "SIM"
      endif
      if ssx2=2
       replace &varxx2 with "SIM"
      endif
       if ssx2=3
       replace &varxx3 with "SIM"
      endif
      if ssx2=4
       replace &varxx4 with "SIM"
      endif
    endif
 

endfor

endfor
skip
enddo   
replace all selecao with ""
go top
do while .not. eof()
store recno() to record
for ssx1 = 1 to 41


   
 


   

         
  && ? vary(ssx3,ssx2),menorx(ssx1,ssx2),vary(ssx3,ssx2),maiorx(ssx1,ssx2)
  && wait
  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  && setosa
  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  &&? ssx3,ssx2
  if ssx1<10
    varxx1="var0"+str(ssx1,1,0)+"x01"
     varxx2="var0"+str(ssx1,1,0)+"x02"
      varxx3="var0"+str(ssx1,1,0)+"x03"
       varxx4="var0"+str(ssx1,1,0)+"x04"
  else
    varxx1="var"+str(ssx1,2,0)+"x01"
     varxx2="var"+str(ssx1,2,0)+"x02"
      varxx3="var"+str(ssx1,2,0)+"x03"
       varxx4="var"+str(ssx1,2,0)+"x04"
  
  endif
  
    if  &varxx1="SIM" .and. &varxx2="SIM" .and. &varxx3="SIM" .and. &varxx4="SIM"
      ? varxx1,&varxx1,varxx2,&varxx2,varxx3,&varxx3,varxx4,&varxx4,ssx1, record
      wait
      store trim(selecao) to selecaox
      replace selecao with selecaox+str(ssx1,2,0)+" ou "
    
    endif
 



endfor
 
skip
enddo

