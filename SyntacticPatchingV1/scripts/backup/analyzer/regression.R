library(graphics)
library(stats)

#simple linear
#r-square range from 0 to 1, the value is more closer to 1, the better fit the linear relations
#here I give a value of 0.8

lmSet<-function(acol_multi,bcol_limit){
  bar<-0.8
  found<-0
  for(i in 1:bcol_limit){


    if(acol_multi == 1){

      l<-lm(B[[i]]~A$a1)
      print("1,1-----------")
      rsquare = summary(l)$r.square

      if(rsquare >= bar){
        print(rsquare)
        print(summary(l)$coefficients)
        found <-1
      }
      else
        print("\n")
    }

    if(acol_multi == 2){


      l1<-lm(B[[i]]~A$a1)
      print("2,1-----------")

      rsquare = summary(l1)$r.square
      if(rsquare >= bar){
        print(summary(l1)$r.square)
        print(summary(l1)$coefficients)
        found <-1
      }
      else
        print("\n")

      l2<-lm(B[[i]]~A$a2)
      rsquare = summary(l2)$r.square
      if(rsquare >= bar){
        print(summary(l2)$r.square)
        print(summary(l2)$coefficients)
        found <-1
      }
      else
        print("\n")

      print("2,2-----------")
      l<-lm(B[[i]]~A$a1+A$a2)
      rsquare = summary(l)$r.square
      if(rsquare >= bar){
        print(summary(l)$r.square)
        print(summary(l)$coefficients)
        found <-1
      }
      else
        print("\n")


    }

    if(acol_multi == 3){

      print("3,1-----------")
      #print(i)

      l1<-lm(B[[i]]~A$a1)

      rsquare = summary(l1)$r.square
      if(rsquare >= bar){
        print(summary(l1)$r.square)
        print(summary(l1)$coefficients)
        found <-1
      }
      else
        print("\n")

      l2<-lm(B[[i]]~A$a2)
      rsquare = summary(l2)$r.square
      if(rsquare >= bar){
        print(summary(l2)$r.square)
        print(summary(l2)$coefficients)
        found <-1
      }
      else
        print("\n")

      l3<-lm(B[[i]]~A$a3)
      rsquare = summary(l3)$r.square
      if(rsquare >= bar){
        print(summary(l3)$r.square)
        print(summary(l3)$coefficients)
        found <-1
      }
      else
        print("\n")


      print("3,2-----------")
      l12<-lm(B[[i]]~A$a1+A$a2)
      rsquare = summary(l12)$r.square
      if(rsquare >= bar){
        print(summary(l12)$r.square)
        print(summary(l12)$coefficients)
        found <-1
      }
      else
        print("\n")

      l13<-lm(B[[i]]~A$a1+A$a3)
      rsquare = summary(l13)$r.square
      if(rsquare >= bar){
        print(summary(l13)$r.square)
        print(summary(l13)$coefficients)
        found <-1
      }
      else
        print("\n")

      l23<-lm(B[[i]]~A$a2+A$a3)
      rsquare = summary(l23)$r.square
      if(rsquare >= bar){
        print(summary(l23)$r.square)
        print(summary(l23)$coefficients)
        found <-1
      }
      else
        print("\n")

      print("3,3-----------")
      l<-lm(B[[i]]~A$a1+A$a2+A$a3)
      rsquare = summary(l)$r.square
      if(rsquare >= bar){
        print(summary(l)$r.square)
        print(summary(l)$coefficients)
        found <-1
      }
      else
        print("\n")


    }

    if(acol_multi == 4){

      print("4,1-----------")
      l1<-lm(B[[i]]~A$a1)
      rsquare = summary(l1)$r.square
      if(rsquare >= bar){
        print(summary(l1)$r.square)
        print(summary(l1)$coefficients)
        found <-1
      }
      else
        print("\n")

      l2<-lm(B[[i]]~A$a2)
      rsquare = summary(l2)$r.square
      if(rsquare >= bar){
        print(summary(l2)$r.square)
        print(summary(l2)$coefficients)
        found <-1
      }
      else
        print("\n")

      l3<-lm(B[[i]]~A$a3)
      rsquare = summary(l3)$r.square
      if(rsquare >= bar){
        print(summary(l3)$r.square)
        print(summary(l3)$coefficients)
        found <-1
      }
      else
        print("\n")

      l4<-lm(B[[i]]~A$a4)
      rsquare = summary(l4)$r.square
      if(rsquare >= bar){
        print(summary(l4)$r.square)
        print(summary(l4)$coefficients)
        found <-1
      }
      else
        print("\n")

      print("4,2-----------")
      l12<-lm(B[[i]]~A$a1+A$a2)
      rsquare = summary(l2)$r.square
      if(rsquare >= bar){
        print(summary(l12)$r.square)
        print(summary(l12)$coefficients)
        found <-1
      }
      else
        print("\n")

      l13<-lm(B[[i]]~A$a1+A$a3)
      rsquare = summary(l13)$r.square
      if(rsquare >= bar){
        print(summary(l13)$r.square)
        print(summary(l13)$coefficients)
        found <-1
      }
      else
        print("\n")

      l14<-lm(B[[i]]~A$a1+A$a4)
      rsquare = summary(l14)$r.square
      if(rsquare >= bar){
        print(summary(l14)$r.square)
        print(summary(l14)$coefficients)
        found <-1
      }
      else
        print("\n")

      l23<-lm(B[[i]]~A$a2+A$a3)
      rsquare = summary(l23)$r.square
      if(rsquare >= bar){
        print(summary(l23)$r.square)
        print(summary(l23)$coefficients)
        found <-1
      }
      else
        print("\n")

      l24<-lm(B[[i]]~A$a2+A$a4)
      rsquare = summary(l24)$r.square
      if(rsquare >= bar){
        print(summary(l24)$r.square)
        print(summary(l24)$coefficients)
        found <-1
      }
      else
        print("\n")

      l34<-lm(B[[i]]~A$a3+A$a4)
      rsquare = summary(l34)$r.square
      if(rsquare >= bar){
        print(summary(l34)$r.square)
        print(summary(l34)$coefficients)
        found <-1
      }
      else
        print("\n")

      print("4,3-----------")
      l123<-lm(B[[i]]~A$a1+A$a2+A$a3)
      rsquare = summary(l123)$r.square
      if(rsquare >= bar){
        print(summary(l123)$r.square)
        print(summary(l123)$coefficients)
        found <-1
      }
      else
        print("\n")

      l124<-lm(B[[i]]~A$a1+A$a2+A$a4)
      rsquare = summary(l124)$r.square
      if(rsquare >= bar){
        print(summary(l124)$r.square)
        print(summary(l124)$coefficients)
        found <-1
      }
      else
        print("\n")

      l134<-lm(B[[i]]~A$a1+A$a3+A$a4)
      rsquare = summary(l134)$r.square
      if(rsquare >= bar){
        print(summary(l134)$r.square)
        print(summary(l134)$coefficients)
        found <-1
      }
      else
        print("\n")

      l234<-lm(B[[i]]~A$a2+A$a3+A$a4)
      rsquare = summary(l234)$r.square
      if(rsquare >= bar){
        print(summary(l234)$r.square)
        print(summary(l234)$coefficients)
        found <-1
      }
      else
        print("\n")

      print("4,4-----------")
      l<-lm(B[[i]]~A$a1+A$a2+A$a3+A$a4)
      rsquare = summary(l)$r.square
      if(rsquare >= bar){
        print(summary(l)$r.square)
        print(summary(l)$coefficients)
        found <-1
      }
      else
        print("\n")

    }

    print("*******************")
  }

  return(found)

}

#interaction linear
lmPolySet<-function(acol_multi,bcol_limit){

  bar<-0.8
  found<-0

  for(i in 1:bcol_limit){

    if(acol_multi == 1){
      print("a,a^2-----------")
      l1<-lm(B[[i]]~A$a1+A$a2)
      rsquare = summary(l1)$r.square
      if(rsquare >= bar){
        print(summary(l1)$r.square)
        print(summary(l1)$coefficients)
        found <-1
      }

    }

    if(acol_multi == 2){

      print("a,a^2-----------")
      l1<-lm(B[[i]]~A$a1+A$a3)

      rsquare = summary(l1)$r.square
      if(rsquare >= bar){
        print(summary(l1)$r.square)
        print(summary(l1)$coefficients)
        found <-1
      }

      print("a,b^2-----------")

      l2<-lm(B[[i]]~A$a1+A$a4)
      rsquare = summary(l2)$r.square
      if(rsquare >= bar){
        print(summary(l2)$r.square)
        print(summary(l2)$coefficients)
        found <-1
      }

      print("a,a*b-----------")
      l<-lm(B[[i]]~A$a1+A$a5)
      rsquare = summary(l)$r.square
      if(rsquare >= bar){
        print(summary(l)$r.square)
        print(summary(l)$coefficients)
        found <-1
      }

      print("b,a^2-----------")
      l11<-lm(B[[i]]~A$a2+A$a3)

      rsquare = summary(l11)$r.square
      if(rsquare >= bar){
        print(summary(l11)$r.square)
        print(summary(l11)$coefficients)
        found <-1
      }

      print("b,b^2-----------")

      l21<-lm(B[[i]]~A$a2+A$a4)
      rsquare = summary(l21)$r.square
      if(rsquare >= bar){
        print(summary(l21)$r.square)
        print(summary(l21)$coefficients)
        found <-1
      }

      print("b,a*b-----------")
      l31<-lm(B[[i]]~A$a1+A$a5)
      rsquare = summary(l31)$r.square
      if(rsquare >= bar){
        print(summary(l31)$r.square)
        print(summary(l31)$coefficients)
        found <-1
      }

      print("a^2,b^2-----------")
      l12<-lm(B[[i]]~A$a2+A$a3)

      rsquare = summary(l12)$r.square
      if(rsquare >= bar){
        print(summary(l12)$r.square)
        print(summary(l12)$coefficients)
        found <-1
      }

      print("a^2,a*b-----------")

      l22<-lm(B[[i]]~A$a2+A$a4)
      rsquare = summary(l22)$r.square
      if(rsquare >= bar){
        print(summary(l22)$r.square)
        print(summary(l22)$coefficients)
        found <-1
      }

      print("b^2,a*b-----------")
      l32<-lm(B[[i]]~A$a1+A$a5)
      rsquare = summary(l32)$r.square
      if(rsquare >= bar){
        print(summary(l32)$r.square)
        print(summary(l32)$coefficients)
        found <-1
      }

      print("a, b, a^2-----------")
      l13<-lm(B[[i]]~A$a1+A$a2+A$a3)

      rsquare = summary(l13)$r.square
      if(rsquare >= bar){
        print(summary(l13)$r.square)
        print(summary(l13)$coefficients)
        found <-1
      }

      print("a,b,b^2-----------")

      l14<-lm(B[[i]]~A$a1+A$a2+A$a4)
      rsquare = summary(l14)$r.square
      if(rsquare >= bar){
        print(summary(l14)$r.square)
        print(summary(l14)$coefficients)
        found <-1
      }

      print("a, b, a*b-----------")
      l15<-lm(B[[i]]~A$a1+A$a2+A$a5)
      rsquare = summary(l15)$r.square
      if(rsquare >= bar){
        print(summary(l15)$r.square)
        print(summary(l15)$coefficients)
        found <-1
      }


      print("b,a^2, b^2-----------")

      l23<-lm(B[[i]]~A$a3+A$a2+A$a4)
      rsquare = summary(l23)$r.square
      if(rsquare >= bar){
        print(summary(l23)$r.square)
        print(summary(l23)$coefficients)
        found <-1
      }

      print("b, a^2, a*b-----------")
      l24<-lm(B[[i]]~A$a3+A$a2+A$a5)
      rsquare = summary(l24)$r.square
      if(rsquare >= bar){
        print(summary(l24)$r.square)
        print(summary(l24)$coefficients)
        found <-1
      }

      print("b, b^2, a*b-----------")
      l25<-lm(B[[i]]~A$a4+A$a2+A$a5)
      rsquare = summary(l25)$r.square
      if(rsquare >= bar){
        print(summary(l25)$r.square)
        print(summary(l25)$coefficients)
        found <-1
      }

      print("b^2, a^2, a*b-----------")
      l26<-lm(B[[i]]~A$a3+A$a4+A$a5)
      rsquare = summary(l26)$r.square
      if(rsquare >= bar){
        print(summary(l26)$r.square)
        print(summary(l26)$coefficients)
        found <-1
      }


      print("a, b, b^2, a^2, a*b-----------")
      lt<-lm(B[[i]]~A$a1+A$a2+A$a3+A$a4+A$a5)
      rsquare = summary(lt)$r.square
      if(rsquare >= bar){
        print(summary(lt)$r.square)
        print(summary(lt)$coefficients)
        found <-1
      }

      print("a, b, b^2, a^2, a*b-----------")
      lt<-lm(B[[i]]~A$a1+A$a2+A$a3+A$a4+A$a5)
      rsquare = summary(lt)$r.square
      if(rsquare >= bar){
        print(summary(lt)$r.square)
        print(summary(lt)$coefficients)
        found <-1
      }

      print("a, b^2, a^2, a*b-----------")
      lt1<-lm(B[[i]]~A$a1+A$a3+A$a4+A$a5)
      rsquare = summary(lt1)$r.square
      if(rsquare >= bar){
        print(summary(lt1)$r.square)
        print(summary(lt1)$coefficients)
        found <-1
      }

      print("a, b, b^2, a*b-----------")
      lt2<-lm(B[[i]]~A$a1+A$a2+A$a4+A$a5)
      rsquare = summary(lt2)$r.square
      if(rsquare >= bar){
        print(summary(lt2)$r.square)
        print(summary(lt2)$coefficients)
        found <-1
      }

      print("a, b, a^2, a*b-----------")
      lt3<-lm(B[[i]]~A$a1+A$a2+A$a3+A$a5)
      rsquare = summary(lt3)$r.square
      if(rsquare >= bar){
        print(summary(lt3)$r.square)
        print(summary(lt3)$coefficients)
        found <-1
      }

      print("a, b, b^2, a^2-----------")
      lt4<-lm(B[[i]]~A$a1+A$a2+A$a3+A$a4)
      rsquare = summary(lt4)$r.square
      if(rsquare >= bar){
        print(summary(lt4)$r.square)
        print(summary(lt4)$coefficients)
        found <-1
      }

      print("a*b, b, b^2, a^2-----------")
      lt5<-lm(B[[i]]~A$a2+A$a3+A$a4)
      rsquare = summary(lt5)$r.square
      if(rsquare >= bar){
        print(summary(lt5)$r.square)
        print(summary(lt5)$coefficients)
        found <-1
      }

    }

    if(acol_multi == 3){

      print("a,b,c,9-----------")
      l<-lm(B[[i]]~A$a1+A$a2+A$a3+A$a4+A$a5+A$a6+A$a7+A$a8+A$a9)
      rsquare = summary(l)$r.square
      if(rsquare >= bar){
        print(summary(l)$r.square)
        print(summary(l)$coefficients)
        found <-1
      }


    }

    if(acol_multi == 4){
      print("a,b,c,d,14-----------")
      l<-lm(B[[i]]~A$a1+A$a2+A$a3+A$a4+A$a5+A$a6+A$a7+A$a8+A$a9+A$a10+A$a11+A$a12+A$a13+A$a14)
      rsquare = summary(l)$r.square
      if(rsquare >= bar){
        print(summary(l)$r.square)
        print(summary(l)$coefficients)
        found <-1
      }

    }

    print("*******************")
  }

  return(found)

}

args<-commandArgs(TRUE)
acol_total<-count.fields(args[[2]], sep = ",")
bcol_total<-count.fields(args[[3]], sep = ",")

A<-read.csv(args[[2]], fill = TRUE, header = FALSE,
              col.names= paste("a", seq_len(acol_total),sep=""), skip=1)
B<-read.csv(args[[3]], fill = TRUE, header = FALSE,
            col.names= paste("b", seq_len(bcol_total),sep=""), skip=1)

#C<-read.table("c:/wei/git/R/parameter2", sep = ",")

acol<-acol_total
bcol<-bcol_total

#print(B[1])
#print(B[2])

#support 4 parameters multiple linear
#stepwise linear model (simple, multiple

#print(acol)
#print(bcol)

found<-lmSet(acol_total,bcol_total)

#Support 3 paramter poly
if(found == 0 && acol <= 4){
  print("poly log starts ...")
  lmPolySet(acol,bcol)
}









                #B <- read.table("c:/wei/git/R/outputfinal.txt",sep=",",col.names= c("b1","b2","b3","b4","b5"))
#C <- read.table("c:/wei/git/R/parameter.txt",sep=",",col.names=c("c1","c2","c3"))

#x<-C$c1
#y<-C$c2
#z<-C$c3

#if(x==1 && y == 0)  T<-data.frame(first=A$a1)
#if(x==2 && y == 0 )  T<-data.frame(first=A$a1,second=A$a2)
#if(x==3 && y== 0 )  T<-data.frame(first=A$a1,second=A$a2,third=A$a3)
#if(x==4 && y == 0 )  T<-data.frame(first=A$a1,second=A$a2,third=A$a3,fourth=A$a4)

#if(x==1)  T<-data.frame(first=D$d1)
#if(x==2)  T<-data.frame(first=D$d1,second=D$d2)
#if(x==3)  T<-data.frame(first=D$d1,second=D$d2,third=D$d3)
#if(x==4)  T<-data.frame(first=D$d1,second=D$d2,third=D$d3,fourth=D$d4)


#if(z==1)  P<-data.frame(first=B$b1)

#if(z==2)  P<-data.frame(first=B$b1,second=B$b2)
#if(z==3)  P<-data.frame(first=B$b1,second=B$b2,third=B$b3)
#if(z==4)  P<-data.frame(first=B$b1,second=B$b2,third=B$b3,fourth=B$b4)
#if(z==5)  P<-data.frame(first=B$b1,second=B$b2,third=B$b3,fourth=B$b4,fifth=B$b5)


#library(graphics)
#library(stats)

#print(x)
#print(y)
#print(z)

#if(x>0 && z>0 && nrow(C)>0){
#	for(i in P){
#                if(x == 2)
#   			l<-lm(i~T$first*T$second)
#		if(x == 3)
#   			l<-lm(i~T$first*T$second*T$third)
#		if(x == 4)
#			l<-lm(i~T$first*T$second*T$third*T$four)

#		s<-step(l)
 #   		print(s$coefficients)

#	}
#}
