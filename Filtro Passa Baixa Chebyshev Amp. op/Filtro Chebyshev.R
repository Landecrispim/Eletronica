f1 = 5000000
f2 = 10000000
R1 = 1000
R2 = 1000
#f1 = 10000
#f2 = 25000

w1 = 2*pi*f1
w2 = 2*pi*f2
w0 = w1
print("w1")
print(w1)
print("w2")
print(w2)


a1 = 1

a2 = 30



E = sqrt(((10^(a1/20))^2)-1)

print("E")
print(E)

g2 = 10^(-a2/20)

print("g2")
print(g2)


n= acosh(sqrt(((1/g2^2)-1)/E^2))*1/acosh(f2/f1)

print(n)
n=ceiling(n)


print("n")
print(n)

pk = matrix(nrow = n,ncol = 2)
pkc = matrix(nrow = n,ncol = 1)

for (k in c(1:n)) {


  pk[k,1] = -w0*sin(((2*k-1)*pi)/(2*n))*sinh((1/n)*asinh(1/E))
  pk[k,2] = w0*cos(((2*k-1)*pi)/(2*n))*cosh((1/n)*acosh(1/E))
  pkc[k] = complex( real = pk[k,1],imaginary = pk[k,2])
}

print(pkc)


W <- matrix(nrow = ceiling(n/2),ncol = 1)
Q <- matrix(nrow = ceiling(n/2),ncol = 1)
C <-matrix(nrow = ceiling(n/2),ncol = 2)
Q1 = sqrt(-pkc[1]*-pkc[4])/((-pkc[1])+(-pkc[4]))
auxr = 1/(R1+R2)
if((n%%2)==0){

  print("=======================================")
  for (i in c(1:(n/2))) {
    Q[i] =  sqrt(-pkc[i]*-pkc[length(pkc)-i+1])/((-pkc[i])+(-pkc[length(pkc)-i+1]))
    W[i] = sqrt(-pkc[i]*-pkc[length(pkc)-i+1])
    c1=(Q[i]*(R1+R2))/(R1*R2*W[i])
    c2 = 1/(W[i]*Q[i]*(R2+R1))
    C[i,1] = c1;
    C[i,2] = c2;

  }

}



print("Q")
print(Q)
print("W")
print(W)
print("C")
print(C)










