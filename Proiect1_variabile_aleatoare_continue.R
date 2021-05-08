rm(list = ls())

# 1
constantaNormalizare <- function(f) {
  print(
    tryCatch(
      {1/integrate(f, lower = -Inf, upper = Inf)$value},
      error = function(e) {"Nu exista constanta de normalizare pt functia data."}
    )
  )
}

# 2
densitateProbabilitate <- function(f) {
  if(nlm(f, p = 100)$minimum<0)
    return(F)				  # functia poate avea valori negative
  tryCatch({
    if(isTRUE(all.equal(1, integrate(f, lower = -Inf, upper = Inf)$value)))
      return(T)				# integrala este o constanta == 1
    return(F)	 			  # integrala este o constanta /= 1
  },
  error = function(e) {return(F)} 	# integrala nu este o constanta
  )
}

# 3
setClass("va", slots=list(densitate="function", st="numeric", dr="numeric"))

# 4
repartitiaGrafica <- function(f, mini, maxi){
  tryCatch(
    {
      x <- seq(mini, maxi, (maxi-mini)/1000)
      d <- c()
      for (a in x)
        d <- c(d, integrate(f, mini, a)$value)
      plot(x, d, type="l", col="green" )
      par(new=T)
      lines(x, f(x), type="l", col="red")
      legend(x = "topright", lty = c(1, 1), col = c("red", "green"),
             legend = c("Densitate de probabilitate", "Functia de repartitie"))
    },
    error = function(e){"Nu a mers constructia graficului."}
  )
}

# 5
calculelePosibilePeVAC <- function(var){
  tryCatch({		#calculam media
    print("Media este: ")
    print(integrate(function(x) {x*var@densitate(x) },lower = var@st, upper = var@dr))
  }, error = function(e) {"Nu se poate calcula."}
  )
  tryCatch({		#calculam dispersia
    print("Dispersia este: ")
    aux <- function(x){ integrate(var@densitate, lower = var@st, upper = var@dr)$value }
    print(integrate(function(x){ (x-aux(x))*(x-aux(x)) }, lower = var@st, upper = var@dr))
  }, error = function(e) {"Nu se poate calcula."}
  )
  for (i in c(1, 2, 3, 4)) {
    tryCatch({
      print(paste(c("Momemntul de ordin ", i), collapse = ""))
      print(integrate(function(x){ (x^i)*var@densitate(x) },lower = var@st, upper = var@dr))
    }, error = function(e) { print("Nu se poate calcula.") }
    )}
}

# 6
CalculMedieSiDispersie <- function(g,X){
  tryCatch({
    h <-function(x){ g(x)*X@densitate(x) } #calculez media noului obiect
    m <- integrate(h,lower = X@st,upper = X@dr) #functia pentru medie
    print("Media noului obiect este: ")
    print(m$value)
    h1 <- function(x){ integrate(h,lower = X@st, upper = X@dr)$value } # dispersia
    disp <- integrate(function(x){(x - h1(x))*(x-h1(x))}, lower = X@st, upper = X@dr)
    print("Dispersia noului obiect este: ")
    print(disp$value)
  },
  error = function(e) {"Nu merge calculat."}
  )
}

# 7
probabilitate <- function(object) {
  integrate(object@densitate, lower = object@st, upper = object@dr)$value
}

# 8
afisareInformatiiVariabila <- function(var){
  print("Functia densitatii de probabilitate este:")
  print(var@densitate)
  print("Capul stang al integralei este ")
  print(var@st)
  print("iar capul drept este: ")
  print(var@dr)
  calculelePosibilePeVAC(var)
  repartitiaGrafica(var@densitate, var@st,var@dr)
}



# EXEMPLE

# 1
constantaNormalizare(function(x){exp(1)^((-x^2)/2)})
constantaNormalizare(function(x){x^2 + 2*x + 3})

# 2
print(densitateProbabilitate(function(x){exp(1)^((-x^2)/2)}))
print(densitateProbabilitate(function(x){x^2 + 2*x + 3}))
print(densitateProbabilitate(function(x){x^2 + 2*x - 3}))
print(densitateProbabilitate(function(x){exp(1)^(-exp(1)^(-x)-x)}))

# 3
var <- new("va", densitate=function(x){exp(1)^(-exp(1)^(-x)-x)}, st=-10, dr=10)

# 4
repartitiaGrafica(function(x){exp(1)^(-exp(1)^(-x)-x)}, -10, 10)

# 5
calculelePosibilePeVAC(var)

# 6
CalculMedieSiDispersie(function(x){x+1}, var)

# 7
probabilitate(var)

# 8
afisareInformatiiVariabila(var)
