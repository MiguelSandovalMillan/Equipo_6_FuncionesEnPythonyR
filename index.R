# Funciones para calcular interés simple


# Primero creamos la funcion llamada "interesSimple" 
# Y con ella hay 4 argumentos para su calculación , cada uno de estos argumentos tiene un valor por defecto de null se entiende que cuando alguno de los argumentos no tiene valor es el que se debe calcular . 
interesSimple = function(va = NULL, r = NULL, n = NULL, VF = NULL) { 
 
  
  # Se llama la primera condicional en este caso con la pregunta VF es null y si se ejecuta lo que está en este bloque , como no se dio VF se usa la formula VA multiplicado por 1 más tasa de interés del periodo multiplicado por el número de periodos y te regresa el valor de VF .
  if(is.null(VF)){
    VF = va*(1 + r*n)
    return(VF)
  }
  

  # Se llama la segunda condicional en este caso con la pregunta VA es null y si se ejecuta lo que está en este bloque , como no se dio VA se usa la formula VF dividiendo por 1 más tasa de interés del periodo multiplicado por el número de periodos y te regresa el valor de VA .
  else if(is.null(va)){
    va = VF/(1 + r*n)
    return(va)
  }
  

  # Se llama la tercera condicional en este caso con la pregunta R es null y si se ejecuta lo que está en este bloque , como no se dio R se usa la formula VF dividiendo VA menos 1 y todo esto dividido por el número de periodos y te regresa el valor de R .
  else if(is.null(r)){
    r = ((VF/va )- 1)/n
    return(r)
  }
  

  # Se llama la cuarta condicional en este caso con la pregunta N es null y si se ejecuta lo que está en este bloque , como no se dio R se usa la formula VF dividiendo VA menos 1 y todo esto dividido por la tasa de interés del periodo y te regresa el valor de R.
  else if(is.null(n)){
    n = ((VF/va) - 1)/r
    return(n)
  }
  
