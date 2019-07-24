# Pedidos
App para hacer pedido de donas

Para ensayar la aplicación con su cuenta de google correr:

library(googlesheets)
auth<-gs_auth() # aqui debe logearse con una cuenta de google
saveRDS(auth, "auth.rds")

posteriormente debe tener un archivo de googlesheets llamado "Pedido" con esta estructura de columnas
Hora	Investigador	Op1	Cant1	Op2	Op3
