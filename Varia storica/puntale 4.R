rm(list = ls())
v.100 <- 9.000    ##diametro barra cilindrica, FF')
v.101 <- 8.000    ##diametro collare, EE')
v.102 <- 8.500    ##diametro filettatura, LL')
v.103 <- 6.950    ##diametro posteriore puntale, CC')
v.104 <- 3.550    ##diametro punta puntale, BB')
v.105 <- 14.480   ##lunghezza parte conica puntale, OO')
v.106 <- 1.000    ##lunghezza collare, EH)
v.107 <- 12.000   ##lunghezza coda filettata, LM)

############### PARAMETRI MACCHINA UTENSILE ####################################)
v.200 <- 50      ##Velocita' di spostamento F)
v.201 <- 360     ##gradi di un giro completo o parziale)
v.202 <- 90       ##gradi di spostamento per ogni ciclo)
v.203 <- -0.100  ##incremento+ decremento- asse z ad ogni ciclo)
v.204 <- 1.000  #COSE DA FARE: #204 = diametro del fresino. Suo diametro ?)
##G28 X~ Y~ Z~ A~ B~ C~ 0vvero variabili riservate da 5161 a 5166)

############### MISURE DERIVATE ################################################)
v.1104 <- v.103/2-v.104/2           ##raggio puntale, BA, positivo)
v.1105 <- v.1104/v.105              ##(rapporto cateti, piccolo/grande, BA/AC

############### RIPETIZIONI NECESSARIE #########################################)
##COSE DA FARE: arrotondare il n di passi a numero  intero)
v.1204 <- v.201/v.202               ##n passi per completare rotazione, asse A)
v.1205 <- v.1104/abs(v.203)         ##n passi per percorrere AB, asse Z)
v.1206 <- (v.100-v.102)/2/abs(v.203) ##n passi per percorrere HL, asse Z)
############### PARAMETRI DINAMICI MACCHINA UTENSILE ###########################)
#v.1 <- 0.000          ##POSIZIONE y)
v.1 <- v.105/v.1205  ##POSIZIONE z   =   FF'/2 +- decremento asse z)
v.2 <- (v.100/2-v.203)
############### PROGRAMMA VERO E PROPRIO  ######################################)
##COSE DA FARE: chiudere la fresatura con un passaggio finale per portare a     )
##misura il pezzo. Potrebbe succedere che i passaggi z non siano multipli esatti)
##della misura finale)
##COSE IN PROVA: G30 dovrebbe essere il "torna a casa" G28 definisce casa)
df.1giro <- data.frame(Y = NA, Z = NA, A = NA )
df.tutto <- data.frame()
passi.gradi <- 1:(v.1204*2)
passi.gradi <- passi.gradi[c(TRUE, FALSE)]
passi.prof <- 1:(v.1205)

for (k in passi.prof){
    for (i in passi.gradi){
        df.1giro[i,] <- c(0, v.2, v.202*i)
        df.1giro[i+1,] <- c(v.1, v.2, v.202*i)
    }
    df.tutto <- rbind.data.frame(df.tutto, df.1giro)
    v.1 <- (v.100/2-v.2) * 1/v.1105 ##spostamento asse y verso C)
    v.2 <- v.2 + v.203           ##spostamento asse z verso B)
}
# df.tutto
 v.1 <- v.105+v.106+v.204           #azzeramento POSIZIONE y)
 v.2 <- v.100/2+v.203    #azzeramento POSIZIONE z = FF'/2)

passi.prof <- 1:(v.1206)

for (k in passi.prof){
    for (i in passi.gradi){
        df.1giro[i,] <- c(v.105+v.106, v.2, v.202*i)
        df.1giro[i+1,] <- c(v.105+v.106+v.107, v.2, v.202*i)
    }
    df.tutto <- rbind.data.frame(df.tutto, df.1giro)
    v.2 <- v.2 + v.203           ##spostamento asse z verso B)
}
df.tutto




G30 ##torna a "casa", al punto G, A = zero gradi rotazione -forse ° inutili-)
##A0 Y0 Z[v.3] F[v.200] ##Posiziona inizialmente l'utensile sul bordo barra, in G))
M98P1700L[v.1206]    ##esegui subroutine O1700,taglia cono, [v.1206] volte)
v.1 <- 0.000            ##azzeramento POSIZIONE y)
v.2 <- v.100/2 + v.203   ##azzeramento POSIZIONE z = FF'/2)
G30 ##torna a "casa", al punto G, A = zero gradi rotazione -forse ° inutili-)
##A0 Y0 Z[v.3] F[v.200] ##Posiziona inizialmente l'utensile sul bordo barra, in G)
M98P1800L[v.1206]    ##esegui subroutine O1800, coda filettata, v.1206 volte )
##COSE DA FARE: scrivere routine filettatura)
##M98P1900L[v.1206]    esegui subroutine O1900, filettatura)
##MSG, REALIZZAZIONE PUNTALE ESEGUITA)
M30                 ##finisce il programma)



##%%%%%%%%%%%%%%%%% taglia diagonale, ruota, ritorna %%%%%%%%%%%%%%%%%%%%%%%%%%%)
##effettua un passaggio diagonale)
##da)
##un punto intermedio tra G e B situato in alle coordinate Y0Zv.1 )
##verso)
##un punto intermedio tra A e E, situato alle coordinate Yv.2Z0)
##ruotando il pezzo ogni volta v.202 gradi)
O500
G90 Y0 Z[v.2]  F[v.200]
G91A[v.202]  F1000 ##rotazione mandrino di v.202 gradi)
##MSG, Qui ci si possono mettere dei messaggi)
G90 Y[v.1] Z[v.2]  F[v.200]
M99 ##chiude O500 taglia diagonale ruota e ritorna)

##%%%%%%%%%%%%%%%%%%% affonda profondita diagonale di taglio %%%%%%%%%%%%%%%%%%%)
O600
##affonda la diagonale di asportazione della routine O500, cambiando v.1 e v.2)
v.1 <- v.1 + v.2 * 1/v.1105 ##spostamento asse y verso C)
v.2 <- v.2 + v.203           ##spostamento asse z verso B)
##v.2 <- [v.2+v.1107*COS[v.1106]] spostamento asse y verso C)
M99 ##chiude O600 affonda profondita diagonale di taglio)

##%%%%%%%%%%%%%%%%%%% affonda profondita dritta di taglio %%%%%%%%%%%%%%%%%%%%%%)
O601
##affonda la profondita' di asportazione di O700, coda filettata, cambiando v.1)
v.2 <- v.2+v.203           ##spostamento asse z verso B)
M99 ##chiude O601 affonda profondita di taglio)

##%%%%%%%%%%%%%%%%%%% asporta dietro fino a diametro MM' %%%%%%%%%%%%%%%%%%%%%%%)
O700
##effettua un passaggio rettilineo)
##dal )
##punto situato in alle coordinate L)
##verso)
##il punto situato alle coordinate M)
##ruotando il pezzo ogni volta v.202 gradi per v.1204 volte)
G90 Y[v.105+v.106] Z[v.2]
G91A[v.202] F1000 ##rotazione mandrino di v.202 gradi)
G90 Y[v.105+v.106+v.107] Z[v.2] F[v.200]
M99 ##chiude O700 asporta dietro)

##%%%%%%%%%%%%%%%%%%% taglia tutta la parte conica %%%%%%%%%%%%%%%%%%%%%%%%%%%%%)
O1700
##effettua un passaggio diagonale)
##da )
##un punto intermedio tra A e B situato in alle coordinate Y0Zv.1 )
##verso)
##un punto intermedio tra A e C, situato alle coordinate Yv.2Z0)
##ruotando il pezzo ogni volta v.202 gradi per v.1204 volte)
##Infine cambia i parametri dinamici v.1 e v.2 della routine 0600)
##che affondando l'asportazione per il successivo passaggio)
M98P500L[v.1204] ##M98 esegui asportazione e rotazione diagonale v.1204 volte)
M98P600L1       ##cambia una volta i parametri v.1 e v.2)
M99             ##chiude routine O1700 taglia tutta la parte conica)

##%%%%%%%%%%%%%%%%%%% taglia tutta la coda filettata %%%%%%%%%%%%%%%%%%%%%%%%%%%)
O1800 ##inizio routine coda filettata)
M98P400L[v.1204] ##M98 esegui asportazione e rotazione rettilinea v.1204 volte)
M98P601L1       ##cambia una volta il parametro v.1)
M99             ##chiude routine 01800 taglia tutta la coda filettata)

O1900
##%%%%%%%%%%%%%%%%%%% taglia la filettatura della coda %%%%%%%%%%%%%%%%%%%%%%%%)
##DA SCRIVERE TUTTA)
M99 ##chiude routine O1800 taglia la filettatura)
