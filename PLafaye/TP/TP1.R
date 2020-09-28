
# Integer 
rep1 <-rep(1L,10000)
rep1
object.size(rep1)/10000
# 4 octets
# réel
rep_real <-rep(1.0,10000)
rep_real
object.size(rep_real)/10000
# 8 octets
#texte
rep_text <-paste(rep("a",10000),collapse = "")
rep_text
object.size(rep_text)/10000
# 1 octet
#logique
rep_logic  <- rep(TRUE,10000)
rep_logic
object.size(rep_logic)/10000
# 8 octets
#nombres complexes
rep_complex  <- rep(1i,10000)
rep_complex
object.size(rep_complex)/10000
# 16 octets


f_convert_hex_to_bin <- function(hexa){
 as.numeric(hexa)
}

f_convert_bin_to_hex <-function(bina){
  as.hexmode(bina)
}





f_convert_bin_to_hex(160)
f_convert_hex_to_bin('0xa0')



setwd("~/Documents/M2-fouille-donn-es/PLafaye/TP")
data_affairs <-read.csv("./Affairs.csv")
object.size(data_affairs)/10000


