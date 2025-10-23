# CienciaDeDatoParaCiudades
Para instalar R (lenguaje)
1. sudo apt install --no-install-recommends software-properties-common dirmngr
2. wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
3. sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
4. sudo apt update
5. sudo apt install r-base

Para ejecutarlo:
Opcion 1:
  Rscript /ruta/a/tu/archivo.r
Opcion 2:
  1. Abro R en una terminal: R (esto abre una consola de R)
  2. Dentro de la consola: > source("ruta al archivo.R")
  3. para salir de la consola: > quit()

 Al ejecutarlo se crean los .png que son los graficos, un htlm que es un informe de lo que se ejecuto junto con los resultados 
