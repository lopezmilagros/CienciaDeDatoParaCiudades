# TPE - Ciencia de Datos para Ciudades

Análisis de tiempos de viaje utilizando el dataset  
**“Viajes origen destino optativa Exactas.xlsx”** y lenguaje **R**.

Este proyecto reproduce la tabla de **tiempo promedio de viaje por motivo**  
y calcula métricas adicionales de movilidad urbana para el trabajo práctico.

---

## 1. Requisitos

- Sistema operativo: Linux (probado en Ubuntu/Debian).
- Conexión a internet (solo para instalar paquetes).
- Espacio en disco para R y las librerías.

---

### 2. Instalación de R
Para instalar R (lenguaje)

    sudo apt install --no-install-recommends software-properties-common dirmngr
    wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
    sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
    sudo apt update
    sudo apt install r-base

Para ejecutarlo: Opcion 1: Rscript /ruta/a/tu/archivo.r Opcion 2:

    Abro R en una terminal: R (esto abre una consola de R)
    Dentro de la consola: > source("ruta al archivo.R")
    para salir de la consola: > quit()

Al ejecutarlo se crean los .png que son los graficos, un htlm que es un informe de lo que se ejecuto junto con los resultados



### 3. Paquetes necesarios
Ejecute en la terminal:

```bash
R
```
Dentro de R, instalar los paquetes necesarios (solo la primera vez):
```bash
install.packages(c("jsonlite", "readxl", "dplyr", "janitor", "ggplot2"))
q()  # para salir de R
```

## 2. Descargar el proyecto desde GitHub

### **Opción A (recomendada): Descargar ZIP**
1. Ir al repositorio oficial:  
   👉 **https://github.com/lopezmilagros/CienciaDeDatoParaCiudades**
2. Clic en **Code** (botón verde)
3. Clic en **Download ZIP**
4. Descomprimir la carpeta en tu computadora  
   (Ej: `/home/milagros/Documentos/C. ciudades/TPE-GRUPO21/`)

---

### **Opción B: Clonar el repositorio (si usás Git)**

```bash
git clone https://github.com/lopezmilagros/CienciaDeDatoParaCiudades
```

# Ejecutar codigo
abrir una terminal y ejecutar 
```bash
R
```
Dentro de la consola de R escribiir:
```bash
source("TrabajoPractico.R", encoding = "UTF-8")
tabla_tiempo_promedio_motivo
```