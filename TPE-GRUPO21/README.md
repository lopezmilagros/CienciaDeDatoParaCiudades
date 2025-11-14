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

#### 2.1. Instalar R desde los repositorios de Ubuntu/Debian

Abrir una terminal y ejecutar:

```bash
sudo apt update
sudo apt install r-base r-base-dev
```

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
source("TrabajoPractico.R", encoding = "UTF-8")
