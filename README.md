# TFG_AidaBravoMunoz

Este repositorio contiene el código, los datos y los materiales utilizados para el análisis del efecto del PEUAT sobre la evolución de Airbnb en Barcelona.

El objetivo del trabajo es estudiar si la entrada en vigor del PEUAT se asocia con un cambio en las nuevas altas de Airbnb, y, si es así, si es el esperado, diferenciando entre viviendas de uso turístico y hogares compartidos.

## Contenido del repositorio

El repositorio incluye:

* el código principal en R;
* los datos utilizados para construir la base de análisis;
* los gráficos incluidos en la memoria;
* los mapas incluidos en la memoria.

La estructura general del repositorio es la siguiente:

```text
TFG_AidaBravoMunoz/
├── CodigoTFG_AidaBravo.R
├── Datos/
├── Gráficos/
├── MAPAS/
└── README.md
```

## Código principal

El archivo principal es:

```text
CodigoTFG_AidaBravo.R
```

Este script contiene todo el proceso de análisis desarrollado en el trabajo. En concreto, incluye:

1. Carga y limpieza inicial de la base de datos de Airbnb;
2. Asignación espacial de cada anuncio a la zona PEUAT correspondiente;
3. Incorporación de la capa de barrios de Barcelona;
4. Tratamiento de los barrios partidos por la delimitación del PEUAT;
5. Construcción de variables demográficas y socioeconómicas;
6. Creación de los paneles barrio-mes para viviendas de uso turístico y hogares compartidos;
7. Análisis descriptivo y generación de gráficos;
8. Estimación de los modelos principales Poisson con efectos fijos;
9. Extracción e interpretación de los efectos fijos temporales y de barrio;
10. Validación de los modelos principales;
11. Estimación de modelos lineales complementarios;
12. Validación de los modelos complementarios.

## Datos

La carpeta `Datos` contiene los archivos necesarios para construir la base de análisis. Entre ellos se incluyen:

* datos de Airbnb;
* cartografía de barrios de Barcelona;
* cartografía de zonas PEUAT;
* datos de población;
* datos de inmigración;
* datos de movilidad;
* datos de renta;
* datos del índice de Gini.

## Gráficos y mapas

La carpeta `Gráficos` contiene las figuras generadas durante el análisis descriptivo, la validación de los modelos y la representación de efectos fijos temporales.

La carpeta `MAPAS` contiene los mapas utilizados en la memoria. Aquellos utilizados para la visualización de las zonas PEUAT y la división de sub-barrios; así como las representaciones de efectos fijos de barrio.

## Ejecución del código

El script está preparado para ejecutarse desde la carpeta principal del repositorio:

```text
TFG_AidaBravoMunoz/
```

Antes de ejecutar el código, es necesario comprobar que las carpetas `Datos`, `Gráficos` y `MAPAS` se encuentran en la misma ubicación que el archivo `CodigoTFG_AidaBravo.R`.

Si el repositorio se descarga en otro ordenador, puede ser necesario ajustar el directorio de trabajo o revisar las rutas definidas al inicio del script.

## Nota sobre reproducibilidad

El código permite reconstruir el proceso seguido en el análisis, desde la preparación de los datos hasta la estimación de los modelos y la generación de resultados. No obstante, algunos pasos dependen de archivos espaciales y datos externos que deben conservar la estructura de carpetas indicada para que el script se ejecute correctamente.

