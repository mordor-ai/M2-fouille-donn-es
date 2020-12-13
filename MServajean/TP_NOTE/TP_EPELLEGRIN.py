######## TP EMMANUEL PELLEGRIN ####
# 9/9/2020#
###################################

# Import de  spark
import pyspark
from pyspark.sql import SparkSession

spark = SparkSession.builder.appName("Python Spark").getOrCreate()
sc = spark.sparkContext

# Chargement des données csv

df_ddos = spark.read\
    .option("delimiter", ",")\
    .option("header", "true")\
    .option("inferSchema", "true")\
    .csv('dataset.netflow')


df_ddos.printSchema()