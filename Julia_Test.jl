using Pkg
using RCall
using GLM
using StatFiles
using DataFrames
using Random

df = read_dta("G:/My Drive/FGV EESP/3o SEMESTRE/Econo I/Data/basecon1_old.dta")

df = DataFrame(load("G:/My Drive/FGV EESP/3o SEMESTRE/Econo I/Data/basecon1_old.dta"))

lm(@formula(df.LWKLYWGE ~ df.AGE), df)

first(df)
