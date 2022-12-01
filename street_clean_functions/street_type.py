#!/usr/bin/env python
# coding: utf-8

# In[ ]:


#import the packages that needed
import pandas as pd
import numpy as np
import regex as re
import warnings
warnings.filterwarnings('ignore')


# In[ ]:


def street_type(x):
    pattern = re.compile(r'^(\W+\s)')
    x = re.sub(pattern, "", x)
    pattern = re.compile(r'\sSTREET|\sSTREE$|\sSTR|\sSTE$|\sSRT$|\sSR$|\sSST$|\sSEET$|\sTREET$|\sSHEER$|\sSHEE$|\sSTREE$|\sSREET$|\sREET$|\sSTEE$|\sST$|\sS$')
    x = re.sub(pattern, " ST", x)
    pattern = re.compile(r'\sDRIVE$|\sDRV$|\sDRI$|\sDRIV$|\sDRIE$|\sDE$|\sDV$|\sDR$')
    x = re.sub(pattern, " DR", x)
    pattern = re.compile(r'\sCIRCLE$|\sCIRCL$|\sCICLE$|\sCIRC$|\sCIR$|\sCRL$|\sCL$|\sCR$')
    x = re.sub(pattern, " CIR", x)
    pattern = re.compile(r'\sAVENUE$|\sAVENUE|\sAVENU$|\sAVEN$|\sAVE$|\sAVN$|\sAV$')
    x = re.sub(pattern, " AVE", x)
    pattern = re.compile(r'^AVENUE\s|^(\W+)\sAV\s')
    x = re.sub(pattern, "AVE ", x)
    pattern = re.compile(r"\sCOURT$|\sCT$|\sCRT$|\sCTR$|\sCOUR$|<=\sCOT$|\sCORT$")
    x = re.sub(pattern, " CT", x)
    pattern = re.compile(r"\sBOULEVARD$|\sBVLD|\sBL.$|\sB.$")
    x = re.sub(pattern, " BLVD", x)
    pattern = re.compile(r"\sROAD$|\sRD$|\sRAD$|\sROD$")
    x = re.sub(pattern, " RD", x)
    pattern = re.compile(r"\sALLEY$|\sALY$|\sALEY$|\sALL.$|\sAL$|\sAY$")
    x = re.sub(pattern, " ALY", x)
    pattern = re.compile(r"\sPLACE$|\sPL.$|\sP.$|\sPLAC$|\sPLCE$|\sPCE$")
    x = re.sub(pattern, " PL", x)
    pattern = re.compile(r"\sPK$|\sPRK$|\sPRAK$|\sPAK$")
    x = re.sub(pattern, " PARK", x)
    pattern = re.compile(r"\sPARKWAY$|\sPKWY$|\sPARKW$|\sPWY$|\sPKW$|\sPRKWY$|\sPKW$")
    x = re.sub(pattern, " PKWY", x)
    pattern = re.compile(r"\sAPPROA$|\sAPRCH$|\sAPPRCH$|\sAPPR$|\sAPR$")
    x = re.sub(pattern, " APPROACH", x)
    pattern = re.compile(r"\sTERRACE$|\sTERR$|\sTER$|\sTRCE$|\sTRC$|\sTR$")
    x = re.sub(pattern, " TER", x)
    pattern = re.compile(r"\sPLAZA$|\sPLZA$|\sPLZ$|\sPLAZ$|\sPZ$")
    x = re.sub(pattern, " PLZ", x)
    pattern = re.compile(r"\sLANE$|\sLNE$|\sLN$|\sLAN$")
    x = re.sub(pattern, " LN", x)
    pattern = re.compile(r"\sBRIDGE$|\sBRGD$|\sBRG$|\sBGE$")
    x = re.sub(pattern, " BRG",x)
    pattern = re.compile(r"\sHILL$|\sHLL$|\sHL$|\sHIL$")
    x = re.sub(pattern, " HL", x)
    pattern = re.compile(r"\sHEIGHTS$|\sHTS$|\sHT$|\sHEGHTS$|\sHEIGHT$|\sHHT$|\sHEIGT$") 
    x = re.sub(pattern, " HTS", x)
    pattern = re.compile(r"\sSLP$|\sSLEP$|\sSLIIP$|\sSLI$")
    x = re.sub(pattern, " SLIP", x)
    pattern = re.compile(r"\sROOW$|\sRO.$|\sRW$")
    x = re.sub(pattern, " ROW", x)
    pattern = re.compile(r"\sSQUARE$") 
    x = re.sub(pattern, " SQ", x)
    pattern = re.compile(r"\sSQUARE\s") 
    x = re.sub(pattern, " SQ ", x)

    return x

