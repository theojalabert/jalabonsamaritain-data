*Importation store.csv*/
PROC IMPORT OUT= WORK.store
            DATAFILE= "C:\MySAS\store.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=yes;
     DATAROW=2; 
RUN;
PROC PRINT;
RUN;

/*Renommer des variables*/
DATA store_V2;
SET store;
RENAME storeID = ID
 storetypeID=storetype;
RUN;
PROC PRINT;
RUN;
/*Selectionner les variables à afficher dans la nouvelle base de données*/
DATA store_V3;
SET store;
KEEP storeID;
RUN;
PROC PRINT;
RUN;
/*fonctions statistiques*/
DATA notes; 
INPUT nom$ note1 note2 note3; 
CARDS;
SARA 13 14 16
PAUL 17 10 9
VICTOR 14 15 12
;
RUN;
DATA note_V2;
SET notes;
Max=max(of note1-note3);
Min=min(of note1-note3);
Moyenne=mean(of note1-note3);
Somme=sum(of note1-note3);
Standard_Deviation=std(of note1-note3);
Median=median(of note1-note3);
RUN;
PROC PRINT;
RUN;
/* Fonctions Statistique 2 */
DATA NormalDistribution;
cdf=CDF('NORMAL',0,0,1);
pdf=PDF('NORMAL',0,0,1);
quantile=QUANTILE('NORMAL',0.5,0,1);
RandomNumber=RAND('NORMAL',0,1);
RUN;
PROC PRINT;
RUN;
/*Procedure FREQ*/
DATA notes; 
INPUT nom$ note1 note2 note3 mention$; 
CARDS;
SARA 13 14 16 "admis"
PAUL 17 10 9 "admis"
VICTOR 4 5 7 "ajournee"
;
RUN;
/*Table de fréquence*/
PROC FREQ DATA = notes ;
RUN;
PROC PRINT;
RUN;
/*Table de fréquence des variables selectionnées*/
PROC FREQ DATA = notes ;
TABLES mention;
RUN;
PROC PRINT;
RUN;
/*Table de contingence */
PROC FREQ DATA = notes ;
TABLES note1*mention;
RUN;
PROC PRINT;
RUN;

/*Procedure MEANS*/
DATA notes; 
INPUT nom$ note1 note2 note3 mention$; 
CARDS;
SARA 13 14 16 "admis"
PAUL 17 10 9 "admis"
VICTOR 4 5 7 "ajournee"
;
RUN;
/*Statistiques*/
PROC MEANS DATA = notes ;
RUN;
PROC PRINT;
RUN;
/*Statistiques en séléctionnant les variables*/
PROC MEANS DATA = notes ;
VAR note1;
RUN;
PROC PRINT;
RUN;
