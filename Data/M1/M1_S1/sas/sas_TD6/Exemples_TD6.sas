/*Importation store.csv*/
PROC IMPORT OUT= WORK.store
            DATAFILE= "C:\MySAS\store.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=yes;
     DATAROW=2; 
RUN;
PROC PRINT;
RUN;

*Histogramme;
PROC SGPLOT DATA = store;
 HISTOGRAM storeSize;
 TITLE "Quantité des produits stockés";
RUN;
*Histogramme avec densité;
PROC SGPLOT DATA = store;
 HISTOGRAM storeSize;
 DENSITY storeSize;
 TITLE "Quantité des produits stockés avec leur densité";
RUN;
* Colonnes (Bar Charts);
PROC SGPLOT DATA = store;
 VBAR storetypeID;
 TITLE 'Produits stockés par catégorie';
RUN; 
PROC SGPLOT DATA = store;
 VBAR storetypeID / GROUP = cityID;
 TITLE 'Produits stockés par catégorie et Villes';
RUN; 
PROC SGPLOT DATA = store;
 VBAR cityID / RESPONSE = storeSize;
 TITLE 'Quantité des Produits stockés par Ville';
RUN; 
* Série de données (Series plot);
PROC SGPLOT DATA = store;
 SERIES X = storeSize Y = cityID;
 TITLE 'Série des quantités des produits par ville';
RUN;
/*Ouverture*/
proc sgplot data=store;
   title1 "Quantité des produits par catégorie";
   styleattrs datacolors=(red green purple orange cyan) backcolor=vpav wallcolor=pwh;
   vbox storeSize / category=storetypeID group=storetypeID;
 
run;

