# Het effect van verschillende maatregelen op rendementen van partciulieren verhuurders 

## Doel van het Onderzoek
- 	Particuliere verhuurders hebben sinds 2022 te maken met tenminste drie ontwikkelingen die het rendement van huurwoningen drukken: (1) de stijging van de hypotheekrente, (2) de aangekondigde Wet betaalbare huur en (3) de aanpassing van drie fiscale maatregelen (i. box 3, ii. leegwaarderatio en iii. overdrachtsbelasting). Deze ontwikkelingen worden in verband gebracht met een toename in de verkoop van huurwoningen  en stijgende huren .
-	We hebben gekeken naar de effecten van bovenstaande factoren op het verwachte nettorendement van een particuliere verhuurders. Voortbouwend op het onderzoek en model van SEO, hebben we gebruik gemaakt van een dataset van ca. 5.500 huurwoningen van Box 3-beleggers, gekoppeld aan gegevens van het Kadaster.
-	Voor de circa 5500 huurwoningen in onze dataset kijken we naar twee scenario’s: het nul-scenario en het beleidsscenario. Het nul-scenario is het scenario waarin het nettorendement wordt geschat zonder hypotheekrentestijging en beleidsmaatregelen. Het beleidsscenario bevat de nieuwe beleidsmaatregelen en de gestegen hypotheekrente. In beide scenario’s wordt gekeken naar een exploitatietermijn van 15 jaar.

## Data
- Voor dit onderzoek is gebruik gemaakt van een dataset van huurwoningen die in het tweede kwartaal van 2022 op onlinehuurplatformen, zoals Pararius, stonden. De dataset bevat informatie over het adres, de gevraagde huurprijs en verschillende karakteristieken van de woning, zoals woonoppervlakte, energielabel en bouwjaar. Deze data is gekoppeld aan eigendomsinformatie van het Kadaster, waaronder gedetailleerde gegevens over eigendom (natuurlijk persoon), de aankoopsom en het aankoopjaar van de woning. 

- In totaal omvat de dataset 13.122 woningen die zijn verzameld van online platformen voor huurwoningen gedurende de onderzoeksperiode. Om een link te leggen met particuliere beleggers, zijn er 5.498 woningen geïdentificeerd als eigendom van natuurlijke personen, aan de hand van de beschikbare informatie in het Kadaster. Het is belangrijk op te merken dat een aantal woningen uitgesloten moest worden vanwege lage datakwaliteit. We hebben ervoor gekozen om deze woningen niet op te nemen in de analyse, om de betrouwbaarheid en nauwkeurigheid van de resultaten te waarborgen. 

De onderliggende data mag niet gedeeld worden met derden. Daarom is er een gesimuleerde dataset gepubliceerd met gelijke karakteristieken, zodat de analyse gerepliceerd kan worden. 


## Methode
- Dit onderzoek werkt met dezelfde DCF-methodiek als de analyse van SEO die is toegepast in het stapelingsonderzoek.  Voor de berekening van het rendement van de woningen in de dataset worden inkomsten en uitgaven over een exploitatietermijn van 15 jaar gemodelleerd. Inkomsten en uitgaven over de exploitatietermijn worden vervolgens verdisconteerd aan de hand van een discounted cash flow (DCF) methode. Vervolgens kan het gemodelleerde rendement berekend worden.
-	Het gemodelleerde rendement wordt geschat met de Internal Rate of Return (IRR) methode. Om iets te kunnen zeggen over de impact van beleidsmaatregelen en externe factoren op het toekomstige rendement van verhuurders wordt de IRR toegepast. De IRR is de disconteringsvoet die de netto contante waarde (NCW) van een project nul maakt. Met andere woorden: de IRR is het verwachte samengestelde jaarlijkse rendement van de huurwoning. Hoe hoger de IRR, hoe hoger de kasstromen in vergelijking met de investeringskosten. De IRR zegt daarmee niets over de werkelijke investeringsbeslissing (zoals: verkopen of aanhouden), maar kan wel vergeleken worden met de IRR van andere investeringen.



## Structuur van Repo
