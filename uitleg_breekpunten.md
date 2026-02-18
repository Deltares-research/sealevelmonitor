# Conceptueel verschil tussen `strucchange` en `segmented`

2026-02-17 Copilot

## **1. `strucchange` – zoekt *structuurbreuken* (abrupt, onbekend, data‑gedreven)**

**Hoofdrol:** detectie van *onbekende* breekpunten in tijdreeksen of regressiemodellen.\
**Focus:** *modelstructuur* verandert abrupt → andere regressieparameters voor/na het breekpunt.\
**Typische vraag:** “**Waar** zitten breekpunten? En zijn ze significant?”

### Kenmerken

-   **Breekpunten worden automatisch gezocht** (Bai–Perron methodologie).

-   Detecteert **één of meerdere** breuken.

-   Werkt goed in **tijdreekscontext**, inclusief autocorrelatie.

-   Richt zich op **harde shifts**: niveau, trend, variantie, regressiecoëfficiënten.

-   Biedt tests (CUSUM, MOSUM, F‑statistieken).

### Gebruiksscenario

> *Je vermoedt een structurele verandering, maar weet niet waar.*

## **2. `segmented` – past *piecewise regressies* toe (met continue aansluiting, model‑gedreven)**

**Hoofdrol:** regressiemodellen waarbij je verwacht dat **hellingen veranderen**, maar dat die lijnen **continu** aan elkaar passen.\
**Focus:** *verandering in trend* (slope), vaak niet abrupt in niveau.

### Kenmerken

-   Je **start met een regressiemodel** en laat het pakket **breekpunten in helling** schatten.

-   Garantie dat de stukjes regressie **continu aansluiten** (geen sprongen).

-   Werkt vooral goed bij **niet‑tijdreeksdata** of wanneer autocorrelatie geen issue is.

-   Je kunt één of meer breekpunten laten schatten, of een initiële guess geven.

### Gebruiksscenario

> *Je verwacht dat de trend verandert — bijvoorbeeld een groeicurve met knikken — en wilt een vloeiend model met meerdere segmenten.*

# Belangrijkste conceptuele tegenstelling

| Aspect | `strucchange` | `segmented` |
|----|----|----|
| **Type breekpunt** | Abrupte *structuurbreuk* in model / tijdreeks | Verandering in *slope*, continu model |
| **Doel** | Detectie van onverwachte breaks | Modelling van piecewise lineaire relaties |
| **Input** | Tijdreeksen of regressie | Regressiemodel (lm/glm/etc.) |
| **Continuïteit** | **Niet** noodzakelijk | **Altijd** continu |
| **Zoekt breekpunten zelf?** | Ja, volledig data-gedreven | Ja, maar binnen het gegeven model |
| **Autocorrelatie** | Goed ondersteund | Niet primair bedoeld voor tijdreeksen |
| **Use case** | Detectie: *waar* zijn de breuken? | Modelleren: *hoe* verandert de trend? |

# In één zin

-   **strucchange** → detecteert *abrupte structurele veranderingen* op onbekende tijdstippen.

-   **segmented** → modelleert *piecewise regressies* met veranderende hellingen die *continu* aan elkaar passen.

Als je wilt, kan ik ook helpen bepalen **welk van de twee het meest geschikt is voor jouw dataset**, of voorbeeld‑R‑code maken voor beide varianten.
