# Analisi del titolo MSFT

Progetto per il corso *Metodi Probabilistici e Statistici per i Mercati Finanziari* che contiene codice e dati raccolti per analizzare il titolo *Microsoft Corporation*.

## Scopo del progetto

Il progetto si focalizza sull'analisi del titolo azionario *MSFT* i cui dati storici sono stati ottenuti da Yahoo Finance. In particolare, è stato analizzato l'andamento del titolo negli ultimi due anni, è stata calcolata la volatilità del titolo e sono stati confrontati i rendimenti attesi ottenuti tramite il modello CRR con qelli reali dei dati storici.

## Prelievo dei dati

Per il titolo *MSFT* sono stati prelevati i dati storici dal 28/08/2021 al 28/08/2023. Da questi dati sono stati ricavati:
- Un grafico dei rendimenti giornalieri.
- La volatilità utilizzata per i parametri del modello.

Inoltre, sono state prelevate giornalmente le opzioni *Call* e *Put* utilizzate come confronto per i risultati del modello CRR.

## Calibrazione del modello

Il modello CRR è stato calibrato utilizzando come parametri:

- Numero di *Trading Days* (N): 14 (ovvero i giorni di trading dal 28/08/2023 al 15/09/2023)
- Time step ($\Delta t$): 1 (ovvero un giorno in un *Trading Year*)
- Esito positivo (u): $e^{\sigma \sqrt{\Delta t}}$
- Esito negativo (d): $e^{-\sigma \sqrt{\Delta t}}$
- $\tilde{p}$: $\frac{1+r - d}{u - d}$
- $\tilde{q}$: $\frac{u - (1+r)}{u - d}$