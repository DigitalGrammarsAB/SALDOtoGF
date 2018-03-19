# SALDO to GF

Script for importing SALDO as a GF grammar
Based primarily on the work of Malin Ahlberg: <https://github.com/MalinAhlberg/SwedishProject/tree/master/saldo>

Also see Krasimir Angelov's older import script: [make-saldo.hs](make-saldo.hs)

## Usage

```
runghc SaldoMain.hs "saldo1.json"
```

Import of all SALDO is quite slow (in particular the `updateGF` function).
TODO: re-enable splitting into smaller parts as in Malin's original code (see `SaldoTools.splts`).

## General SALDO info

- Official download page: <https://spraakbanken.gu.se/swe/resurs/saldom>
- Latest release (LMF/XML): <https://svn.spraakdata.gu.se/sb-arkiv/pub/lmf/saldom/saldom.xml>
- JSON format: ...
