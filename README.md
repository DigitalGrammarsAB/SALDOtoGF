# SALDO to GF

[SALDO](https://spraakbanken.gu.se/eng/resource/saldo) (Swedish Associative Thesaurus version 2) is an extensive electronic lexicon resource for modern Swedish written language.

[Grammatical Framework](https://www.grammaticalframework.org) is a programming language for multilingual grammar applications.

This repository contains a script for converting SALDO into a large-scale, monolingual, morphological GF dictionary.
The dictionary itself, which is produced from the script, is part of GF's open-source [Resource Grammar Library](https://github.com/GrammaticalFramework/gf-rgl) (RGL) and can be found under `src/swedish/NewDictSwe.gf`.

This work is primarily based on that of Malin Ahlberg: <https://github.com/MalinAhlberg/SwedishProject/tree/master/saldo>
and has undergone improvement by [Digital Grammars AB](https://www.digitalgrammars.com/).

## Usage

The SALDO data itself is not included as part of this repository.
We specifically use the morphological SALDO lexicon (`saldom`).
This is obtained from:

- LMF (XML): <https://svn.spraakdata.gu.se/sb-arkiv/pub/lmf/saldom/saldom.xml>
- JSON: ?

The main executable is then run with this file as input argument, e.g.:

```
stack run -- data/saldo.xml
```

## Folder structure

- `data`: SALDO source data, in XML or JSON format
- `src`: Haskell source code
- `logs`: output logs
- `generate`: generated GF files
- `build`: compiled PGFs and binaries
