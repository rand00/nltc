#! /bin/bash

DIR="src"

wc -l \
    $DIR/analysis_aux.ml \
    $DIR/analysis.ml \
    $DIR/cb.ml \
    $DIR/cmp_texts.ml \
    $DIR/cmp_token.ml \
    $DIR/combine.ml \
    $DIR/dbWrap.ml \
    $DIR/omphand_aux.ml \
    $DIR/omphand.ml \
    $DIR/otext.ml \
    $DIR/tokenizer/token.ml \
    $DIR/tokenizer/tokenizer.ml \
    $DIR/stats.ml
