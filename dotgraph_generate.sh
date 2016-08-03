#! /bin/bash

set -e

ocamlfind ocamldoc \
          -thread \
          -syntax camlp4o \
          -package 'lwt,lwt.syntax' \
          -package 'lwt.unix,lwt.log,lwt-parallel' \
          -package 'batteries,text,text.pcre,text.pcre-syntax' \
          -package 'sqlexpr,sqlexpr.syntax,yojson' \
          -I _build/src/ \
          -I _build/src/parajobs/ \
          -I _build/src/tokenizer/ \
          -dot \
          src/analysis_aux.ml \
          src/analysis.ml \
          src/arg_aux.ml \
          src/cb.ml \
          src/cmp_texts.ml \
          src/cmp_token.ml \
          src/combine.ml \
          src/compare_texts.ml \
          src/compare_words.ml \
          src/dB.ml \
          src/otext.ml \
          src/settings.ml \
          src/parajobs/parallel_jobs.ml \
          src/tokenizer/tokenizer.ml \
          src/tokenizer/token.ml

dot \
    -Tpng \
    -Gsize=18,12\! \
    -Gdpi=100 \
    -Grotate=90 \
    -O \
    ocamldoc.out

feh ocamldoc.out.png


#    -hide BatExt_rand00,Pervasives \
#    lib_sc/src/*ml \
#    lib_batext_rand00/src/*.ml \
