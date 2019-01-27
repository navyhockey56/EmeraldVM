#!/bin/bash

for i in $(seq 1 `ls inputs/public* | wc -l`);do
  echo $i: $(../main.byte inputs/public_$i.evm | diff -Bw outputs/public_$i.out -);
done
