#!/bin/bash

ACCOUNTS=(
  "現金 売上"
  "仕入 現金"
  "雑費 現金"
  "現金 短期借入金"
)

for m in {0..11}; do
  y=2023
  month=$((4 + m))
  if [ "$month" -gt 12 ]; then
    month=$((month - 12))
    y=2024
  fi
  month=$(printf "%02d" "$month")
  for i in {1..10}; do
    pair=${ACCOUNTS[$RANDOM % ${#ACCOUNTS[@]}]}
    dr=$(echo "$pair" | cut -d' ' -f1)
    cr=$(echo "$pair" | cut -d' ' -f2)
    amount=$(( (RANDOM % 20 + 1) * 1000 ))  # 1000〜20000円
    echo "./03_tools/journal_entry.sh $dr $cr $amount $y $month"
  done
done

