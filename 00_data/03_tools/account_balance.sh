#!/bin/bash
#
# 実行例:
#   ./account_balance.sh 現金
#   ./account_balance.sh 現金 2025
#   ./account_balance.sh 現金 2025 04
#
# 出力例（JSON形式）:
# {
#   "account": "現金",
#   "debit": 150000,
#   "credit": 50000,
#   "balance": {
#     "side": "debit",
#     "value": 100000
#   }
# }
#

ACCOUNT=$1
YEAR_LIMIT=${2:-$(date +%Y)}
MONTH_LIMIT=${3:-""}

if [ -z "$ACCOUNT" ]; then
  echo "Usage: $0 ACCOUNT_NAME [YEAR] [MONTH]" >&2
  exit 1
fi

JOURNAL_DIR="04_journal-db"

sum_dr=0
sum_cr=0

process_dir() {
  local base_path=$1
  local side=$2

  if [ ! -d "$base_path/$side" ]; then
    echo 0
    return
  fi

  local total
  total=$(find "$base_path/$side" -type f -printf "%s\n" | awk '{sum+=$1} END {print sum+0}')
  echo "$total"
}

for year_dir in "$JOURNAL_DIR"/*; do
  year=$(basename "$year_dir")

  if [ "$year" -gt "$YEAR_LIMIT" ]; then
    continue
  fi

  for month_dir in "$year_dir"/*; do
    month=$(basename "$month_dir")

    if [ "$year" -eq "$YEAR_LIMIT" ] && [ -n "$MONTH_LIMIT" ] && [ "$month" -gt "$MONTH_LIMIT" ]; then
      continue
    fi

    base_path="$month_dir/$ACCOUNT"

    dr_val=$(process_dir "$base_path" "dr")
    cr_val=$(process_dir "$base_path" "cr")

    sum_dr=$((sum_dr + dr_val))
    sum_cr=$((sum_cr + cr_val))
  done
done

balance=$((sum_dr - sum_cr))

if [ "$balance" -gt 0 ]; then
  balance_side="debit"
  balance_value=$balance
elif [ "$balance" -lt 0 ]; then
  balance_side="credit"
  balance_value=$(( -balance ))
else
  balance_side="none"
  balance_value=0
fi

# JSON 一行出力
echo "{
  \"account\": \"$ACCOUNT\",
  \"debit\": $sum_dr,
  \"credit\": $sum_cr,
  \"balance\": {
    \"side\": \"$balance_side\",
    \"value\": $balance_value
  }
}"

exit 0

