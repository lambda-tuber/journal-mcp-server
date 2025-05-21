#!/bin/bash
#
# 使用例:
#   ./journal_entry.sh 現金 売上 10000 2025 05 "売上入金"
#   ./journal_entry.sh 現金 売上 10000 "" "" "テスト備考"
#
# 出力例（JSON形式）:
# {
#   "status": "success",
#   "debit_account": "現金",
#   "credit_account": "売上",
#   "amount": 10000,
#   "year": 2025,
#   "month": 5,
#   "debit_file": "04_journal-db/2025/05/現金/dr/15d123045.123456789.6a2..._売上入金",
#   "credit_file": "04_journal-db/2025/05/売上/cr/15d123045.123456789.6a2..._売上入金"
# }
#

DR_ACCOUNT=$1
CR_ACCOUNT=$2
AMOUNT=$3
YEAR=${4:-$(date +%Y)}
MONTH=${5:-$(date +%m)}
REMARK=$6

BASE_DIR="04_journal-db"

# 引数チェック
if [ -z "$DR_ACCOUNT" ] || [ -z "$CR_ACCOUNT" ] || [ -z "$AMOUNT" ]; then
  echo "Usage: $0 DR_ACCOUNT CR_ACCOUNT AMOUNT [YEAR MONTH]" >&2
  exit 1
fi

# 日時パーツを変数に分けて取得
DAY=$(date +%d)
TIME=$(date +%H%M%S)
NANO=$(date +%N)
UUID=$(uuidgen)

# タイムスタンプ作成
TIMESTAMP="${DAY}d${TIME}.${NANO}_${UUID}"

# 備考をファイル名用に整形（スペース→_、記号除去）
if [ -n "$REMARK" ]; then
  SAFE_REMARK=$(echo "$REMARK" | tr -s ' ' '_' | sed 's/[\/:*?"<>|]//g')
  TIMESTAMP="${TIMESTAMP}_$SAFE_REMARK"
fi

# 借方ディレクトリ作成
DR_DIR="${BASE_DIR}/${YEAR}/${MONTH}/${DR_ACCOUNT}"
mkdir -p "${DR_DIR}/dr" "${DR_DIR}/cr"

# 貸方ディレクトリ作成
CR_DIR="${BASE_DIR}/${YEAR}/${MONTH}/${CR_ACCOUNT}"
mkdir -p "${CR_DIR}/dr" "${CR_DIR}/cr"

# 借方ファイル作成
DR_FILE="${DR_DIR}/dr/${TIMESTAMP}"
truncate -s "$AMOUNT" "$DR_FILE"
if [ $? -ne 0 ]; then
  echo "{\"status\": \"error\", \"message\": \"借方ファイル作成失敗\", \"file\": \"$DR_FILE\"}" >&2
  exit 1
fi

# 貸方ファイル作成
CR_FILE="${CR_DIR}/cr/${TIMESTAMP}"
truncate -s "$AMOUNT" "$CR_FILE"
if [ $? -ne 0 ]; then
  echo "{\"status\": \"error\", \"message\": \"貸方ファイル作成失敗\", \"file\": \"$CR_FILE\"}" >&2
  exit 1
fi

# JSON出力
echo "{
  \"status\": \"success\",
  \"year\": \"$YEAR\",
  \"month\": \"$MONTH\",
  \"amount\": $AMOUNT,
  \"dr_account\": \"$DR_ACCOUNT\",
  \"cr_account\": \"$CR_ACCOUNT\",
  \"dr_file\": \"$DR_FILE\",
  \"cr_file\": \"$CR_FILE\"
}"

# JSONを標準出力
echo "$OUTPUT_JSON"

# CSVログファイルに追記（ヘッダは最初の1回だけ作成）
LOG_FILE="journal_entry.csv"
if [ ! -f "$LOG_FILE" ]; then
  echo "datetime,dr_account,cr_account,amount,year,month,day,time,nanosecond,uuid,dr_file,cr_file" >> "$LOG_FILE"
fi

# 現在日時
NOW=$(date '+%Y-%m-%d %H:%M:%S')

# CSV行を書き込み
echo "\"$NOW\",\"$DR_ACCOUNT\",\"$CR_ACCOUNT\",\"$AMOUNT\",\"$YEAR\",\"$MONTH\",\"$DAY\",\"$TIME\",\"$NANO\",\"$UUID\",\"$DR_FILE\",\"$CR_FILE\"" >> "$LOG_FILE"

exit 0

