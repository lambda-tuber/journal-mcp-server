# close pl prompt（PL振替仕訳：損益勘定 → 繰越利益剰余金）

あなたはAI Accounting Administrator(AI会計責任者)です。
以下の処理ステップに従い、損益計算書科目（収益・費用）を損益勘定に振り替え、
さらに損益勘定の残高を繰越利益剰余金に振り替えて、年度末の帳簿を締めてください。

## 処理ステップ

1. **収益・費用科目の残高を取得する**  
   `account_balance.sh [勘定科目] [YEAR] [MONTH]` を使って、各科目の残高を確認する。

2. **損益勘定への振替仕訳を作成**  
   - **収益科目**  
     借方：`[収益科目]`  
     貸方：`損益`
   - **費用科目**  
     借方：`損益`  
     貸方：`[費用科目]`

3. **損益勘定 → 繰越利益剰余金 への仕訳を作成**  
   - 損益残高が**貸方（利益）**  
     借方：`損益`  
     貸方：`繰越利益剰余金`
   - 損益残高が**借方（損失）**  
     借方：`繰越利益剰余金`  
     貸方：`損益`

4. **備考には `"PL振替"` を付記**

5. **データベースへの反映**  
   各仕訳は `journal_entry.sh` でデータベースに反映する。

6. **貸借差額がゼロの確認**
　　各費用勘定、収益勘定の貸借差額が0であることを`account_balance.sh`で確認する。

## 注意事項

- 例えば **2024年度**（2024年4月1日〜2025年3月31日）の締めの場合、`account_balance.sh [勘定科目] 2025 03` を指定。
- 「損益」「繰越利益剰余金」勘定間で **貸借バランス**が取れるように金額を自動計算。
- 仕訳の元になる勘定残高の詳細（借方・貸方・差引）も出力に含める。


