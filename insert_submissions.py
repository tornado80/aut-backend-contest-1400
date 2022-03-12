import gspread
import psycopg2

conn = psycopg2.connect(host="localhost", port=5432, dbname="judge_db", user="judge_user", password="judge_password")

cur = conn.cursor()

gc = gspread.service_account(filename="service_account_credentials.json")

sh = gc.open("Responses")

result = sh.sheet1.get_all_values()
#with open("file.txt", "w", encoding="utf-8") as f:
#    f.write(str(result))
#    print(result)
for i in range(1, len(result)):
    row = result[i]
    print("inserting", row[1])
    if row[6] == "":
        continue
    cur.execute("""INSERT INTO submission ("
                delivery_guid,
                repository_full_name,
                repository_name,
                repository_clone_url,
                repository_owner_login,
                head_commit_id,
                head_commit_message,
                head_commit_timestamp,
                pushed_at_timestamp,
                status
                ) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)""",
            (f"{row[3]}-{row[4]}-latest",
             f"{row[3]}/{row[4]}",
             row[4],
             row[5],
             row[3],
             row[6],
             "commit-message",
             "commit-timestamp",
             "queue"))
conn.commit()
cur.close()
conn.close()

