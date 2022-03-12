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
    if row[3] == "":
        continue
    cur.execute("INSERT INTO delivery_guid (id, name, repository_full_name, technology, quera_email) VALUES (%s, %s, %s, %s, %s)",
            (i, row[5], f"{row[3]}/{row[4]}", "django" if "Django" in row[2] else "dotnet", row[1]))
conn.commit()
cur.close()
conn.close()

