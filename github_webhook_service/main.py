import docker
import gspread
import flask

"""
gc = gspread.service_account(filename='enduring-grid-343612-1bee3e445db0.json')
sh = gc.open("Scoreboard")
sh.sheet1.update('A1', "Good Evening")
print(sh.sheet1.get('A1'))
"""

client = docker.from_env()
l = client.images.list()
print(l)