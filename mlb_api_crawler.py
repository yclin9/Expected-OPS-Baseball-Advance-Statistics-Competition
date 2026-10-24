import os
import requests
import csv

# year = date_in.split("-")[0]
# month = date_in.split("-")[1]
# date = date_in.split("-")[2]

year = 2025
month = 12
days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

plays = []
processed_gamepks = set()

for i in range(month):
    if i + 1 < 3 or i + 1 > 9:
        continue
    for day in range(1, days[i] + 1):
        DATE_URL = "https://statsapi.mlb.com/api/v1/schedule?sportId=1&date="
        date = f"{year}-{i + 1}-{day}"
        DATE_URL += date

        response = requests.get(DATE_URL)   
        data = response.json()
        if not data["dates"]:
            print(f"No data on {date}")
            continue
        dates = data["dates"]

        gamePks = []
        for d in dates:
            games = d["games"]
            for game in games:
                if game["gameType"] != "R":
                    continue
                if game["gamePk"] not in gamePks:
                    gamePks.append(game["gamePk"])
        if len(gamePks) > 0:
            print(f"Found {len(gamePks)} games on {date}")
        else:
            print(f"No data on {date}")
            continue
        
        for gamePk in gamePks:
            if gamePk in processed_gamepks:
                continue
            processed_gamepks.add(gamePk)
            GAME_URL = f"https://statsapi.mlb.com/api/v1.1/game/{gamePk}/feed/live"
            response = requests.get(GAME_URL)
            data = response.json()

            dateTime = data["gameData"]["datetime"]["dateTime"]
            
            allPlays = data["liveData"]["plays"]["allPlays"]
            if allPlays:
                batter_count = {}
                for play in allPlays:

                    pa = {}
                    result = play.get("result").get("event")
                    batterID = play.get("matchup").get("batter").get("id")
                    if (result.find("Caught") != -1) or (result.find("Pickoff") != -1) or (result.find("Stolen") != -1) or result == "Game Advisory" or result == "Runner Out" or result == "Field Out" or result == "Wild Pitch":
                        continue

                    if batterID not in batter_count:
                        batter_count[batterID] = 1
                    else:
                        batter_count[batterID] += 1
                    pa["batterID"] = batterID
                    pa["dateTime"] = dateTime
                    pa["gamePk"] = gamePk
                    pa["appearance"] = batter_count[batterID]
                    pa["result"] = result
                    plays.append(pa)

with open("2025_results.csv", "w", newline= "", encoding= "utf-8-sig") as file:
    fieldnames = ["batterID", "dateTime", "gamePk", "appearance", "result"]
    writer = csv.DictWriter(file, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(plays)
    print("File written successfully.")
