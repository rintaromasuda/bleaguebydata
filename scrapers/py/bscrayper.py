import requests as rq
from bs4 import BeautifulSoup
import json as js
import pandas as pd
import numpy as np

def main():
    res = rq.get("https://www.bleague.jp/schedule/?tab=1&year=2019&event=2&club=&setuFrom=1&setuTo=36")
    soup = BeautifulSoup(res.content, 'html.parser')

    df_pbyp = pd.DataFrame()

    for div_tag in soup.find_all('div', {'class': "state_link btn report"}):
        detail_link = div_tag.a['href']
        res = rq.get(detail_link)
        soup = BeautifulSoup(res.content, 'html.parser')
        print(soup.title)

        for script_tag in soup.find_all('script'):
            start_index = script_tag.text.find("_contexts_s3id.data")
            if (start_index > -1):
                content = script_tag.text[start_index:]
                start_index = content.find("{\"")
                end_index = content.find("\"};")
                json_str = content[start_index:end_index] + "\"}"
                json_dct = js.loads(json_str)

                df = pd.DataFrame(json_dct['PlayByPlays'])
                df_pbyp = df_pbyp.append(df, ignore_index=True)
                #print(df_pbyp.head())

                # for action in json_dct['PlayByPlays']:
                #     df_pbyp = df_pbyp.append(action, ignore_index=True)
                # <script>
                break
        
        # per game
        #break

    df_pbyp.to_csv("play_by_play_20200222.csv", header=True, index=False, sep=",")

if __name__ == "__main__":
    main()