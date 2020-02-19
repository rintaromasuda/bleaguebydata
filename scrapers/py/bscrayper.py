import requests as rq
from bs4 import BeautifulSoup

def main():
    res = rq.get("https://www.bleague.jp/schedule/?tab=1&year=2019&event=2&club=&setuFrom=1&setuTo=36")
    soup = BeautifulSoup(res.content, 'html.parser')

    for div in soup.find_all('div', {'class': "state_link btn report"}):
        detail_link = div.a['href']
        res = rq.get(detail_link)
        soup = BeautifulSoup(res.content, 'html.parser')
        print(soup.title)
        print("\n")
        break

if __name__ == "__main__":
    main()