import requests as rq

def main():
    res = rq.get("https://www.bleague.jp/schedule/?tab=1&year=2019&event=2")

if __name__ == "__main__":
    main()