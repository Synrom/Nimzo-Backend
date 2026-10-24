"""Copy two existing PUBLIC Sicilian decks into the isolated local QA container.
No production writes: /deck/cards is a read query and isDownload is false.
"""
import json, subprocess, urllib.request
BASE = 'https://nimzochess.com/api'
def get(path, data=None):
    req = urllib.request.Request(BASE + path, data=json.dumps(data).encode() if data else None, headers={'Content-Type':'application/json'})
    return json.load(urllib.request.urlopen(req, timeout=40))
def quote(value): return "'" + str(value).replace("'", "''") + "'"
decks = get('/deck/search/full?query=Sicilian%20Defense')
sql = []
for deck in decks:
    if deck['deckId'] not in [68134, 68384]: continue
    cards, cursor = [], None
    while True:
        page = get('/deck/cards', {'deckId':deck['deckId'], 'limit':100, 'cursor':cursor, 'isDownload':False})
        cards.extend(page['cards']); cursor = page['next_cursor']
        if not cursor: break
    author, did = deck['author'], deck['user_deck_id']
    color = deck['color']
    sql.append(f"INSERT INTO users(username,password,salt,email,verified) VALUES({quote(author)},'local-disabled','local-disabled',{quote(str(deck['deckId'])+'@example.invalid')},true) ON CONFLICT DO NOTHING;")
    sql.append(f"INSERT INTO user_deck_views(id,user_id,name,is_public,description,color,num_cards_total) VALUES({quote(did)},{quote(author)},{quote(deck['name'])},true,{quote(deck['description'])},{quote(color)},{len(cards)}) ON CONFLICT DO NOTHING;")
    for i,c in enumerate(cards):
        sql.append(f"INSERT INTO user_card_views(id,user_id,user_deck_id,next_request,moves,title,color) VALUES({quote('public-qa-'+str(deck['deckId'])+'-'+str(i))},{quote(author)},{quote(did)},0,{quote(c['moves'])},{quote(c['title'])},{quote(c['color'])}) ON CONFLICT DO NOTHING;")
    sql.append(f"INSERT INTO decks(id,name,user_deck_id,is_public,description,color,author,num_cards_total,featured_source,featured_rank) VALUES({deck['deckId']},{quote(deck['name'])},{quote(did)},true,{quote(deck['description'])},{quote(color)},{quote(author)},{len(cards)},'tiktok',1) ON CONFLICT DO NOTHING;")
    print('Public deck copied:', deck['deckId'],deck['name'],deck['color'],len(cards))
subprocess.run(['docker','exec','-i','nimzo-onboarding-qa','psql','-v','ON_ERROR_STOP=1','-U','postgres','-d','nimzo'],input='\n'.join(sql).encode(),check=True,stdout=subprocess.DEVNULL)
