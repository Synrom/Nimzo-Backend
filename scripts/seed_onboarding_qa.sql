-- LOCAL QA ONLY. This is a fictional public fixture, never an initdb migration.
INSERT INTO users(username,password,salt,email,verified)
VALUES('conversion-qa-author','not-a-login-password','local-qa-salt','conversion-qa@example.invalid',true)
ON CONFLICT(username) DO NOTHING;
INSERT INTO user_deck_views(id,user_id,name,is_public,description,color,num_cards_total)
VALUES('conversion-qa-opening','conversion-qa-author','Conversion QA Opening',true,'Local fixture: learn the first opening moves.','wh',1)
ON CONFLICT(id) DO NOTHING;
INSERT INTO user_card_views(id,user_id,user_deck_id,next_request,moves,title,color)
VALUES('conversion-qa-line','conversion-qa-author','conversion-qa-opening',0,'e4 e5 Nf3','First opening line','w')
ON CONFLICT(id) DO NOTHING;
INSERT INTO decks(name,user_deck_id,is_public,description,color,author,num_cards_total,featured_source,featured_card_id,featured_rank)
VALUES('Conversion QA Opening','conversion-qa-opening',true,'Local fixture: learn the first opening moves.','wh','conversion-qa-author',1,'instagram','conversion-qa-line',1)
ON CONFLICT(user_deck_id) DO NOTHING;
