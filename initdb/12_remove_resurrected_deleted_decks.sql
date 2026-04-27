BEGIN;

-- Remove deck data that was recreated with an id that already exists in deleted_udvs.
DELETE FROM decks d
USING deleted_udvs du
WHERE d.user_deck_id = du.id;

DELETE FROM user_card_views ucv
USING deleted_udvs du
WHERE ucv.user_deck_id = du.id;

DELETE FROM user_deck_views udv
USING deleted_udvs du
WHERE udv.id = du.id;

COMMIT;
