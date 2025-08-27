CREATE OR REPLACE FUNCTION decks_cards_count_updater()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'INSERT' THEN
    UPDATE decks SET num_cards_total = num_cards_total + 1
    WHERE id = NEW.deck_id;
    RETURN NEW;

  ELSIF TG_OP = 'DELETE' THEN
    UPDATE decks SET num_cards_total = num_cards_total - 1
    WHERE id = OLD.deck_id;
    RETURN OLD;

  ELSIF TG_OP = 'UPDATE' THEN
    IF NEW.deck_id IS DISTINCT FROM OLD.deck_id THEN
      UPDATE decks SET num_cards_total = num_cards_total - 1
      WHERE id = OLD.deck_id;
      UPDATE decks SET num_cards_total = num_cards_total + 1
      WHERE id = NEW.deck_id;
    END IF;
    RETURN NEW;
  END IF;
END;
$$;

CREATE TRIGGER cards_count_ins
AFTER INSERT ON cards
FOR EACH ROW EXECUTE FUNCTION decks_cards_count_updater();

CREATE TRIGGER cards_count_del
AFTER DELETE ON cards
FOR EACH ROW EXECUTE FUNCTION decks_cards_count_updater();

CREATE TRIGGER cards_count_upd
AFTER UPDATE OF deck_id ON cards
FOR EACH ROW EXECUTE FUNCTION decks_cards_count_updater();

