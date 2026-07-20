CREATE INDEX IF NOT EXISTS user_card_views_deck_fen_idx
  ON user_card_views (user_deck_id, fen)
  WHERE fen IS NOT NULL
    AND fen <> ''
    AND fen <> 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -'
    AND fen NOT LIKE 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - %';
