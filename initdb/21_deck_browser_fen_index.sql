CREATE INDEX IF NOT EXISTS user_card_views_deck_fen_idx
  ON user_card_views (user_deck_id, fen)
  WHERE fen IS NOT NULL
    AND fen <> ''
    AND split_part(fen, ' ', 1) <> 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR';
