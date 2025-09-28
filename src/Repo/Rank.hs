{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Repo.Rank where
import Repo.Classes (MonadDB (runQuery))
import Models.User (UserXP)
import Database.PostgreSQL.Simple.Types (Query(Query))
import Models.Rank (Direction(..), RankQuery(..))



directionToRange :: RankQuery -> (Integer, Integer)
directionToRange query = case query.direction of
  Down  -> (start, start + step)
  Up    -> (max (start - step) 0, start)
  Both  -> (max (start - halfStep) 0, start + halfStep)
  where
    step = query.limit
    halfStep = div query.limit 2
    start = query.rank

listRank :: MonadDB m => RankQuery -> m [UserXP]
listRank rawQuery = runQuery query (start, end)
  where
    query :: Query
    query = "SELECT username, xp, rank FROM users WHERE rank BETWEEN ? AND ? ORDER BY rank"
    rankQuery = rawQuery { limit = min rawQuery.limit 100 } -- TODO: maybe even higher
    (start,end) = directionToRange rankQuery 