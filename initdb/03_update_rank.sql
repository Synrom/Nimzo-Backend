CREATE OR REPLACE FUNCTION update_user_ranks()
RETURNS TRIGGER AS $$
BEGIN
    -- Recompute ranks for all users
    UPDATE users
    SET rank = sub.new_rank
    FROM (
        SELECT 
            username,
            RANK() OVER (ORDER BY xp DESC, username ASC) AS new_rank
        FROM users
    ) AS sub
    WHERE users.username = sub.username
    AND users.rank <> sub.new_rank;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_update_user_rank
AFTER INSERT OR UPDATE OF xp ON users
FOR EACH ROW
EXECUTE FUNCTION update_user_ranks();