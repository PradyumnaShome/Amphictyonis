CREATE OR REPLACE FUNCTION tasks_available
(
    task_id_lookup integer
)
RETURNS boolean AS $$
DECLARE status varchar;
DECLARE result boolean;
BEGIN
    SELECT task_status(task_id_lookup)
    INTO status;

    SELECT available
    FROM work_statuses
    WHERE name = status
    INTO result;

    RETURN result;
END
$$ LANGUAGE plpgsql;

