CREATE OR REPLACE FUNCTION task_data_get
(
    task_id_lookup integer
)
RETURNS TABLE(key varchar, value varchar) AS $$
BEGIN
    RETURN QUERY
    SELECT td.key, td.value
    FROM task_data as td
    WHERE td.task_id = task_id_lookup;
END
$$ LANGUAGE plpgsql;

