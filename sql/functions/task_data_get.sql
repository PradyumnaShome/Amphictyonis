CREATE OR REPLACE FUNCTION task_data_get
(
    task_id integer
)
RETURNS TABLE(task_data_id integer, key varchar, value varchar) AS $$
BEGIN
    RETURN QUERY
    SELECT task_data_id, key, value
    FROM task_data as td
    WHERE td.task_id = task_id;
END
$$ LANGUAGE plpgsql;

