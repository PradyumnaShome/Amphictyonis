CREATE OR REPLACE FUNCTION tasks_get_next
(
    job_name varchar
)
RETURNS TABLE(task_id integer) AS $$
BEGIN
    RETURN QUERY
    SELECT t.task_id
    FROM jobs AS j
    INNER JOIN tasks AS t ON j.job_id = t.job_id
    WHERE j.name = job_name AND
          NOT check_status(t.task_id, 'finished') AND
          NOT any_worker(t.task_id, 'running')
    LIMIT 1;
END
$$ LANGUAGE plpgsql;

