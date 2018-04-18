CREATE OR REPLACE FUNCTION tasks_get_next
(
    job_name varchar
)
RETURNS integer AS $$
BEGIN
    RETURN
    (
        SELECT t.task_id
        FROM jobs AS j
        INNER JOIN tasks AS t ON j.job_id = t.job_id
        INNER JOIN task_log as tl on tl.task_id = t.task_id
        WHERE j.name = job_name AND
              NOT check_status(t.task_id, 'finished')
        LIMIT 1
    );
END
$$ LANGUAGE plpgsql;

