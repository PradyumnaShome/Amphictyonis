CREATE OR REPLACE FUNCTION tasks_get_next
(
    job_name varchar,
    worker_id integer
)
RETURNS integer AS $$
DECLARE chosen_task_id integer;
BEGIN
    SELECT t.task_id
    FROM jobs AS j
    INNER JOIN tasks AS t ON j.job_id = t.job_id
    WHERE j.name = job_name AND tasks_available(t.task_id)
    LIMIT 1
    INTO chosen_task_id;

    PERFORM task_log_insert(chosen_task_id, worker_id, 'requested');

    RETURN chosen_task_id;
END
$$ LANGUAGE plpgsql;

