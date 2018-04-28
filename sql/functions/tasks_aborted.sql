CREATE OR REPLACE FUNCTION tasks_aborted
(
    worker_id_lookup integer
)
RETURNS void AS $$
DECLARE task_id_lookup integer;
DECLARE most_recent timestamp;
BEGIN
    SELECT tl.log_time
    FROM task_log AS tl
    INNER JOIN workers AS w on tl.worker_id = w.worker_id
    WHERE w.worker_id = worker_id_lookup
    INTO most_recent;

    SELECT t.task_id
    FROM tasks AS t
    INNER JOIN task_log AS tl on t.task_id = tl.task_id
    INNER JOIN workers AS w on tl.worker_id = w.worker_id
    WHERE w.worker_id = worker_id_lookup AND
          tl.log_time = most_recent
    INTO task_id_lookup;

    perform task_log_insert(task_id_lookup, worker_id_lookup, 'aborted');
END
$$ LANGUAGE plpgsql;

