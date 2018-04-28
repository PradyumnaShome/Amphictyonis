CREATE OR REPLACE FUNCTION any_worker
(
    task_id_lookup integer,
    work_status_name varchar
)
RETURNS boolean AS $$
DECLARE newest_time timestamp;
BEGIN
    SELECT MAX(log_time)
    FROM task_log
    WHERE task_id = task_id_lookup
    INTO newest_time;

    -- TODO: Let this be configurable based on job.
    IF (now() - interval '1 hour') > newest_time THEN
        RETURN FALSE;
    ELSE
        RETURN
        (
            SELECT EXISTS(
                SELECT 1
                FROM task_log AS tl
                INNER JOIN work_statuses AS ws ON tl.work_status_id = ws.work_status_id
                WHERE tl.task_id = task_id_lookup AND
                      ws.name = work_status_name AND
                      log_time = newest_time
            )
        );
    END IF;
END
$$ LANGUAGE plpgsql;

