CREATE OR REPLACE FUNCTION task_log_insert
(
    task_id integer,
    worker_id integer,
    work_status_name varchar
)
RETURNS void AS $$
BEGIN
    INSERT INTO task_log
    (
        task_id,
        worker_id,
        work_status_id,
        log_time
    )
    VALUES
    (
        task_id,
        worker_id,
        work_status_get_id(work_status_name),
        now()
    );
END
$$ LANGUAGE plpgsql;

