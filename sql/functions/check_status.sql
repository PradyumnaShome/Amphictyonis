CREATE OR REPLACE FUNCTION check_status
(
    task_id_lookup integer,
    work_status_name varchar
)
RETURNS boolean AS $$
BEGIN
    RETURN
    (
        SELECT EXISTS(
            SELECT 1
            FROM tasks AS t
            INNER JOIN task_log AS tl ON t.task_id = tl.task_id
            INNER JOIN work_statuses AS ws ON tl.work_status_id = ws.work_status_id
            WHERE t.task_id = task_id_lookup AND
                  ws.name = work_status_name
        )
    );
END
$$ LANGUAGE plpgsql;

