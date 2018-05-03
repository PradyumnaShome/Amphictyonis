CREATE OR REPLACE FUNCTION task_status
(
    task_id_lookup integer
)
RETURNS varchar AS $$
DECLARE most_recent timestamp;
DECLARE status varchar;
BEGIN
    SELECT MAX(tl.log_time)
    FROM task_log AS tl
    WHERE tl.task_id = task_id_lookup
    INTO most_recent;

    SELECT ws.name
    FROM task_log AS tl
    INNER JOIN work_statuses AS ws on tl.work_status_id = ws.work_status_id
    WHERE tl.task_id = task_id_lookup AND
          tl.log_time = most_recent
    INTO status;

    RETURN
    (
        SELECT COALESCE(status, 'none')
    );
END
$$ LANGUAGE plpgsql;

