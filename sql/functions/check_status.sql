CREATE OR REPLACE FUNCTION check_status
(
    task_id integer,
    work_status_name varchar
)
RETURNS boolean AS $$
BEGIN
    RETURN
    (
        SELECT EXISTS(
            SELECT 1
            FROM tasks AS t
            INNER JOIN work_statuses as ws on tl.work_status_id = ws.work_status_id
            WHERE t.task_id = task_id AND
                  status != 'finished'
        )
    );
END
$$ LANGUAGE plpgsql;

