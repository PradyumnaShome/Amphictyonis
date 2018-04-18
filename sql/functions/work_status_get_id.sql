CREATE OR REPLACE FUNCTION work_status_get_id
(
    work_status_name varchar
)
RETURNS integer AS $$
BEGIN
    RETURN
    (
        SELECT work_status_id
        FROM work_statuses
        WHERE name = work_status_name
    );
END
$$ LANGUAGE plpgsql;

