CREATE OR REPLACE FUNCTION jobs_get_id
(
    job_name varchar
)
RETURNS integer AS $$
BEGIN
    RETURN
    (
        SELECT job_id
        FROM jobs
        WHERE name = job_name
    );
END
$$ LANGUAGE plpgsql;

