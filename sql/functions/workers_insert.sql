CREATE OR REPLACE FUNCTION workers_insert
(
    worker_name varchar,
    job_name varchar
)
RETURNS integer AS $$
DECLARE new_worker_id integer;
BEGIN
    -- Don't insert twice.
    IF EXISTS(SELECT 1
              FROM workers
              WHERE name = worker_name AND job_id = jobs_get_id(job_name)) THEN
        SELECT worker_id
        FROM workers
        WHERE name = worker_name AND job_id = jobs_get_id(job_name)
        INTO new_worker_id;
    ELSE
        INSERT INTO workers (name, job_id)
        VALUES (worker_name, jobs_get_id(job_name))
        RETURNING worker_id INTO new_worker_id;
    END IF;

    RETURN new_worker_id;
END
$$ LANGUAGE plpgsql;

