CREATE OR REPLACE FUNCTION workers_insert
(
    worker_name varchar,
    job_name varchar
)
RETURNS integer AS $$
DECLARE new_worker_id integer;
BEGIN
    INSERT INTO workers (name, job_id)
    VALUES (worker_name, jobs_get_id(job_name))
    RETURNING worker_id INTO new_worker_id;

    RETURN new_worker_id;
END
$$ LANGUAGE plpgsql;

