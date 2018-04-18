CREATE OR REPLACE FUNCTION jobs_insert
(
    job_name varchar
)
RETURNS void AS $$
BEGIN
    INSERT INTO jobs (name)
    VALUES (job_name);
END;
$$ LANGUAGE plpgsql;

