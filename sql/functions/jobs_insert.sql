CREATE OR REPLACE FUNCTION jobs_insert
(
    job_name varchar
)
RETURNS void AS $$
BEGIN
    IF NOT EXISTS(SELECT 1 FROM jobs WHERE name = job_name) THEN
        INSERT INTO jobs (name)
        VALUES (job_name);
    END IF;
END;
$$ LANGUAGE plpgsql;

