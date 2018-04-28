CREATE OR REPLACE FUNCTION jobs_insert
(
    job_name varchar,
    config varchar
)
RETURNS void AS $$
BEGIN
    INSERT INTO jobs (name, config)
    VALUES (job_name, config);
END;
$$ LANGUAGE plpgsql;

