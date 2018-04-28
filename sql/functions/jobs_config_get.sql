CREATE OR REPLACE FUNCTION jobs_config_get
(
    job_name varchar
)
RETURNS varchar AS $$
BEGIN
    RETURN
    (
        SELECT config
        FROM jobs
        WHERE name = job_name
    );
END;
$$ LANGUAGE plpgsql;

