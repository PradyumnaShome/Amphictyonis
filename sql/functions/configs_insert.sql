CREATE OR REPLACE FUNCTION configs_insert
(
    job_name varchar,
    version varchar,
    config varchar,
    timeout varchar
)
RETURNS void AS $$
DECLARE job_id_insert integer;
BEGIN
    SELECT job_id
    FROM jobs
    WHERE name = job_name
    INTO job_id_insert;

    INSERT INTO configs
    (
        job_id,
        version,
        version_date,
        config,
        timeout
    )
    VALUES
    (
        job_id_insert,
        version,
        now(),
        config,
        trim(timeout)::interval
    );
END;
$$ LANGUAGE plpgsql;

