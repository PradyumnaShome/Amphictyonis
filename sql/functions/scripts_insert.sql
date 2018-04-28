CREATE OR REPLACE FUNCTION scripts_insert
(
    job_name varchar,
    os_name varchar,
    script_type_name varchar,
    script varchar
)
RETURNS void AS $$
BEGIN
    INSERT INTO scripts (job_id, os_kind_id, script_type_id, script)
    VALUES (jobs_get_id(job_name), os_kinds_get_id(os_name), script_type_get_id(script_type_name), script);
END;
$$ LANGUAGE plpgsql;

