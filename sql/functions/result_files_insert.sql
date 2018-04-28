CREATE OR REPLACE FUNCTION result_files_insert
(
    job_name varchar,
    task_id integer,
    file_path varchar,
    file_content varchar
)
RETURNS void AS $$
DECLARE job_id_lookup integer;
BEGIN
    SELECT job_id
    FROM jobs
    WHERE name = job_name
    INTO job_id_lookup;

    INSERT INTO result_files (job_id, task_id, file_path, file_content)
    VALUES (job_id_lookup, task_id, file_path, file_content);
END
$$ LANGUAGE plpgsql;

