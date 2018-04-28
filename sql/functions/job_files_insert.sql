CREATE OR REPLACE FUNCTION job_files_insert
(
    job_name varchar,
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

    INSERT INTO job_files (job_id, file_path, file_content)
    VALUES (job_id, file_path, file_content);
END
$$ LANGUAGE plpgsql;

