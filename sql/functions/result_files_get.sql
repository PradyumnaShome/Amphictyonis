CREATE OR REPLACE FUNCTION result_files_get
(
    job_name varchar,
    task_id_lookup integer,
    file_path_lookup varchar
)
RETURNS varchar AS $$
DECLARE job_id_lookup integer;
BEGIN
    SELECT job_id
    FROM jobs
    WHERE name = job_name
    INTO job_id_lookup;

    RETURN
    (
        SELECT file_content
        FROM result_files AS rf
        WHERE rf.job_id = job_id_lookup AND
              rf.file_path = file_path_lookup AND
              rf.task_id = task_id_lookup
    );
END;
$$ LANGUAGE plpgsql;

