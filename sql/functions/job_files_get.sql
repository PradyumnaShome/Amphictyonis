CREATE OR REPLACE FUNCTION job_files_get
(
    job_name varchar,
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
        FROM job_files AS jf
        WHERE jf.job_id = job_id_lookup AND jf.file_path = file_path_lookup
    );
END;
$$ LANGUAGE plpgsql;

