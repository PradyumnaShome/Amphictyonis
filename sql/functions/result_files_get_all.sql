CREATE OR REPLACE FUNCTION result_files_get_all
(
    job_name varchar
)
RETURNS varchar[] AS $$
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
        WHERE rf.job_id = job_id_lookup
    );
END;
$$ LANGUAGE plpgsql;

