CREATE OR REPLACE FUNCTION configs_get
(
    job_name varchar
)
RETURNS varchar AS $$
BEGIN
    RETURN
    (
        SELECT config
        FROM configs AS C
        INNER JOIN jobs AS j ON c.job_id = j.job_id
        WHERE j.name = job_name
    );
END;
$$ LANGUAGE plpgsql;

