CREATE OR REPLACE FUNCTION configs_latest_get
(
    job_name varchar
)
RETURNS varchar AS $$
DECLARE newest_version timestamp;
BEGIN
    SELECT MAX(version_date)
    FROM configs AS C
    INNER JOIN jobs AS j ON c.job_id = j.job_id
    WHERE j.name = job_name
    INTO newest_version;

    RETURN
    (
        SELECT config
        FROM configs AS c
        INNER JOIN jobs AS j ON c.job_id = j.job_id
        WHERE j.name = job_name AND
              c.version_date = newest_version
    );
END;
$$ LANGUAGE plpgsql;

