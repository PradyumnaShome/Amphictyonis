CREATE OR REPLACE FUNCTION jobs_list
(
)
RETURNS TABLE(job_name varchar, tasks_left integer) AS $$
BEGIN
    RETURN QUERY
    SELECT j.name, COUNT(t.task_id)
    FROM jobs AS j
    LEFT JOIN tasks AS t on j.job_id = t.job_id AND tasks_available(t.task_id)
    GROUP BY j.name;
END
$$ LANGUAGE plpgsql;

