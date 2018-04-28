CREATE OR REPLACE FUNCTION tasks_insert
(
    job_name varchar,
    task_data_keys varchar[],
    task_data_values varchar[]
)
RETURNS void AS $$
DECLARE job_id_lookup integer;
DECLARE new_task_id integer;
BEGIN
    SELECT job_id
    FROM jobs
    WHERE name = job_name
    INTO job_id_lookup;

    INSERT INTO tasks (job_id)
    VALUES (job_id_lookup)
    RETURNING task_id INTO new_task_id;

    FOR i IN array_lower(task_data_keys, 1)..array_upper(task_data_keys, 1)
    LOOP
        INSERT INTO task_data (task_id, key, value)
        VALUES (new_task_id, task_data_keys[i], task_data_values[i]);
    END LOOP;
END;
$$ LANGUAGE plpgsql;

