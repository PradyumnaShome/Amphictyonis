CREATE TABLE jobs (
    job_id serial PRIMARY KEY,
    name varchar NOT NULL
);

CREATE TABLE configs (
    config_id serial PRIMARY KEY,
    job_id integer NOT NULL,
    version varchar NOT NULL,
    version_date timestamp NOT NULL,
    config varchar NOT NULL,
    timeout interval NOT NULL
);

CREATE TABLE script_types (
    script_type_id serial PRIMARY KEY,
    name varchar NOT NULL
);

CREATE TABLE os_kinds (
    os_kind_id serial PRIMARY KEY,
    version varchar NOT NULL,
    primary_name varchar NOT NULL,
    secondary_name varchar
);

CREATE TABLE scripts (
    script_id serial PRIMARY KEY,
    job_id integer NOT NULL,
    os_kind_id integer NOT NULL,
    script_type_id integer NOT NULL,
    script varchar NOT NULL
);

CREATE TABLE tasks (
    task_id serial PRIMARY KEY,
    job_id integer NOT NULL
);

CREATE TABLE task_data (
    task_data_id serial PRIMARY KEY,
    task_id integer NOT NULL,
    key varchar NOT NULL,
    value varchar
);

CREATE TABLE job_files (
    job_file_id serial PRIMARY KEY,
    job_id integer NOT NULL,
    file_path varchar NOT NULL,
    -- base 64 encoded.
    file_content varchar NOT NULL
);

CREATE TABLE result_files (
    result_file_id serial PRIMARY KEY,
    job_id integer NOT NULL,
    task_id integer NOT NULL,
    file_path varchar NOT NULL,
    -- base 64 encoded.
    file_content varchar NOT NULL
);

CREATE TABLE task_log (
    task_log_id serial PRIMARY KEY,
    task_id integer NOT NULL,
    worker_id integer NOT NULL,
    work_status_id integer NOT NULL,
    log_time timestamp NOT NULL
);

CREATE TABLE work_statuses (
    work_status_id serial PRIMARY KEY,
    name varchar NOT NULL,
    available boolean NOT NULL
);

CREATE TABLE workers (
    worker_id serial PRIMARY KEY,
    name varchar NOT NULL,
    job_id integer NOT NULL
);

