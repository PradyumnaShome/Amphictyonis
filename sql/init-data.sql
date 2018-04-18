INSERT INTO script_types (name)
VALUES ('setup'),
       ('runner'),
       ('cleanup');

INSERT INTO os_kinds (version, primary_name, secondary_name)
VALUES ('any', 'Linux', 'Ubuntu'),
       ('any', 'Linux', 'Other'),
       ('any', 'MacOS', NULL),
       ('any', 'Windows', NULL);

INSERT INTO work_statuses (name)
VALUES ('running'),
       ('failed'),
       ('finished'),
       ('aborted');

