CREATE OR REPLACE FUNCTION script_type_get_id
(
    script_type_name varchar
)
RETURNS integer AS $$
BEGIN
    RETURN
    (
        SELECT script_type_id
        FROM script_types
        WHERE name = script_type_name
    );
END
$$ LANGUAGE plpgsql;

