# Only install if necessary
if [[ $(dpkg-query -W -f='${Status}' "postgresql" 2>/dev/null | grep -c "ok installed") -eq 0 ]]; then
    echo "[INFO] Installing postgresql."
    sudo aptitude update
    sudo aptitude install postgresql postgresql-contrib "postgresql-server-dev-9.5"
fi

# Get rid of the user and db if they already exist.
if [[ `sudo -u postgres psql postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='amphictyonis'"` -eq "1" ]]; then
    echo "[INFO] Dropping old db and user"
    sudo -u postgres dropdb amphictyonis
    sudo -u postgres dropuser amphictyonis
    sudo userdel amphictyonis
fi

echo "[INFO] Creating new db and user 'amphictyonis'"
sudo -u postgres createuser amphictyonis --superuser
sudo -u postgres createdb amphictyonis
sudo adduser amphictyonis < create-db-user

echo "[INFO] Initializing database."
sudo -u amphictyonis psql < ../sql/setup-user.sql
sudo -u amphictyonis psql < ../sql/create-tables.sql
sudo -u amphictyonis psql < ../sql/init-data.sql
for func_file in `find "../sql/functions/" -type f -name "*.sql"`
do
    echo "[INFO] Creating function from: $func_file"
    sudo -u amphictyonis psql < "$func_file"
done

