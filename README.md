# Amphictyonis

Amphictyonis (or `amphi`) is a system for parallelizing programs such as scripts that were not originally built to be run in parallel.

It interacts with normal Bash scripts and programs with command line interfaces to provide an infrastructure for running these programs in parallel.

The goal of Amphictyonis is to allow a group of normal computers to work together on a single task.

# Quickstart

```
# Install the Haskell build tool/package manager stack. Only necessary if you don't already have stack.
wget -qO- https://get.haskellstack.org/ | sh
stack update

git clone https://github.com/ReedOei/Amphictyonis.git
cd Amphictyonis
stack install
```

After installation is complete, you can run Amphictyonis by running the `amphi` command.
See below for more details.

To run an already existing job, simply run `amphi run <job-name>`.

To submit a new job, see the "Submitting a job" section.

# Job Configuration

All configuration should be placed into a `config.yaml` (can be named whatever you want).
To create a `config.yaml` in your current directory with some default values set, run `amphi new <job-name>`.

The following options are required:

- `name`: The name of your job. Currently, this cannot be changed.
- `worker-dir`: A file-system description of the directory that the worker should run in.
    - Each item in the file system must have a name.
    - Optionally, you can provide a source for the file, which is a url in one of the following formats:
        - `git://url`: This should be a git repository. The repository will be cloned to the relevant place in the worker's path.
        - `server://file/path/to/file`: Note, you **must** have `file` as the first part of your path. This specifies a static file that was upload as part of the job, which Will be downloaded from the server. Note that these files are stored in the database, rather than on the server running the database.
        - Anything else: Assuming to be a file on some other server. Amphictyonis will attempt to download this file into the relevant place in the worker's path.
    - You can also optionally provide a script for each file/directory, which will be run after the whole directory is set up. This is generally used for building/installing a dependency.
    - Any directory in the file system can have an arbitrary number of files/directories inside of it. Files cannot have any other files/directories as children.
- `runner`: The series of shell commands to execute which will generate your results.

Additionally, the following optional arguments are supported:

- `version`: The version of the config file that you are submitting.
- `reuse-paths`: Whether to create an entirely fresh directory structure each time, or just the first time. Set this to `true` if you want to be more sure your programs will run independently.
- `working-path`: Defaults to the top level directory. Should be the `name` field of some directory in your worker's path.
- `result-paths`: A list of directories that will contain the result files when your runner is done executing a task.
- `diff-upload`: Defaults to `true`. If `true`, will upload all files that have been modified in the worker's result paths since the task began execution.
- `data-sources`: A series of shell commands to execute which will generate output in the form of a .csv output to stdout. Each row will be passed to your runner as environment variables.

# Submitting a job

When you are done configuring your job, you can submit it to your local Amphictyonis server by running `amphi submit config.yaml`

If you want to run it on the master Amphictyonis server (hosted at <http://reedoei.com>), you can run `amphi submit config.yaml --host="reedoei.com"` (or modify your `amphi.yaml` to set the host) to submit the job.
The job will **not** be automatically available, as it must be approved first.

# Setting up the server

To set up the server on a system that has the `apt` package manager, you should simply be able to run from the root of this repository:

```
cd scripts
bash setup.sh
```

If you do not have the `apt` package manager, you will have to set up the Postgre-SQL server yourself.
All SQL files are in the `sql/` directory of this repository.

# Command line options

- `host`: The Amphictyonis server host. Defaults to `reedoei.com` (the location of the master Amphictyonis server).
- `port`: The port the Amphictyonis server is running on. Defaults to `3000`.
- `n`: The number of tasks to run simultaneously (defaults to 1).

## Amphictyonis Configuration

All client configuration for the current computer is stored in `~/amphi.yaml` by default.
The configuration is identical to specifying command line options with identical names.
Therefore, if you wish to avoid typing `amphi run <job> -n 100 --host mywebsite.com --port 4000` you can simply write in your `~/amphi.yaml`:

```
host: mywebsite.com
port: 4000
n: 100
```

Additionally, you can specify the default job by writing `job: <job>` in your `amphi.yaml`.

# Terminology

- Worker
- Worker path
- Job
- Task
- Run
- Submit
- Runner

# Other Features

- SQL-izer (not completed)
    - This takes a collection of files in some standard data format (currently formats are .xml, .json, and .csv), and creates SQL scripts that will generate and populate tables with the equivalent data.
    - The goal is to provide an easy way to convert from a less-structured data format to a SQL database which can be easily queried.

- Script Instrumentation (not completed)
    - Modifies the source code of your script so that relevant invocations of your target program are captured and transformed into *tasks* that are compatible with the Amphictyonis system.
    - Takes all environment variables as well, to improve reproducibility.
    - Also tracks all programs invoked, and will attempt to generate a config.yaml that accurate reflects all of these dependencies.

