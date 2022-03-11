import json
import os
import shutil
import docker
import sys
import subprocess
import judge
from time import sleep

technology = sys.argv[1]
delivery = sys.argv[2]
clone_url = sys.argv[3]
repo_name = sys.argv[4]
commit_id = sys.argv[5]

client = docker.from_env()

uploads_path = "/home/amirhosein/uploads"
delivery_path = f"{uploads_path}/{delivery}"
repo_path = f"{uploads_path}/{delivery}/{repo_name}"


def clear():
    # clear containers
    for container in client.containers.list():
        if container.status == "running":
            container.stop()
        container.remove(force=True)

    # clear images except "python:3.9.6-alpine" and "postgres:13.0-alpine"
    for image in client.images.list():
        if len(image.tags) == 0:
            client.images.remove(image.short_id, force=True)
        elif image.tags[0] not in ["python:3.9.6-alpine", "postgres:13.0-alpine"]:
            client.images.remove(image.short_id, force=True)


def clone():
    os.mkdir(delivery_path)
    result = subprocess.run(
        f"git clone {clone_url} && cd {repo_name} && git checkout {commit_id}",
        capture_output=False,
        cwd=delivery_path,
        shell=True,
        stdout=sys.stdout,
        stderr=sys.stderr
    )
    if result.returncode != 0:
        with open("result.json", "w") as f:
            f.write(json.dumps({"score" : total}))
        print(result.stderr)
        sys.exit(1)


def prepare():
    if technology == "django":
        shutil.copy("./django/docker-compose.yml", delivery_path)
        shutil.copy("./django/env.db", delivery_path)
        shutil.copy("./django/env.dev", delivery_path)
        shutil.copy("./django/Dockerfile", repo_path)
        shutil.copy("./django/base-requirements.txt", repo_path)
        shutil.copy("./django/entrypoint.sh", repo_path)
    elif technology == "dotnet":
        with open("result.json", "w") as f:
            f.write(json.dumps({"score" : total}))
        sys.exit(1)


def before():
    result = subprocess.run(
        "/usr/local/bin/docker-compose up -d",
        capture_output=False,
        cwd=delivery_path,
        shell=True,
        stdout=sys.stdout,
        stderr=sys.stderr,
        env={"REPONAME": repo_name}
    )
    if result.returncode != 0:
        with open("result.json", "w") as f:
            f.write(json.dumps({"score" : total}))
        print(result.stderr)
        sys.exit(1)


def after():
    result = subprocess.run(
        "/usr/local/bin/docker-compose down",
        capture_output=False,
        cwd=delivery_path,
        shell=True,
        stdout=sys.stdout,
        stderr=sys.stderr,
        env={"REPONAME": repo_name}
    )
    if result.returncode != 0:
        with open("result.json", "w") as f:
            f.write(json.dumps({"score" : total}))
        sys.exit(1)


def do_judge():
    j = judge.Judge("http://localhost:8000")
    clear()
    clone()
    prepare()
    total = 0
    for i, (test, score) in enumerate(judge.tests, 1):
        test_name = ' '.join(test.split('_')).title()
        try:
            before()
            subprocess.run("while ! httping -qc1 http://localhost:8000 ; do sleep 1 ; done", shell=True, timeout=10)
            getattr(j, test)()
            total = total + score
            print(f'{i}. {test_name}: PASSED')
        except Exception:
            print(f'{i}. {test_name}: FAILED')
        finally:
            after()
    print(f'\nTotal score: {total}/{sum(score for test, score in judge.tests)}')
    with open("result.json", "w") as f:
        f.write(json.dumps({"score" : total}))
    clear()

do_judge()
