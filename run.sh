#! /bin/bash

dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-00.json
echo
dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-00.json --args workflows/workflow-sample-00.json
echo
dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-01.json
