#! /bin/bash

dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-00.json
echo --------------------------------------------------------------------------
dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-00.json --args workflows/workflow-sample-00.json
echo --------------------------------------------------------------------------
dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-01.json
echo --------------------------------------------------------------------------
dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-01.json --args workflows/workflow-sample-00.json
echo --------------------------------------------------------------------------
dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-02.json
echo --------------------------------------------------------------------------
dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-02.json --args workflows/tag-hello.json
echo --------------------------------------------------------------------------
dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-03.json --state workflows/tag-hello.json
echo --------------------------------------------------------------------------
dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-04.json --args workflows/tag-hello.json --state workflows/workflow-sample-00.json
echo --------------------------------------------------------------------------
dist/build/reesd-intake/reesd-intake run --file workflows/workflow-sample-05.json
