# Cloud Run v2 Job Deployment

This setup targets **Cloud Run v2 Jobs** (batch), which is the correct runtime
model for NASTRAN-style finite element runs.

## Files

- `Dockerfile.cloudrun`: container build file
- `deploy/cloudrun/entrypoint.sh`: container entrypoint
- `deploy/cloudrun/job_runner.py`: job orchestration (input, execution, output)
- `deploy/cloudrun/job.yaml`: template for `gcloud run jobs replace`
- `deploy/cloudrun/job.env.example`: environment variables example

## Important Runtime Note

The legacy binaries included under `bin/` are SPARC binaries and are not
directly runnable on Cloud Run amd64. Provide a native Linux executable and set:

```bash
NASTRAN_CMD_TEMPLATE="/path/to/native/nastran --batch {problem}"
```

## 1) Build and push image

```bash
PROJECT_ID="your-project"
REGION="us-central1"
REPO="containers"
IMAGE="nastran95-cloudrun"

gcloud builds submit \
  --project "${PROJECT_ID}" \
  --region "${REGION}" \
  --tag "${REGION}-docker.pkg.dev/${PROJECT_ID}/${REPO}/${IMAGE}:latest" \
  -f Dockerfile.cloudrun .
```

## 2) Create or update Cloud Run Job (v2)

Option A: CLI deploy

```bash
JOB_NAME="nastran95-job"
IMAGE_URI="${REGION}-docker.pkg.dev/${PROJECT_ID}/${REPO}/${IMAGE}:latest"

gcloud run jobs deploy "${JOB_NAME}" \
  --project "${PROJECT_ID}" \
  --region "${REGION}" \
  --image "${IMAGE_URI}" \
  --tasks 1 \
  --max-retries 1 \
  --task-timeout 3600s \
  --memory 2Gi \
  --cpu 2 \
  --set-env-vars-file deploy/cloudrun/job.env.example
```

Option B: YAML replace

```bash
gcloud run jobs replace deploy/cloudrun/job.yaml \
  --project "${PROJECT_ID}" \
  --region "${REGION}"
```

## 3) Execute job

```bash
JOB_NAME="nastran95-job"
gcloud run jobs execute "${JOB_NAME}" \
  --project "${PROJECT_ID}" \
  --region "${REGION}" \
  --wait
```

## Environment Variables

Required:

- `PROBLEM_NAME`: e.g. `d01000a`

Input (one of):

- `INPUT_GCS_URI`: `gs://bucket/path/problem.inp`
- `INPUT_FILE`: local path inside container
- fallback: `/app/examples/input/{PROBLEM_NAME}.inp`

Output (optional):

- `OUTPUT_GCS_URI`: `gs://bucket/path/prefix`

Execution:

- `NASTRAN_CMD_TEMPLATE`: command template with `{problem}` placeholder
- `RUN_ROOT`: run directory root (default `/workspace`)
- `MANIFEST_PATH`: optional JSON manifest output path

## IAM

If using GCS input/output, the Cloud Run Job service account needs:

- `roles/storage.objectViewer` for input bucket
- `roles/storage.objectCreator` (or `objectAdmin`) for output bucket
