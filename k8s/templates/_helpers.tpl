{{/*
Expand the name of the chart.
*/}}
{{- define "the0.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
If release name contains chart name it will be used as a full name.
*/}}
{{- define "the0.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "the0.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "the0.labels" -}}
helm.sh/chart: {{ include "the0.chart" . }}
{{ include "the0.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "the0.selectorLabels" -}}
app.kubernetes.io/name: {{ include "the0.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Create the name of the service account to use
*/}}
{{- define "the0.serviceAccountName" -}}
{{- if .Values.serviceAccount.create }}
{{- default (include "the0.fullname" .) .Values.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.serviceAccount.name }}
{{- end }}
{{- end }}

{{/*
=== Infrastructure Connection Helpers ===
Resolve internal vs external connection strings for each infrastructure service.
When <service>.enabled is true, use internal cluster addresses.
When false, use external.connectionString (if set) or build from external.* fields.
*/}}

{{/*
PostgreSQL DATABASE_URL
*/}}
{{- define "the0.databaseUrl" -}}
{{- if .Values.postgresql.enabled -}}
postgresql://{{ .Values.postgresql.username }}:{{ .Values.postgresql.password }}@{{ include "the0.fullname" . }}-postgres:{{ .Values.postgresql.port }}/{{ .Values.postgresql.database }}?sslmode={{ .Values.postgresql.sslmode }}
{{- else if .Values.postgresql.external.connectionString -}}
{{ .Values.postgresql.external.connectionString }}
{{- else -}}
postgresql://{{ .Values.postgresql.external.username }}:{{ .Values.postgresql.external.password }}@{{ .Values.postgresql.external.host }}:{{ .Values.postgresql.external.port }}/{{ .Values.postgresql.external.database }}?sslmode={{ .Values.postgresql.external.sslmode }}
{{- end -}}
{{- end }}

{{/*
MongoDB connection URL
*/}}
{{- define "the0.mongoUrl" -}}
{{- if .Values.mongodb.enabled -}}
mongodb://{{ .Values.mongodb.rootUsername }}:{{ .Values.mongodb.rootPassword }}@{{ include "the0.fullname" . }}-mongo:{{ .Values.mongodb.port }}
{{- else if .Values.mongodb.external.connectionString -}}
{{ .Values.mongodb.external.connectionString }}
{{- else -}}
mongodb://{{ .Values.mongodb.external.username }}:{{ .Values.mongodb.external.password }}@{{ .Values.mongodb.external.host }}:{{ .Values.mongodb.external.port }}/{{ .Values.mongodb.external.database }}?authSource={{ .Values.mongodb.external.authSource }}{{ with .Values.mongodb.external.options }}&{{ . }}{{ end }}
{{- end -}}
{{- end }}

{{/*
NATS connection URL
*/}}
{{- define "the0.natsUrl" -}}
{{- if .Values.nats.enabled -}}
nats://{{ include "the0.fullname" . }}-nats:{{ .Values.nats.port }}
{{- else if .Values.nats.external.url -}}
{{ .Values.nats.external.url }}
{{- else -}}
nats://{{ .Values.nats.external.host }}:{{ .Values.nats.external.port }}
{{- end -}}
{{- end }}

{{/*
MinIO endpoint (host only, no port) — used by the API service
*/}}
{{- define "the0.minioEndpoint" -}}
{{- if .Values.minio.enabled -}}
{{ include "the0.fullname" . }}-minio
{{- else -}}
{{ .Values.minio.external.endpoint }}
{{- end -}}
{{- end }}

{{/*
MinIO endpoint with port (host:port) — used by the bot-controller
*/}}
{{- define "the0.minioEndpointWithPort" -}}
{{- if .Values.minio.enabled -}}
{{ include "the0.fullname" . }}-minio:{{ .Values.minio.port }}
{{- else -}}
{{ .Values.minio.external.endpoint }}:{{ .Values.minio.external.port }}
{{- end -}}
{{- end }}

{{/*
MinIO port
*/}}
{{- define "the0.minioPort" -}}
{{- if .Values.minio.enabled -}}
{{ .Values.minio.port }}
{{- else -}}
{{ .Values.minio.external.port }}
{{- end -}}
{{- end }}

{{/*
MinIO use SSL
*/}}
{{- define "the0.minioUseSSL" -}}
{{- if .Values.minio.enabled -}}
false
{{- else -}}
{{ .Values.minio.external.useSSL }}
{{- end -}}
{{- end }}

{{/*
MinIO access key
*/}}
{{- define "the0.minioAccessKey" -}}
{{- if .Values.minio.enabled -}}
{{ .Values.minio.accessKey }}
{{- else -}}
{{ .Values.minio.external.accessKey }}
{{- end -}}
{{- end }}

{{/*
MinIO secret key
*/}}
{{- define "the0.minioSecretKey" -}}
{{- if .Values.minio.enabled -}}
{{ .Values.minio.secretKey }}
{{- else -}}
{{ .Values.minio.external.secretKey }}
{{- end -}}
{{- end }}