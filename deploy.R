# deployment
library(googleCloudRunner)
library(crayon)
cr_project_set(Sys.getenv("GCS_DEFAULT_PROJECT"))
cr_bucket_set(Sys.getenv("GCS_DEFAULT_BUCKET"))
if (interactive()){
  googleAuthR::gar_auth(token = rjson::fromJSON(file = "wh-secret.json"),
                        use_oob = T)
}else{
  googleAuthR::gar_auth(token = rjson::fromJSON("GCS_SECRET"),
                        use_oob = T)}
# cr_setup_auth(file = "wh-secret.json")
# cr_setup()
# Set up your Google Cloud Build
# build <- cr_build_yaml("cloudbuild.yaml")
cr_deploy_docker(local = "precious-metals",image_name = "precious-metals-report",tag = "v0.0.11",timeout = 4500)
# Build the container image
# cr_build(build, timeout = 1800)
#
# Deploy your Shiny app to Google Cloud Run
cr_run(
  # "gcr.io/work-horse-369310/cra-forms-shiny:v0.0.9",
  paste0(
    "gcr.io/",
    Sys.getenv("GCS_DEFAULT_PROJECT"),
    "/precious-metals-report@sha256:115d54d757a5a6457e13fb18d8118553218f3c2e1fdeae60cea8f918a5035f69"
  ),#tag = c("latest","$BUILD_ID"),
  allowUnauthenticated = T,
  concurrency = 80,
  max_instances = 1,port = 3838
)

# Print the URL of your deployed app
cat("Your Shiny app is running at:",
    cr_run_url("precious-metals-report"))
